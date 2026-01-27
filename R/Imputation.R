#' @import data.table
#' @import depmixS4
#' @importFrom hms as_hms
#' @importFrom stats lm predict quantile
#' @importFrom utils capture.output
NULL

#' Create a naive prior probability matrix
#'
#' @description Generates a matrix of equal probabilities for all candidate animals
#'   for each egg. Each row corresponds to an egg, each column to a candidate animal.
#'
#' @param eggid Vector of egg IDs
#' @param cand_ani Vector of candidate animal IDs
#'
#' @return Matrix of size `[length(eggid) x length(cand_ani)]` with equal probabilities
#' @export
#'
make_naive_prior <- function(eggid, cand_ani) {
  prior <- matrix(1 / length(cand_ani), 
                  nrow = length(eggid), 
                  ncol = length(cand_ani), 
                  dimnames = list(eggid, cand_ani))
  prior
}


#' Impute missing laying dates for a given animal
#'
#' @description Imputes likely laying dates for a single animal based on its
#'   observed eggs in `pen_meta`, using Hidden Markov Model fitting.
#'
#' @param pen_meta data.table with at least the columns: `ani`, `Date`, `Eggsignal`,
#'   `F_combined`, `datelay`, `laydiffh_pre_all`, `laydiffh_nxt_all`, `Layingtime`
#' @param aniid Character or numeric ID of the animal to process
#' @param max_gap_hour Maximum allowed gap in hours before considering a "break" (default 200)
#' @param nstate Number of states for HMM model (default 4)
#' @param max_iter Maximum iterations for HMM fitting (default 10)
#' @param stop_pegg Stopping criteria for fitting HMM when the prob of laying reached a certain percentage (default 8)
#' @param min_pegg Minimum prob of laying (percentage) for a state to be considered as laying (default 1.5)
#' @param thrd_laydiffh Minimum hours threshold for laydiff to consider imputation (default 24)
#' @param plot TRUE or FALSE to plot the output (default FALSE)
#'
#' @return data.table of imputed laying dates and time (`Date`, `datelay`, `Layingtime`) and `ani` ID,
#' `datelay` is POSIXct datetime, combined by imputed laying date and mean laying time
#'   including `imp` as information and `p_perc` as the prob of laying (percentage). Returns `NULL` if insufficient data or fit fails.
#' @export
#'
get_imputed_laydates <- function(pen_meta, aniid,
                                 max_gap_hour = 200, 
                                 nstate = 4, max_iter = 10, stop_pegg = 8,
                                 min_pegg = 1.5, thrd_laydiffh = 24,
                                 plot = FALSE) {
  
  if (!is.data.table(pen_meta)) {
    stop("'pen_meta' must be a data.table.")
  }
  
  if (length(aniid) != 1) {
    stop("'aniid' must be a single value (animal ID).")
  }
  
  check_required_cols(pen_meta, 
                      c("ani", "Date", "Eggsignal", "F_combined", "datelay", 
                        "laydiffh_pre_all", "laydiffh_nxt_all", "Layingtime"))
  
  if (!is.numeric(max_gap_hour) || max_gap_hour <= 0) {
    stop("'max_gap_hour' must be a positive numeric value.")
  }
  
  sub <- pen_meta[ani == aniid & Eggsignal > 0 & F_combined != TRUE, 
                  .(ani, Date, Eggsignal, datelay, laydiffh_pre_all, laydiffh_nxt_all, Layingtime)]
  
  # Check if F3_ani_cbd_dup exists
  if ("F3_ani_cbd_dup" %in% names(pen_meta)) {
    tmpp <- pen_meta[ani == aniid & Eggsignal > 0 & F3_ani_cbd_dup == TRUE &
                       F1_noAni != TRUE & F2_latelay != TRUE, 
                     .SD[order(Layingtime)][1],
                     by = Date,
                     .SDcols = c("ani", "Eggsignal", "datelay", "laydiffh_pre_all", 
                                 "laydiffh_nxt_all", "Layingtime")]
    tmpp[, imp := "dups"]
    sub <- rbindlist(list(sub, tmpp), fill = TRUE)
  }
  
  data.table::setorder(sub, Date)
  
  if (nrow(sub) <= 5) {
    message("Skip: animal ", aniid, ": Not enough egg record (<=5) for analysis")
    return(NULL)
  }
  
  # First remove lonely single point
  idx_rm <- sub[(laydiffh_nxt_all > max_gap_hour & laydiffh_pre_all > max_gap_hour) |
                  (laydiffh_nxt_all > max_gap_hour & is.na(laydiffh_pre_all)) |
                  (laydiffh_pre_all > max_gap_hour & is.na(laydiffh_nxt_all)),
                which = TRUE]
  
  if (length(idx_rm) > 0) {
    sub <- sub[!idx_rm]
  }
  
  # Group laying dates if big gap in between
  x <- 0
  idx1 <- sub[laydiffh_nxt_all > max_gap_hour, which = TRUE]
  tmp <- data.table::copy(sub)
  idx_shift <- sort(c(
    idx1 + x,
    tmp[idx1, laydiffh_pre_all := 0][
      laydiffh_pre_all > max_gap_hour, which = TRUE] - (x + 1)
  ))
  idx_shift <- idx_shift[!idx_shift == 0]
  idx_shift <- sort(unique(idx_shift))
  
  brks <- unique(c(0, idx_shift, nrow(sub)))
  ngrp <- length(brks) - 1
  labs <- seq_len(ngrp)
  sub[, grp := cut(.I, breaks = brks, labels = labs)]
  
  implist <- list()
  dtlist <- list()
  
  for (g in unique(sub$grp)) {
    subb <- sub[grp == g]
    
    seq_dates <- seq(min(subb$Date), max(subb$Date), by = "days")
    seq_hours <- CJ(Date = seq_dates, Hour = 1:24)
    seq_hours[, day := .GRP, by = Date]
    
    # Control: need at least x dates
    if (length(seq_dates) <= 5) {
      message("Skip animal ", aniid, ", grp ", g, ": Not enough dates (<=5).")
      next
    }
    
    # Control: at least 5% records
    if (nrow(subb) / length(seq_dates) < 0.05) {
      message("Skip: animal ", aniid, ": Less than 5% records available.")
      next
    }
    
    # Print progress
    print(sprintf("Imputing Laying dates for animal %s, From %s To %s, across %s weeks",
                  aniid, seq_dates[1], seq_dates[length(seq_dates)], 
                  round(difftime(seq_dates[length(seq_dates)], seq_dates[1], units = "weeks"), 1)))
    
    # Extend subb into hours, get laydiff hours pre/next lay
    subb[, layh := hour(hms::as_hms(Layingtime))]
    subb <- subb[seq_hours, on = .(Date = Date, layh = Hour), nomatch = NA][order(Date)]
    subb[, seqtime := (day - 1) * 24 + layh]
    subb[is.na(Eggsignal), Eggsignal := 0]
    
    first_egg <- which(subb$Eggsignal == 1)[1]
    if (!is.na(first_egg)) {
      subb <- subb[first_egg:.N]
    }
    
    # Calculate laydiffh_pre
    subb[, laydiffh_pre := {
      lays <- ifelse(Eggsignal > 0, seqtime, NA_real_)
      prevlay <- data.table::nafill(lays, type = "locf")
      seqtime - data.table::shift(prevlay, type = "lag")
    }]
    
    subb[is.na(laydiffh_pre), laydiffh_pre := 
           median(subb[Eggsignal > 0, laydiffh_pre], na.rm = TRUE)]
    
    # Calculate laydiffh_nxt
    subb[, laydiffh_nxt := {
      lays <- ifelse(Eggsignal > 0, seqtime, NA_real_)
      nextlay <- data.table::nafill(lays, type = "nocb")
      data.table::shift(nextlay, type = "lead") - seqtime
    }]
    
    idx <- subb[is.na(laydiffh_nxt), which = TRUE]
    if (length(idx) > 0) {
      tmph <- subb[idx[1], laydiffh_pre]
      if (!is.na(tmph)) {
        subb[idx, laydiffh_nxt := c(tmph:(tmph + 1 - length(idx)))]
      }
    }
    
    # Calculate deltaT_pre and deltaT_nxt
    subb[, deltaT_pre := {
      lays <- ifelse(Eggsignal > 0, layh, NA_real_)
      prevlay <- data.table::nafill(lays, type = "locf")
      layh - data.table::shift(prevlay, type = "lag")
    }]
    
    subb[, deltaT_nxt := {
      lays <- ifelse(Eggsignal > 0, layh, NA_real_)
      nextlay <- data.table::nafill(lays, type = "nocb")
      layh - data.table::shift(nextlay, type = "lead")
    }]
    
    subb[is.na(deltaT_pre), deltaT_pre := -deltaT_nxt]
    subb[is.na(deltaT_nxt), deltaT_nxt := -deltaT_pre]
    
    # Calculate dd_pre and dd_nxt
    subb[, dd_pre := {
      lays <- ifelse(Eggsignal > 0, day, NA_real_)
      prevlay <- data.table::nafill(lays, type = "locf")
      day - data.table::shift(prevlay, type = "lag")
    }]
    
    subb[, dd_nxt := {
      lays <- ifelse(Eggsignal > 0, day, NA_real_)
      nextlay <- data.table::nafill(lays, type = "nocb")
      day - data.table::shift(nextlay, type = "lead")
    }]
    
    subb[is.na(dd_pre), dd_pre := -dd_nxt]
    subb[is.na(dd_nxt), dd_nxt := -dd_pre]
    
    subb[, step_pre := round(abs(deltaT_pre) / abs(ifelse(dd_pre == 0, 1, dd_pre)), 2)]
    subb[, step_nxt := round(abs(deltaT_nxt) / abs(ifelse(dd_nxt == 0, 1, dd_nxt)), 2)]
    
    # HMM fitting
    dt <- data.table::copy(subb)
    
    mod <- depmixS4::depmix(
      list(Eggsignal ~ 1, laydiffh_pre ~ layh, laydiffh_nxt ~ layh, 
           step_pre ~ step_nxt, laydiffh_pre ~ laydiffh_nxt),
      data = dt,
      nstates = nstate,
      family = list(stats::binomial(), stats::gaussian(), stats::gaussian(), 
                    stats::gaussian(), stats::gaussian())
    )
    
    # Run multiple fits until max prob for eggsignal for a state reaches stop_pegg
    max_pegg <- 0
    iter <- 0
    bestfit <- NULL
    bestint <- NULL
    
    while (max_pegg < stop_pegg & iter < max_iter) {
      iter <- iter + 1
      cat(sprintf("Attempting fit, Iteration: %d\n", iter))
      
      fit <- tryCatch({
        invisible(utils::capture.output(fit_result <- depmixS4::fit(mod, verbose = FALSE)))
        fit_result
      }, error = function(e) NULL)
      
      if (is.null(fit)) next
      
      # Extract intercepts for eggsignal
      int_eggsignal <- data.table(
        state = seq_len(nstate),
        int = sapply(fit@response, function(st) {
          st[[1]]@parameters$coefficients["(Intercept)"]
        })
      )
      int_eggsignal[, p_perc := 1 / (1 + exp(-int)) * 100]
      current_pegg <- max(int_eggsignal$p_perc)
      
      if (current_pegg > max_pegg) {
        max_pegg <- current_pegg
        bestfit <- fit
        bestint <- int_eggsignal
        cat(sprintf("NEW Maximum P(Eggsignal=1) for a state achieved: %.2f%%\n", current_pegg))
      }
      cat(sprintf("Maximum P(Eggsignal=1) for a state achieved: %.2f%%\n", current_pegg))
    }
    
    if (is.null(bestfit)) next
    
    laystates <- unlist(bestint[p_perc >= min_pegg, state])
    
    post <- depmixS4::posterior(bestfit, type = "viterbi")
    dt <- cbind(dt, post)
    dtlist[[length(dtlist) + 1]] <- dt
    
    # Get imputed date & layh
    laydates <- dt[Eggsignal > 0, Date]
    dtnew <- data.table::copy(dt[Eggsignal > 0, ])
    dtnew[, `:=`(
      prev_date = Date - 1,
      next_date = Date + 1,
      prev_missing = !((Date - 1) %in% laydates),
      next_missing = !((Date + 1) %in% laydates)
    )]
    
    freq_deltah <- max(2, abs(dtnew[, .N, by = deltaT_pre][order(-N)][, deltaT_pre][1]))
    dtnew[, `:=`(
      prev_est_lt = ifelse(prev_missing, layh - freq_deltah, NA_real_),
      next_est_lt = ifelse(next_missing, layh + freq_deltah, NA_real_)
    )]
    
    missing <- rbind(
      dtnew[prev_missing == TRUE, .(Date = prev_date, layh = prev_est_lt)],
      dtnew[next_missing == TRUE, .(Date = next_date, layh = next_est_lt)]
    )
    missing <- missing[order(Date)]
    imp <- merge(missing, dt[, .(Date, layh, state)], by = c("Date", "layh"))
    imp <- imp[state %in% laystates &
                 layh >= min(dt[Eggsignal > 0, layh]) &
                 layh <= max(dt[Eggsignal > 0, layh])]
    imp <- merge(imp, bestint[, .(state, p_perc)], by = "state")
    imp <- imp[, .SD[p_perc == max(p_perc)], by = Date]
    imp <- imp[, if (.N == 1) .SD, by = Date]
    
    # Save
    if (nrow(imp) > 0) {
      implist[[length(implist) + 1]] <- imp
    }
  }
  
  if (length(implist) > 0) {
    implist <- data.table::rbindlist(implist, fill = TRUE)
  } else {
    implist <- data.table()
  }
  
  if (nrow(implist) > 0) {
    implist[, `:=`(
      ani = aniid,
      Layingtime = hms::as_hms(layh * 60 * 60),
      imp = TRUE
    )]
    implist[, datelay := as.POSIXct(paste(Date, Layingtime), 
                                    format = "%Y-%m-%d %H:%M:%S", 
                                    tz = "Europe/Berlin")]
    
    if ("imp" %in% names(sub)) {
      x1 <- sub[imp == "dups", .(ani, Date, datelay, Layingtime, imp)]
      out <- rbindlist(list(implist, x1), fill = TRUE)
    } else {
      out <- implist
    }
    
    data.table::setorder(out, Date)
    
    # Imputed records only within usual Laying time
    if (nrow(dtnew) > 0) {
      quantiles <- quantile(as.numeric(dtnew$Layingtime), probs = c(0.01, 0.99), na.rm = TRUE)
      q1 <- quantiles[1]
      q2 <- quantiles[2]
      out[, outh := {
        t = as.numeric(Layingtime)
        (t < q1 | t > q2)
      }]
      out <- out[outh != TRUE | is.na(outh)]
    }
    
    # Remove imputed records failed the laydiffh threshold
    out[, laydiffh_pre := {
      as.numeric((datelay - data.table::shift(datelay, type = "lag")), units = "hours")
    }][, outld := laydiffh_pre < thrd_laydiffh]
    out[is.na(outld), outld := FALSE]
    out <- out[outld != TRUE]
    out <- out[, .(ani, Date, datelay, Layingtime, imp, p_perc)]
  } else {
    out <- data.table()
  }
  
  if (plot == TRUE) {
    if (nrow(out) == 0) {
      message("there is no imputed record")
    } else {
      if (!requireNamespace("patchwork", quietly = TRUE)) {
        stop("Package 'patchwork' is required but not installed.")
      }
      
      dtlist <- data.table::rbindlist(dtlist, fill = TRUE)
      dtlist[, Eggsignal := as.factor(Eggsignal)]
      dtlist[, size := factor(ifelse(Eggsignal == 1, 0.7, 0.5))]
      
      if (!is.null(bestint)) {
        pp <- paste0(bestint[, round(p_perc, 2)], collapse = ",")
      } else {
        pp <- "NA"
      }
      
      if ("imp" %in% names(sub)) {
        datg2 <- rbindlist(list(sub, out), fill = TRUE)
      } else {
        datg2 <- out
      }
      
      g1 <- ggplot2::ggplot(dtlist) +
        ggplot2::geom_point(ggplot2::aes(x = Date, y = layh, color = factor(state), 
                                         shape = Eggsignal, size = size)) +
        ggplot2::labs(title = paste0(aniid, ", (later)p%=", pp)) +
        ggplot2::scale_x_date(limits = c(min(datg2$Date), max(datg2$Date)))
      
      g2 <- ggplot2::ggplot(datg2) +
        ggplot2::geom_point(ggplot2::aes(x = Date, y = Layingtime, color = imp)) +
        ggplot2::labs(title = aniid) +
        ggplot2::scale_color_manual(values = c("TRUE" = "tomato", "dups" = "steelblue1"))
      
      print(g1 / g2)
    }
  }
  
  if (nrow(out) == 0) return(NULL)
  return(out[, .(ani, Date, datelay, Layingtime, imp, p_perc = round(p_perc, 5))][order(Date)])
}


#' Update prior probabilities using imputed laying dates
#'
#' Updates a prior probability matrix for egg-animal assignments based on 
#' imputed laying dates and filtered autonest observations.
#'
#' @param prior Numeric matrix with egg IDs as rows and animal IDs as columns.
#' @param ani_impeggs data.table of imputed laying dates with columns: 
#'   `ani`, `Date`, `imp`, `p_perc`.
#' @param from POSIXct start datetime of the observation window.
#' @param to POSIXct end datetime of the observation window.
#' @param date Date object specifying the egg date being updated.
#' @param pen_meta data.table of autonest data including columns: 
#'   `ani`, `Date`, `F_combined`.
#' @param weight Numeric weight to apply to observed/imputed events (default 1).
#' @param factor Numeric factor to scale the prior update (default 1).
#' @param p_perc_dups Numeric probability percentage for duplicate records (default 50).
#'
#' @return Updated numeric matrix of the same dimensions as `prior`.
#' @export
#'
update_prior_laydates <- function(prior, ani_impeggs, from, to, date, pen_meta,
                                  weight = 1, factor = 1, p_perc_dups = 50) {
  
  if (!inherits(prior, "matrix")) {
    stop("'prior' must be a matrix with egg IDs as rows and animal IDs as columns.")
  }
  
  if (nrow(prior) == 0) {
    return(prior)
  }
  if (nrow(ani_impeggs) == 0) {
    return(prior)
  }
  
  check_required_cols(ani_impeggs, c("ani", "Date", "imp", "p_perc"))
  check_required_cols(pen_meta, c("ani", "Date", "F_combined"))
  check_datetime_range(from, to)
  
  if (!inherits(date, "Date")) {
    stop("'date' must be a Date object (use as.Date()).")
  }
  
  for (arg_name in c("weight", "factor", "p_perc_dups")) {
    arg_val <- get(arg_name)
    if (!is.numeric(arg_val) || arg_val < 0) {
      stop(arg_name, " must be a non-negative numeric value.")
    }
  }
  
  # Give p_perc to "dups" recorded by the autonest
  if ("imp" %in% names(ani_impeggs)) {
    ani_impeggs[imp == "dups", p_perc := p_perc_dups]
  }
  
  for (ani_id in colnames(prior)) {
    if (nrow(pen_meta[datelay >= from & datelay <= to & ani == ani_id & 
                      F_combined != TRUE & Eggsignal > 0]) > 0) {
      lik <- 1 * weight
    } else if (nrow(ani_impeggs[Date == date & ani == ani_id]) > 0) {
      pp <- ani_impeggs[Date == date & ani == ani_id, p_perc] / 100
      lik <- ifelse(!is.na(pp), pp, 0) * weight
    } else {
      lik <- 0
    }
    
    prior[, ani_id] <- prior[, ani_id] * (factor + lik)
  }
  
  prior
}



#' Update prior matrix using laying patterns
#'
#' @description Adjusts prior probabilities based on candidates' laying patterns 
#' observed in nearby days from autonest data.
#'
#' @param prior Prior matrix `[egg x candidate]`.
#' @param eggs data.table with at least: `Nest`, `eggid`.
#' @param cand_ani Vector of candidate animal IDs.
#' @param pen_meta data.table with at least: `ani`, `Nestnumber`, `Date`, `Eggsignal`, `F_combined`, `laydiffh_pre_all`.
#' @param from POSIXct start datetime.
#' @param to POSIXct end datetime.
#' @param index_floornest Numeric pen index for floor eggs.
#' @param nearby_days Integer, number of days before & after `date` to include (default 2).
#' @param penalty_weight Numeric penalty for switching nests (default 0.3).
#' @param max_penalty Maximum nest-switching penalty allowed (default 0.6).
#' @param weight Numeric weight to apply to laying patterns of nearby days (default 0.8).
#' @param factor Numeric factor to scale the prior update (default 1).
#'
#' @return Updated prior matrix.
#' @export
#'
update_prior_nestlay <- function(prior, eggs, cand_ani, pen_meta, from, to, index_floornest,
                                 nearby_days = 2, penalty_weight = 0.3, max_penalty = 0.6, 
                                 weight = 0.8, factor = 1) {
  
  if (!inherits(prior, "matrix")) {
    stop("'prior' must be a matrix with egg IDs as rows and animal IDs as columns.")
  }
  
  if (nrow(prior) == 0) {
    return(prior)
  }
  
  check_datetime_range(from, to)
  check_required_cols(eggs, c("Nest", "eggid"))
  check_required_cols(pen_meta, c("ani", "Nestnumber", "Date", "Eggsignal", "F_combined", "laydiffh_pre_all"))
  
  if (!is.numeric(nearby_days) || nearby_days < 1) {
    stop("'nearby_days' must be a positive integer.")
  }
  nearby_days <- as.integer(nearby_days)
  
  for (arg_name in c("weight", "penalty_weight", "max_penalty", "factor")) {
    arg_val <- get(arg_name)
    if (!is.numeric(arg_val) || arg_val < 0) {
      stop(arg_name, " must be a non-negative numeric value.")
    }
  }
  
  if (!is.numeric(index_floornest)) {
    stop("'index_floornest' must be numeric/integer.")
  }
  
  seq_dates <- seq(as.Date(from) - nearby_days, as.Date(to) + nearby_days, by = "day")
  if (!all(seq_dates %in% pen_meta$Date)) {
    warning("Not all seq_dates (from-to) are present in pen_meta$Date")
  }
  
  if (!all(cand_ani %in% pen_meta$ani)) {
    missing_ani <- setdiff(cand_ani, pen_meta$ani)
    warning("Some candidate animals not found in pen_meta: ", paste(missing_ani, collapse = ", "))
  }
  
  pmdays <- pen_meta[ani %in% cand_ani & Eggsignal > 0 &
                       Date %in% seq_dates &
                       F_combined != TRUE, 
                     .(ani, Nestnumber, Date, datelay)]
  
  ## Calculate animal frequency
  ani_freq <- pmdays[, .(total_days = .N), by = ani]
  ani_freq[, layfreq := total_days / length(seq_dates)]
  
  nest_freq <- pmdays[, .(nest_visits = .N), by = .(ani, Nestnumber)]
  nest_freq[, nest_freq := nest_visits / sum(nest_visits), by = ani]
  
  # Add switch penalty if ani likes to switch too often
  nest_patterns <- pmdays[order(ani, Date)][, .(nest_visits = .N, 
                                                nswitch = length(rle(Nestnumber)$values) - 1), 
                                            by = ani]
  
  nest_patterns[, switch_rate := ifelse(nest_visits == 1, 0, nswitch / (nest_visits - 1)), by = ani]
  nest_patterns[, switch_penalty := pmin(switch_rate * penalty_weight, max_penalty)]
  
  nest_freq <- nest_freq[nest_patterns, on = "ani"]
  nest_freq[, nest_scores := nest_freq * (1 - switch_penalty)]
  
  lik_nest <- nest_freq[ani_freq, on = "ani"]
  lik_nest[, prob := nest_scores * layfreq]
  lik_floor <- lik_nest[, .(Nestnumber = index_floornest, prob = 1 - sum(prob)), by = ani]
  
  for (ani_id in unique(ani_freq$ani)) {
    lik <- rbind(lik_nest[ani == ani_id, .(ani, Nestnumber, prob)], 
                 lik_floor[ani == ani_id, .(ani, Nestnumber, prob)])
    egglik <- merge(eggs, lik, by.x = "Nest", by.y = "Nestnumber", all.x = TRUE)
    egglik[is.na(prob), prob := 0]
    prior[egglik$eggid, ani_id] <- prior[egglik$eggid, ani_id] * (factor + weight * egglik$prob)
  }
  
  prior
}


#' Update prior matrix using nest preference data
#'
#' @description Adjusts the prior probability matrix based on individual animals' nest
#' preferences, giving extra weight to their preferred nests while optionally scaling with a factor.
#'
#' @param prior Prior matrix `[egg x candidate]`.
#' @param eggs `data.table` with at least columns: `Nest`, `eggid`.
#' @param ani_prefer_nest `data.table` with at least columns: `ani`, `Nestnumber`, `prop`.
#' @param index_floornest Numeric pen index for floor eggs.
#' @param weight Numeric weight to apply to nest preference (default 0.2).
#' @param factor Numeric factor to scale the prior update (default 1).
#'
#' @return Updated prior matrix (same dimensions as input `prior`).
#' @export
#'
update_prior_nestpref <- function(prior, eggs, ani_prefer_nest, 
                                  index_floornest, weight = 0.2, factor = 1) {
  
  if (!inherits(prior, "matrix")) {
    stop("'prior' must be a matrix with egg IDs as rows and animal IDs as columns.")
  }
  if (nrow(prior) == 0) {
    return(prior)
  }
  
  check_required_cols(eggs, c("Nest", "eggid"))
  check_required_cols(ani_prefer_nest, c("ani", "Nestnumber", "prop"))
  
  for (arg_name in c("weight", "factor")) {
    arg_val <- get(arg_name)
    if (!is.numeric(arg_val) || arg_val < 0) {
      stop(arg_name, " must be a non-negative numeric value.")
    }
  }
  
  if (!is.numeric(index_floornest)) {
    stop("'index_floornest' must be numeric/integer.")
  }
  
  allnest <- setdiff(unique(eggs$Nest), index_floornest)
  
  for (n in allnest) {
    sub <- ani_prefer_nest[ani %in% colnames(prior) & Nestnumber == n, ]
    eid <- eggs[Nest == n, eggid]
    for (j in seq_len(nrow(sub))) {
      ani_id <- sub$ani[j]
      lik <- sub$prop[j] * weight
      prior[eid, ani_id] <- prior[eid, ani_id] * (factor + lik)
    }
  }
  prior
}


#' A helper function for egg assignment based on prior probabilities
#'
#' @description First assign nest eggs by searching for the globally highest probabilities, 
#' then the p of unselected candidates of assigned eggs (leftover) will be add on to their p for other eggs for competition
#' floor eggs assignment can include leftover from nest eggs assignments (but not the other way around).
#'
#' @param dt Prior data.table `[eid, ani, p]`.
#' @param nestleftover data.table of leftover priors `[eid, ani, p]`.
#' @param pen_meta data.table with animal metadata.
#' @return data.table of selected candidate-egg pairs.
#' @export
#'
assign_greedy <- function(dt, nestleftover = NULL, pen_meta) {
  
  if (nrow(dt) == 0) return(dt)
  
  best <- data.table()
  leftover <- data.table()
  
  ani_priority <- pen_meta[Eggsignal > 0 & ani != 0, .N, by = ani][order(-N), ani]
  ani_all <- unique(dt$ani)
  # Randomly shuffle priority of rest candidates as they don't have any info
  ani_res <- sample(ani_all[!ani_all %in% ani_priority])
  ani_order <- c(ani_priority, ani_res)
  
  # For floor assignment, add on nest leftover
  if (!is.null(nestleftover) && nrow(nestleftover) > 0) {
    nlo <- nestleftover[, .(p = sum(p)), by = ani]
    tmp <- merge(dt, nlo, by = "ani", all.x = TRUE)
    tmp[, p := p.x + ifelse(is.na(p.y), 0, p.y)]
    dt$p <- tmp$p
    nestleftover <- NULL
  }
  
  repeat {
    cand <- dt[!(eid %in% best$eid) & !(ani %in% best$ani)]
    
    if (nrow(cand) == 0) break
    
    top <- cand[p == max(p)][order(eid)]
    # If duplicates
    while (nrow(top) > 1) {
      
      if (length(unique(top$ani)) == 1) {
        top <- top[order(eid)][1]
        
      } else if (nrow(leftover) != 0) {
        lo <- leftover[, .(p = sum(p)), by = ani]
        tmp <- merge(top, lo, by = "ani", all.x = TRUE)
        tmp[, p := p.x + p.y]
        # select p after add on from leftover, if still dups, use info from pen_meta
        top <- tmp[, .SD[p == max(p)]]
        top <- top[, .(eid, ani, p.x, nest)]
        colnames(top) <- c("eid", "ani", "p", "nest")
      }
      
      sa <- ani_order[ani_order %in% top$ani][1]
      top <- top[ani == sa]
    }
    
    best <- rbind(best, top)
    # Keep ani's unused prior as ref if later dups showed up
    if (nrow(leftover) > 0) {
      r1 <- leftover[!(ani %in% best$ani)]
    } else {
      r1 <- data.table()
    }
    r2 <- dt[(eid == top$eid) & (ani != top$ani)]
    leftover <- rbind(r1, r2)
    # Check
    if (any(duplicated(best$ani)) | any(duplicated(best$eid))) {
      warning("Duplicated assignment detected!")
    }
  }
  
  return(best)
}


#' Normalise prior probabilities
#'
#' @description Ensures that the prior matrix rows and cols sum to 1 and optionally converts it to an
#' assignment matrix where each egg is assigned to the most probable candidate.
#'
#' @param prior Prior matrix `[egg x candidate]`.
#' @param result Character, either `"prob"` to normalize probabilities (default) or `"assign"` to assign top candidate per egg.
#' @param index_floornest Numeric index for floor eggs.
#' @param pen_meta data.table with animal metadata.
#'
#' @return Prior matrix with row sums = 1 (`result = "prob"`) or with top candidate assigned (`result = "assign"`).
#' @export
#'
Norm_prior <- function(prior, result = "prob", index_floornest, pen_meta) {
  
  if (!inherits(prior, "matrix")) {
    stop("'prior' must be a matrix with egg IDs as rows and animal IDs as columns.")
  }
  if (nrow(prior) == 0) {
    return(prior)
  }
  
  if (!result %in% c("prob", "assign")) {
    stop("Argument 'result' must be either 'prob' or 'assign'")
  }
  
  if (result == "prob") {
    prior <- sweep(prior, 1, rowSums(prior), "/")
    
    # Rescale if prob per ani > 1
    sums <- colSums(prior)
    idx <- which(sums > 1)
    if (length(idx) > 0) {
      prior[, idx] <- sweep(prior[, idx, drop = FALSE], 2, sums[idx], "/")
    }
  }
  
  if (result == "assign") {
    # Flatten the matrix, sort globally, and pick the highest prior
    dt <- as.data.table(as.table(prior))
    setnames(dt, c("eid", "ani", "p"))
    dt[, nest := as.integer(substr(eid, 7, 9))]
    
    dt_nest <- dt[nest != index_floornest]
    dt_floor <- dt[nest == index_floornest]
    
    # First assign nest egg, then floor egg
    sele <- assign_greedy(dt_nest, nestleftover = NULL, pen_meta)
    sele <- rbind(sele,
                  assign_greedy(
                    dt = dt_floor[!(ani %in% sele$ani)], 
                    nestleftover = dt_nest[!(ani %in% sele$ani)],
                    pen_meta)
    )
    
    for (i in seq_len(nrow(sele))) {
      prior[sele$eid[i], sele$ani[i]] <- 1
    }
    prior[prior != 1] <- 0
  }
  
  return(prior)
}


#' Process a single pen and date range to get trusted egg & animals pairs
#'
#' @description This function processes autonest metadata for a given pen across
#' multiple dates. It identifies candidate animals, detects trusted egg-animal pairs,
#' removes trusted pairs from candidates, builds and updates a prior matrix using
#' naive priors, imputed laying dates, nearby days' lay patterns, and nest preferences.
#'
#' @param p Pen ID.
#' @param negg data.table of egg counts per nest/floor per date.
#' @param meta data.table of autonest metadata.
#' @param ani_info data.table with animal presence/absence info.
#' @param index_floornest Integer ID used for floor eggs (default: 99).
#' @param ani_impeggs data.table of imputed laying dates.
#' @param weight_imd For prior update using imputed laying dates: Weight (default: 1).
#' @param factor_imd For prior update using imputed laying dates: factor (default: 1).
#' @param p_perc_dups Probability percentage for duplicate records (default: 50).
#' @param nearby_days Integer, for nearby lay pattern update: Number of days before & after the date to include (default: 2).
#' @param penalty_weight For nearby lay pattern update: Numeric penalty for switching nests (default: 0.3).
#' @param max_penalty For nearby lay pattern update: Maximum nest-switching penalty allowed (default: 0.6).
#' @param weight_nl For nearby lay pattern update: Weight (default: 0.8).
#' @param factor_nl For nearby lay pattern update: Factor (default: 1).
#' @param weight_np For nest preference update: Weight (default: 0.2).
#' @param factor_np For nest preference update: Factor (default: 1).
#' @param norm_result Character, either `"prob"` to normalize probabilities (default) or `"assign"` to assign top candidate per egg.
#'
#' @return A list with:
#' \describe{
#'   \item{pen_priors}{Data.table of prior matrices for each date.}
#'   \item{pen_trusted}{Data.table of trusted eggs for each date.}
#' }
#' @export
#'
process_pen <- function(p, negg, meta, ani_info,
                        index_floornest = 99,
                        ani_impeggs, weight_imd = 1, factor_imd = 1, p_perc_dups = 50,
                        nearby_days = 2, penalty_weight = 0.3, max_penalty = 0.6, 
                        weight_nl = 0.8, factor_nl = 1,
                        weight_np = 0.2, factor_np = 1,
                        norm_result = "prob") {
  
  if (!is.data.table(meta)) {
    stop("'meta' must be a data.table.")
  }
  if (!is.data.table(negg)) {
    stop("'negg' must be a data.table.")
  }
  if (!is.data.table(ani_info)) {
    stop("'ani_info' must be a data.table.")
  }
  if (!is.data.table(ani_impeggs)) {
    stop("'ani_impeggs' must be a data.table.")
  }  
  if (!is.numeric(nearby_days) || nearby_days < 1) {
    stop("'nearby_days' must be a positive integer.")
  }
  nearby_days <- as.integer(nearby_days)
  
  for (arg_name in c("weight_imd", "factor_imd", "p_perc_dups",
                     "penalty_weight", "max_penalty", "weight_nl", "factor_nl",
                     "weight_np", "factor_np")) {
    arg_val <- get(arg_name)
    if (!is.numeric(arg_val) || arg_val < 0) {
      stop(arg_name, " must be a non-negative numeric value.")
    }
  }
  
  if (!norm_result %in% c("prob", "assign")) {
    stop("Argument 'result' must be either 'prob' or 'assign'")
  }
  
  check_required_cols(negg, c("Date", "Pen", "Nest", "Nhand", "Nfloor", "date_time_pre", "date_time"))
  check_required_cols(meta, c("ani", "pen", "datelay", "Eggsignal", "F_combined", "type", "pri_count", "laydiffh_pre_pri", "Nestnumber"))
  check_required_cols(ani_info, c("ani", "Pen", "Leave"))
  if (nrow(ani_impeggs) != 0) {
    check_required_cols(ani_impeggs, c("ani", "Date", "imp", "p_perc"))
  }
  
  negg[, Date := as.Date(Date)]
  check_datetime(meta$datelay)
  check_datetime(negg$date_time_pre)
  check_datetime(negg$date_time)
  
  # Start
  cat("Pen:", p, " \n")
  pen_negg <- negg[Pen == p]
  dates <- sort(unique(pen_negg$Date))
  pen_meta <- meta[pen == p]
  
  # Chickens preferred nest
  ani_prefer_nest <- pen_meta[, .(sumdu = sum(Duration)), by = .(ani, Nestnumber)][ani != 0]
  ani_prefer_nest[, prop := as.numeric(sumdu) / as.numeric(sum(sumdu)), by = ani]
  ani_prefer_nest[is.na(prop), prop := 0]
  
  pen_priors <- list()
  pen_trusted <- list()
  
  for (d in seq_along(dates)) { 
    date <- dates[d]
    date <- as.Date(date)
    
    # Pen/date egg count
    pendate_negg <- pen_negg[Date == date]
    
    # Candidate animals in pen
    warning("Make sure the format of Date in ani_info is yyyy-mm-dd(e.g.'2024-01-01')")
    ani_info[, Leave := as.Date(Leave)]
    cand_ani <- ani_info[Pen == p & Leave >= date, ani]
    
    # Sanity check: too many animals
    if (length(cand_ani) > 21) {
      warning("Pen ", p, " has more than 21 animals?")
    }
    
    # Check if all auto_ani in cand_ani
    auto_ani <- unique(pen_meta$ani)
    auto_ani <- auto_ani[auto_ani != 0]
    x <- auto_ani[!auto_ani %in% cand_ani]
    if (length(x) != 0) {
      if (nrow(pen_meta[ani %in% x & Date >= date]) > 0) {
        warning("Animal ID(s) ", paste(x, collapse = ", "),
                " from autonest not in ani_info, please check!")
      }
    }
    
    # Build egg table
    eggs <- pendate_negg[, .(Nest = c(rep(Nest, Nhand), rep(index_floornest, Nfloor[1])))]
    eggs[, nestc := seq_len(.N), by = Nest]
    eggs[, eggid := sprintf("%s%03d%02d", format(date, "%y%m%d"), Nest, nestc)]
    eggs[, nestc := NULL]
    
    from <- min(pendate_negg$date_time_pre)
    to <- max(pendate_negg$date_time)
    
    # Trusted eggs from autonest
    dt_trust <- get_trusted_autonest(eggs, pen_meta, from, to)
    
    # Remove trusted animals/eggs from candidates
    cand_ani <- cand_ani[!cand_ani %in% dt_trust$ani]
    eggs <- eggs[!eggid %in% dt_trust$eid]
    
    # Naive prior
    prior <- make_naive_prior(eggs$eggid, cand_ani)
    
    # Update by laydates (+imputed) based on laying pattern throughout the period
    prior <- update_prior_laydates(prior, ani_impeggs, from, to, date, pen_meta,
                                   weight = weight_imd, factor = factor_imd, p_perc_dups = p_perc_dups)
    
    # Update by nearby days' laying pattern
    prior <- update_prior_nestlay(prior, eggs, cand_ani, pen_meta, from, to, 
                                  index_floornest,
                                  nearby_days = nearby_days, 
                                  penalty_weight = penalty_weight, max_penalty = max_penalty, 
                                  weight = weight_nl, factor = factor_nl)
    
    # Update by nest preference
    prior <- update_prior_nestpref(prior, eggs, ani_prefer_nest, 
                                   index_floornest, weight = weight_np, factor = factor_np)
    
    # Normalise prior
    prior <- Norm_prior(prior, result = norm_result, index_floornest, pen_meta)
    
    # Check prob sums
    bad <- which(rowSums(prior) > 1 + 1e-6)
    if (length(bad) > 0) {
      warning("Date ", date, ": prob per egg (rowSums) > 1")
    }
    
    # Save per date results
    if (nrow(prior) > 0) {
      pen_priors[[length(pen_priors) + 1]] <- prior
    }
    if (nrow(dt_trust) > 0) {
      pen_trusted[[length(pen_trusted) + 1]] <- data.table(dt_trust, date, pen = p)
    }
  }
  
  # Merge per pen results
  pen_trusted <- rbindlist(pen_trusted, fill = TRUE)
  
  pen_priors_dat <- rbindlist(lapply(pen_priors, function(mat) {
    dt <- as.data.table(mat, keep.rownames = "eid")
    melted <- melt(dt, 
                   id.vars = c("eid"),
                   variable.name = "ani",
                   value.name = "prior")
    return(melted)
  }), fill = TRUE)
  
  pen_priors_dat[, pen := p]
  
  if (norm_result == "assign") {
    pen_priors_dat <- pen_priors_dat[prior != 0]
  }
  
  return(list(
    pen_priors = pen_priors_dat,
    pen_trusted = pen_trusted
  ))
}


#' Add cross-validation fold assignments to a data.table
#'
#' @description Randomly assigns each row of a data.table to one of `k` cross-validation folds.
#'
#' @param dt data.table to which folds should be added.
#' @param k Integer, number of folds (default = 5).
#' @param seed Integer, random seed for reproducibility (default = 123).
#'
#' @return The input data.table `dt` with an added column `fold`.
#' @export
#'
Add_cv_folds <- function(dt, k = 5, seed = 123) {
  set.seed(seed)
  n <- nrow(dt)
  fold_ids <- sample(rep(1:k, length.out = n))
  dt[, fold := fold_ids]
  return(dt)
}


#' Cross-validation for pen-level trusted egg-animal pairs
#'
#' @description Performs cross-validation by masking trusted egg-animal pairs in a pen
#' and evaluating the prior assignment procedure.
#'
#' @param pen_trusted_dat Data table of trusted egg-animal pairs with at least columns: `eid`, `ani`.
#' @param reps Number of CV repetitions (default = 2).
#' @param k Number of folds per repetition (default = 5).
#' @param p Pen ID.
#' @param negg data.table of egg counts per nest/floor per date.
#' @param meta data.table of autonest metadata.
#' @param ani_info data.table with animal presence/absence info.
#' @param index_floornest Integer ID used for floor eggs (default: 99).
#' @param ani_impeggs data.table of imputed laying dates.
#' @param weight_imd For prior update using imputed laying dates: Weight (default: 1).
#' @param factor_imd For prior update using imputed laying dates: factor (default: 1).
#' @param p_perc_dups Probability percentage for duplicate records (default: 50).
#' @param nearby_days Integer, for nearby lay pattern update: Number of days before & after the date to include (default: 2).
#' @param penalty_weight For nearby lay pattern update: Numeric penalty for switching nests (default: 0.3).
#' @param max_penalty For nearby lay pattern update: Maximum nest-switching penalty allowed (default: 0.6).
#' @param weight_nl For nearby lay pattern update: Weight (default: 0.8).
#' @param factor_nl For nearby lay pattern update: Factor (default: 1).
#' @param weight_np For nest preference update: Weight (default: 0.2).
#' @param factor_np For nest preference update: Factor (default: 1).
#' @param norm_result Character, either `"prob"` to normalize probabilities (default) or `"assign"`.
#'
#' @return Data table of masked trusted egg-animal pairs with evaluation results.
#' @export
#'
CV_pen <- function(pen_trusted_dat, reps = 2, k = 5, seed = 123,
                   p, negg, meta, ani_info,
                   index_floornest = 99,
                   ani_impeggs, weight_imd = 1, factor_imd = 1, p_perc_dups = 50,
                   nearby_days = 2, penalty_weight = 0.3, max_penalty = 0.6, 
                   weight_nl = 0.8, factor_nl = 1,
                   weight_np = 0.2, factor_np = 1,
                   norm_result = "prob") {
  
  if (!is.data.table(meta)) {
    stop("'meta' must be a data.table.")
  }
  if (!is.data.table(negg)) {
    stop("'negg' must be a data.table.")
  }
  if (!is.data.table(ani_info)) {
    stop("'ani_info' must be a data.table.")
  }
  if (!is.data.table(ani_impeggs)) {
    stop("'ani_impeggs' must be a data.table.")
  }  
  if (!is.numeric(nearby_days) || nearby_days < 1) {
    stop("'nearby_days' must be a positive integer.")
  }
  
  nearby_days <- as.integer(nearby_days)
  
  for (arg_name in c("weight_imd", "factor_imd", "p_perc_dups",
                     "penalty_weight", "max_penalty", "weight_nl", "factor_nl",
                     "weight_np", "factor_np")) {
    arg_val <- get(arg_name)
    if (!is.numeric(arg_val) || arg_val < 0) {
      stop(arg_name, " must be a non-negative numeric value.")
    }
  }
  
  if (!norm_result %in% c("prob", "assign")) {
    stop("Argument 'result' must be either 'prob' or 'assign'")
  }
  
  if (!is.numeric(reps) || reps < 1) stop("'reps' must be a positive integer")
  if (!is.numeric(k) || k < 2) stop("'k' (folds) must be integer >= 2")
  if (!is.numeric(seed)) stop("'seed' must be numeric")
  
  check_required_cols(negg, c("Date", "Pen", "Nest", "Nhand", "Nfloor", "date_time_pre", "date_time"))
  check_required_cols(meta, c("ani", "pen", "datelay", "Eggsignal", "F_combined", "type", "pri_count", "laydiffh_pre_pri", "Nestnumber"))
  check_required_cols(ani_info, c("ani", "Pen", "Leave"))
  check_required_cols(pen_trusted_dat, c("eid", "ani"))
  if (nrow(ani_impeggs) != 0) {
    check_required_cols(ani_impeggs, c("ani", "Date", "imp", "p_perc"))
  }  
  negg[, Date := as.Date(Date)]
  check_datetime(meta$datelay)
  check_datetime(negg$date_time_pre)
  check_datetime(negg$date_time)
  
  # Start
  pen_negg <- negg[Pen == p]
  dates <- sort(unique(pen_negg$Date))
  pen_meta <- meta[pen == p]
  
  # Chickens preferred nest
  ani_prefer_nest <- pen_meta[, .(sumdu = sum(Duration)), by = .(ani, Nestnumber)][ani != 0]
  ani_prefer_nest[, prop := as.numeric(sumdu) / as.numeric(sum(sumdu)), by = ani]
  ani_prefer_nest[is.na(prop), prop := 0]
  
  # CV
  masked_res <- list()
  
  for (r in seq_len(reps)) {
    pen_trusted_dat <- Add_cv_folds(pen_trusted_dat, k = k, seed = seed + r)
    
    for (f in seq_len(k)) {
      cat("Pen:", p, ", rep =", r, ", fold =", f, ", seed =", seed, "\n")
      mask_info <- pen_trusted_dat[fold == f]
      
      for (d in seq_along(dates)) { 
        date <- dates[d]
        date <- as.Date(date)
        
        # Pen/date egg count
        pendate_negg <- pen_negg[Date == date]
        
        # Candidate animals in pen
        warning("Make sure the format of Date in ani_info is yyyy-mm-dd(e.g.'2024-01-01')")
        ani_info[, Leave := as.Date(Leave)]
        cand_ani <- ani_info[Pen == p & Leave >= date, ani]
        
        # Sanity check: too many animals
        if (length(cand_ani) > 25) {
          warning("Pen ", p, " has more than 25 animals?")
        }
        
        # Check if all auto_ani in cand_ani
        auto_ani <- unique(pen_meta$ani)
        auto_ani <- auto_ani[auto_ani != 0]
        x <- auto_ani[!auto_ani %in% cand_ani]
        if (length(x) != 0) {
          if (nrow(pen_meta[ani %in% x & Date >= date]) > 0) {
            warning("Animal ID(s) ", paste(x, collapse = ", "),
                    " from autonest not in ani_info, please check!")
          }
        }
        
        # Build egg table
        eggs <- pendate_negg[, .(Nest = c(rep(Nest, Nhand), rep(index_floornest, Nfloor[1])))]
        eggs[, nestc := seq_len(.N), by = Nest]
        eggs[, eggid := sprintf("%s%03d%02d", format(date, "%y%m%d"), Nest, nestc)]
        eggs[, nestc := NULL]
        
        # Trusted pairs from autonest
        from <- min(pendate_negg$date_time_pre)
        to <- max(pendate_negg$date_time)
        dt_trust <- get_trusted_autonest(eggs, pen_meta, from, to)
        
        if (nrow(dt_trust) == 0) {
          next
        }
        
        # Mark down the trust rows which will be masked
        masked_rows <- dt_trust[mask_info, on = .(eid, ani), nomatch = NULL]
        
        if (nrow(masked_rows) == 0) {
          next
        }
        
        # Remove the trust rows if it exists in mask_info
        dt_trust_new <- dt_trust[!mask_info, on = .(eid)]
        
        # Remove trusted animals/eggs from candidates
        cand_ani <- cand_ani[!cand_ani %in% dt_trust_new$ani]
        eggs <- eggs[!eggid %in% dt_trust_new$eid]
        
        # Naive prior
        prior <- make_naive_prior(eggs$eggid, cand_ani)
        
        # Update by laydates (+imputed) based on laying pattern throughout the period
        prior <- update_prior_laydates(prior, ani_impeggs, from, to, date, pen_meta,
                                       weight = weight_imd, factor = factor_imd, p_perc_dups = p_perc_dups)
        
        # Update by nearby days' laying pattern
        prior <- update_prior_nestlay(prior, eggs, cand_ani, pen_meta, from, to, 
                                      index_floornest = index_floornest,
                                      nearby_days = nearby_days, 
                                      penalty_weight = penalty_weight, max_penalty = max_penalty, 
                                      weight = weight_nl, factor = factor_nl)
        
        # Update by nest preference
        prior <- update_prior_nestpref(prior, eggs, ani_prefer_nest, 
                                       index_floornest, weight = weight_np, factor = factor_np)
        
        # Normalise prior
        prior <- Norm_prior(prior, result = norm_result, index_floornest, pen_meta)
        
        # Check prob sums
        bad <- which(rowSums(prior) > 1 + 1e-6)
        if (length(bad) > 0) {
          warning("Date ", date, ": prob per egg (rowSums) > 1")
        }
        
        # Merge to compare masked_rows priors
        dt <- as.data.table(prior, keep.rownames = "eid")
        melted_prior <- melt(dt, 
                             id.vars = c("eid"),
                             variable.name = "ani",
                             value.name = "prior")
        
        # Consider the nest, ignore the order
        melted_prior[, nest := substr(eid, 7, 9)]
        if (norm_result == "assign") {
          melted_prior <- melted_prior[prior != 0]
        }
        
        masked_rows[, nest := substr(eid, 7, 9)]
        egg_quota <- masked_rows[, .N, by = nest]
        
        top_candidates <- melted_prior[nest %in% masked_rows$nest,
                                       .SD[order(-prior)], by = nest
        ][, .SD[1], by = .(nest, ani)]
        
        topx <- rbindlist(lapply(seq_len(nrow(egg_quota)), function(i) {
          nnest <- egg_quota$nest[i]
          nn <- egg_quota$N[i]
          top_candidates[nest == nnest][1:nn]
        }))
        
        masked_rows[topx, on = .(ani, nest), 
                    `:=`(topx = TRUE, prior = i.prior)
        ][is.na(topx), topx := FALSE][, rep := r]
        
        masked_res[[length(masked_res) + 1]] <- masked_rows
      }
    }
  }
  
  masked_res <- rbindlist(masked_res, fill = TRUE)
  return(masked_res)
}
