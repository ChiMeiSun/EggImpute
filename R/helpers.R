#' @import data.table
NULL

#' Check for required columns in a data.table
#'
#' This helper function verifies that all specified columns exist in a given
#' `data.table`. If any required columns are missing, the function stops with
#' an informative error message.
#'
#' @param dt A `data.table` to check.
#' @param cols A character vector of required column names.
#'
#' @return Invisibly returns `TRUE` if all required columns are present. 
#' Stops with an error if any columns are missing.
#' @export
#'
check_required_cols <- function(dt, cols) {
  if (!data.table::is.data.table(dt)) {
    stop("`dt` must be a data.table.")
  }
  if (!is.character(cols)) {
    stop("`cols` must be a character vector.")
  }
  
  missing <- setdiff(cols, names(dt))
  if (length(missing) > 0) {
    stop("Missing required column(s): ", paste(missing, collapse = ", "))
  }
  
  invisible(TRUE)
}


#' Validate POSIXct datetime range inputs
#'
#' This helper function ensures that `from` and `to` are valid POSIXct datetime objects, 
#' that they use the same timezone, and that `from` is not later than `to`.
#'
#' @param from A POSIXct start datetime.
#' @param to A POSIXct end datetime.
#'
#' @return Invisibly returns `TRUE` if all checks pass.  
#' Throws an error if inputs are invalid.
#' @export
#'
check_datetime_range <- function(from, to) {
  if (!inherits(from, "POSIXct")) {
    stop("`from` must be a POSIXct datetime object. Use as.POSIXct('YYYY-MM-DD HH:MM:SS').")
  }
  if (!inherits(to, "POSIXct")) {
    stop("`to` must be a POSIXct datetime object. Use as.POSIXct('YYYY-MM-DD HH:MM:SS').")
  }
  
  if (!identical(attr(from, "tzone"), attr(to, "tzone"))) {
    warning(sprintf(
      "`from` and `to` do not have the same timezone. from = %s, to = %s",
      attr(from, "tzone"), attr(to, "tzone")
    ))
  }
  
  if (from > to) {
    stop("`from` must be earlier than `to`.")
  }
  
  invisible(TRUE)
}


#' Validate POSIXct datetime input
#'
#' This helper function checks whether the provided input is a POSIXct datetime object.
#' 
#' @param input A single R object. Expected to be POSIXct datetime.
#'
#' @return Invisible TRUE if the input passes the check.
#' @export
#'
check_datetime <- function(input) {
  if (!inherits(input, "POSIXct")) {
    stop(input, " must be POSIXct datetime objects. 
         Use as.POSIXct('YYYY-MM-DD HH:MM:SS'), be aware of the timezone.")
  }
  
  invisible(TRUE)
}


#' Plot laying pattern of a single animal
#'
#' Visualizes the laying times of a given animal across dates, highlighting flagged eggs.
#'
#' @param pen_meta data.table containing autonest data, must include columns:
#'   `ani`, `Date`, `Eggsignal`, `F_combined`, `Layingtime`.
#' @param aniid Character or numeric ID of the animal to plot.
#' @param impute Logical, whether to include imputed laying dates (default = FALSE).
#' @param p Pen ID for title (optional).
#'
#' @return A ggplot object displaying laying times by date for the selected animal.
#' @export
#' @import ggplot2
#' @importFrom hms as_hms
#'
plot_laying_ani <- function(pen_meta, aniid, impute = FALSE, p = NULL) {
  
  check_required_cols(pen_meta, 
                      c("ani", "Date", "Eggsignal", "F_combined", "Layingtime"))
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required but not installed.")
  }
  
  seq_dates <- data.table::data.table(Date = seq(min(pen_meta$Date), max(pen_meta$Date), by = "days"))
  
  y1 <- min(pen_meta[Eggsignal > 0, Layingtime])
  y2 <- max(pen_meta[Eggsignal > 0, Layingtime])
  
  dat <- data.table::copy(pen_meta[ani == aniid])
  dat <- dat[seq_dates, on = .(Date = Date), nomatch = NA][order(Date)]
  dat[Eggsignal < 1, Layingtime := hms::as_hms("08:00:00")]
  dat[, Eggsignal := as.factor(Eggsignal)]
  
  if (impute == TRUE) {
    impd <- get_imputed_laydates(pen_meta, aniid)
    if (!is.null(impd)) {
      impd[, Eggsignal := 1]
      impd[, F_combined := "impute"]
      impd[, Layingtime := hms::as_hms("10:00:00")]
      dat <- rbindlist(list(dat, impd), fill = TRUE)
    }
  }
  
  title_text <- if (!is.null(p)) {
    paste0("Pen ", p, ": Laying time by ani: ", aniid)
  } else {
    paste0("Laying time by ani: ", aniid)
  }
  
  gg <- ggplot2::ggplot(dat) +
    ggplot2::geom_point(ggplot2::aes(x = Date, y = Layingtime, color = F_combined, shape = Eggsignal), 
                        alpha = 0.7, size = 2) +
    ggplot2::scale_y_continuous(limits = c(as.numeric(y1), as.numeric(y2)),
                                labels = function(sec) format(hms::as_hms(sec))) +
    ggplot2::scale_x_date(date_breaks = "1 day", date_labels = "%b-%d") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::labs(title = title_text)
  
  print(gg)
  
  invisible(gg)
}


#' Load results files RData by type
#'
#' @param types Character of types to load (e.g., c("prob", "assign") or one of them)
#' @param pattern Pattern to match files (default: looks for .RData files)
#' @param directory Directory to search (default: current working directory)
#' @param new The rank position of the newest files sorted by date (default: 1)
#'
#' @return A list with elements for each type containing loaded objects
#' @export
#'
load_results <- function(types = c("prob", "assign"), 
                                 pattern = "\\.RData$",
                                 directory = ".",
                                 new = 1) {
  for (type in types) {
    if (!type %in% c("prob", "assign")) {
      stop(sprintf("Type '%s' is invalid. Must be 'prob', 'assign', or both", type))
    }
  }
  new <- as.integer(new)
  if (!is.integer(new)) warning("new -> the rank position of the newest file sorted by date, used new = 1")
  
  res_list <- list()
  
  for (type in types) {
    files <- list.files(path = directory, 
                        pattern = paste0(type, ".*", pattern),
                        full.names = TRUE)
    
    if (length(files) == 0) {
      warning(sprintf("No files found for type: %s", type))
      next
    }
    
    # Get the most recent file
    file <- sort(files, decreasing = TRUE)[new]
    message(sprintf("Loading: %s", basename(file)))
    
    # Load file into a temporary environment
    tmp_env <- new.env()
    load(file, envir = tmp_env)
    
    # Extract and save the loaded objects
    obj_names <- ls(tmp_env)
    loaded_objs <- mget(obj_names, envir = tmp_env)
    
    res_list[[type]] <- loaded_objs
  }
  
  return(res_list)
}


#' Get CV results from function "CV_pen"
#'
#' @param res_list A list contains both or either "prob" and "assign" output
#'
#' @return A data.table of results, `pen`, `TF`(True/False), `prop`(proportion), `type`(prob/assign)
#' @export
#'
get_CV <- function(res_list) {
  data.table::rbindlist(lapply(names(res_list), function(type_name) {
  CV_res <- res_list[[type_name]]$CV_res
  
  tmp <- data.table::rbindlist(lapply(seq_along(CV_res), function(i) {
    dat <- CV_res[[i]]
    tab <- table(factor(dat$topx, levels = c(TRUE, FALSE))) / nrow(dat)
    data.table(pen = i, TF = names(tab), prop = as.numeric(tab))
  }))
  
  tmp[, type := type_name]
  tmp
}))
}

#' Get summed prob & assign results over a period from function "process_pen"
#'
#' @param res_list A list contains both or either "prob" and "assign" output
#'
#' @return A data.table of results, `ani`, `sum_prior`(sum of prob), `type`(prob/assign)
#' @export
#'
get_sumprob <- function(res_list) {
  data.table::rbindlist(lapply(names(res_list), function(type_name) {
    pen_res <- res_list[[type_name]]$pen_res
    
    tmp <- data.table::rbindlist(lapply(pen_res, function(pen) {
      pen$pen_priors
    }))
    
    tmpp <- tmp[, .(sum_prior = sum(prior)), by = ani]
    
    tmpp[, type := type_name]
    tmpp
  }))
}

#' Get prob/assign results per egg from function "process_pen"
#'
#' @param res_list A list contains both or either "prob" and "assign" output
#'
#' @return A data.table of results, `eid`(egg id), `ani`, `prior`, `pen`, `Date`, `type`(prob/assign)
#' @export
#'
get_prob <- function(res_list, type = "prob") {
  if (! type %in% c("prob", "assign")) {
    stop("type must be either 'prob' or 'assign'")
  }
  
  
  data.table::rbindlist(lapply(type, function(type_name) {
    pen_res <- res_list[[type]]$pen_res
    
    tmp <- data.table::rbindlist(lapply(pen_res, function(pen) {
      pen$pen_priors
    }))
    tmp[, date := as.Date(substr(eid, 1,6), format = "%y%m%d")]
    tmp[, type := type_name]
    tmp
  }))
}

#' Get trusted set from function "process_pen"
#'
#' @param res_list A list contains both or either "prob" and "assign" output
#'
#' @return A data.table of results,`eid`(egg id), `ani`, `layingtime`(from autonest) 
#' `type`(primary/secondary), `date`, `pen`, `fold`(if CV=TRUE)
#' @export
#'
get_trusted <- function(res_list) {
  data.table::rbindlist(lapply(res_list[[names(res_list)[1] ]]$pen_res, function(pen) {
    pen$pen_trusted
  }))
}



#' Get prob/assign results as a [date x ani] matrix
#'
#' @param res_list A list contains both or either "prob" and "assign" output
#'
#' @return A matrix of results [date x ani]
#' @export
#'
get_matrix <- function(res_list, type = "assign") {
  if (! type %in% c("prob", "assign")) {
    stop("type must be either 'prob' or 'assign'")
  }
  if (!type %in% names(res_list)) stop(type, " is not found in the result list")
      
      
  if (type == "prob") {
    respp <- get_prob(res_list, type = "prob")
    
    respp <- respp[, .(
      sump = sum(prior, na.rm = TRUE),
      # Keep other columns from first row
      prior = first(prior)
    ), by = .(ani, date)]
    if (nrow(respp[prior > sump]) > 0 ) warning("Something is wrong when type = prob (prior > sump)")
    respp[, prior := round(sump, 2)]
    
    } else {
    respp <- get_prob(res_list, type = "assign")
  }
  
  restt <- get_trusted(res_list)
  
  # check
  check <- respp[, .N, by = .(date, ani)][N>1]
  if (nrow(check) > 0 ) warning("More than one egg per day for an animal is detected!")
  c1 <- respp[, .N, by = .(date, ani)]
  c2 <- restt[, .N, by = .(date, ani)]
  check <- merge(c1, c2, by = c("date", "ani"))
  if (nrow(check) > 0 ) warning("More than one egg per day for an animal is detected!")
  #
  all <- merge(respp, restt, by = c("date", "ani"), all = TRUE)
  all[is.na(prior), prior := 1]
  resmatrix <- data.table::dcast(all, date ~ ani, 
                     value.var = "prior",
                     fill = 0)
  message("Got the result matrix for type =", type, "\n")
  resmatrix
}


