#' @import data.table
#' @import hms
#' @import lubridate
NULL

#' Prepare autonest laying data
#'
#' Cleans and formats raw autonest metadata, converts times to `hms` objects,
#' converts dates to `Date` objects, and marks primary vs secondary animals.
#'
#' @param meta A `data.table` containing raw autonest data.
#' @param ot_min How many minutes to correct for the autonest clock (default NULL).
#' @param dateformat Date format in meta (default "%d.%m.%Y").
#' @param from,to `Date` or `character(1)`, optional date range (format yyyy-mm-dd.
#' 
#' @return A `data.table` with cleaned time/date columns (`hms`/`Date`), and 
#'   additional columns: `ani` and `type`.
#' @export
#'
prep_data <- function(meta_ori, 
                      ot_min = NULL, 
                      dateformat = "%d.%m.%Y",
                      from = NULL, to = NULL) {
  if (!inherits(meta_ori, "data.table")) stop("`meta` must be a data.table")
  check_date(from)
  check_date(to)
  
  meta <- data.table::copy(meta_ori)
  # Clean column names
  setnames(meta, gsub(" ", "", names(meta)))
  
  check_required_cols(meta, c("Start", "End", "Duration", "Layingtime", "Date",
                              "Animalmark", "Previousanimalmark"))
  
  if (!is.null(ot_min)) {
    if (!is.numeric(ot_min)) {
      ot_min <- suppressWarnings(as.numeric(ot_min))
      if (is.na(ot_min)) stop("ot_min must be a numeric value or NULL (default).")
    }
    ot_min <- hms::as_hms(ot_min * 60)
  }
  
  # Make date & time format
  meta[, Date := as.Date(Date, format = dateformat)]
  if (any(is.na(meta$Date))) stop ("as.Date() failed! maybe dateformat is wrong!")
  meta[lubridate::year(Date) < 100, Date := update(Date, year = lubridate::year(Date) + 2000)]
  
  if (!is.null(from)) {
    cat("subsetting data: Date >= ", as.character(from), "\n")
    meta <- meta[Date >= as.Date(from)]
  }
  if (!is.null(to)) {
    cat("subsetting data: Date <= ", as.character(to), "\n")
    meta <- meta[Date <= as.Date(to)]
  }
  
  time_cols <- c("Start", "End", "Duration", "Layingtime")
  
  # Make empty time as NA
  meta[, (time_cols) := lapply(.SD, function(x) {
    x[x == "" | is.na(x)] <- NA_character_
    return(x)
  }), .SDcols = time_cols]
  
  meta[, (time_cols) := lapply(.SD, hms::as_hms), .SDcols = time_cols]
  
  # Correction for time
  if (!is.null(ot_min)) {
    time_cols <- c("Start", "End", "Layingtime")
    meta[, (time_cols) := lapply(.SD, function(x) hms::as_hms(x + ot_min)), .SDcols = time_cols]
  }
  

  
  # Mark ani & pri/sec animals
  meta[, ani := Animalmark]
  meta[Animalmark == 0, ani := Previousanimalmark]
  meta[, ani := as.character(ani)]
  
  meta[, type := "pri"]
  meta[is.na(Animalmark), type := NA_character_]
  meta[Animalmark == 0, type := "sec"]
  
  return(meta)
}


#' Assign pen numbers based on nest numbers
#'
#' Divides a set of nest numbers into pens of equal size and merges the
#' pen assignment back into the metadata.
#'
#' @param meta A `data.table` containing at least the column `"Nestnumber"`.
#' @param Nestnumbers Numeric or character vector of nest numbers to assign to pens.
#' @param num_per_pen Integer, number of nests per pen.
#' @param verbose Logical, whether to print mapping (default TRUE).
#'
#' @return A `data.table` identical to `meta` but with an additional column `pen`.
#' @export
#'
Make_pen_number <- function(meta, Nestnumbers, num_per_pen, verbose = TRUE) {
  if (!inherits(meta, "data.table")) stop("`meta` must be a data.table")
  if (!is.numeric(num_per_pen) || num_per_pen <= 0) stop("`num_per_pen` must be a positive integer")
  
  check_required_cols(meta, c("Nestnumber"))
  
  if (!all(Nestnumbers %in% unique(meta$Nestnumber))) {
    missing_vals <- setdiff(Nestnumbers, unique(meta$Nestnumber))
    stop("Some Nestnumbers not found in meta: ", paste(missing_vals, collapse = ", "))
  }
  
  if (length(Nestnumbers) %% num_per_pen != 0) {
    stop("Length of Nestnumbers (", length(Nestnumbers), 
         ") is not divisible by num_per_pen (", num_per_pen, ")")
  }
  
  n_pens <- length(Nestnumbers) / num_per_pen
  mapping <- data.table(
    Nestnumber = Nestnumbers,
    pen = rep(seq_len(n_pens), each = num_per_pen)
  )
  
  meta <- merge(meta, mapping, by = "Nestnumber", all.x = TRUE)
  
  if (verbose) {
    cat("Pen–Nestnumber mapping:\n")
    print(mapping[, .(Nestnumbers = paste(Nestnumber, collapse = ", ")), by = pen])
  }
  
  return(meta)
}



#' Create Test Windows with Minimum Days
#'
#' This function generates test windows between a start and end date,
#' ensuring each window contains at least a minimum number of days.
#' If the remaining days at the end are less than the minimum,
#' they are added to the last window.
#'
#' @param start_date Start date of the testing period (Date or character)
#' @param end_date End date of the testing period (Date or character)
#' @param min_days Minimum number of days in each window (default = 30)
#'
#' @return A data.table with columns 'from' and 'to' for each window
#'
#' @export
get_test_win <- function(start_date, end_date, min_days = 30) {
  check_date(start_date)
  check_date(end_date)
  start_date <- as_date(start_date)
  end_date <- as_date(end_date)
  
  if (start_date >= end_date) stop("start_date must be before end_date")
  if (!is.numeric(min_days) || min_days < 1) stop("`num_per_pen` must be a positive integer >1")


  total_days <- as.numeric(end_date - start_date) + 1
  n_windows <- max(1, floor(total_days / min_days))
  
  from_dates <- start_date + days((0:(n_windows-1)) * min_days)
  to_dates <- pmin(from_dates + days(min_days - 1), end_date)
  
  # Adjust last window for remainder days
  if (n_windows > 1) {
    to_dates[n_windows] <- end_date
  }
  
  # Return result
  data.table(from = from_dates, to = to_dates)
}



#' Combine hand-counted and automated egg data for validation
#'
#' @description Reads and integrates manually counted egg data with automatic
#' nest system data to generate a harmonized table for evaluating egg count accuracy.
#'
#' @param meta A `data.table` containing autonest data after prep_data.
#' @param hand `data.table` containing hand-count egg data, required columns: `Date`(format yyyy-mm-dd), 
#' `Time_end`(format hh:mm:ss), `pen`, `Nfloor`, and columns included in param `hand_nest_colnames`
#' @param from,to `Date` or `character(1)`, optional date range (format yyyy-mm-dd.
#' @param hand_nest_colnames Column names for nests per pen (default: `Nest1, Nest2, Nest3, Nest4`).
#' @param Nestnumbers Vector of nest numbers (default: `1:64`).
#' @param num_per_pen Number of nests per pen (default: 4).
#' @param timezone Time zone used for datetime conversion (default: `"Europe/Berlin"`).
#' @param collect_min Total time needed in minutes for egg collection (default: 20).
#' @param fakeegg TRUE/FALSE, if manual transponder for precise time record is available.
#'
#' @return A `data.table` containing hand and automatic egg counts per nest and time interval.
#' with the following key columns:
#'   - `Date`: Date of observation  
#'   - `Pen`: pen ID  
#'   - `Nest`: nest number  
#'   - `Nhand`: manually counted eggs  
#'   - `Nfloor`: manually counted floor eggs  
#'   - `dtm_a`: algorithm adjusted autonest time (from fakeegg transponder if available)  
#'   - `dtm_h` : hand recorded datetime, replaced by reliable dtm_a (if available)
#'   - `dtm_hpre`, : previous record of dtm_h 
#'   - `Nauto_all`: all autonest eggs within time interval  
#'   - `Nauto_filt`: filtered autonest eggs (no flag -> F_combined = FALSE)
#' @export
#'
get_good_hand_eggcount <- function(meta, hand, from = NULL, to = NULL,
                                   hand_nest_colnames = c("Nest1", "Nest2", "Nest3", "Nest4"),
                                   Nestnumbers = 1:64, num_per_pen = 4,
                                   timezone = "Europe/Berlin",
                                   collect_min = 20,
                                   fakeegg = FALSE) {
  
  if (!inherits(meta, "data.table")) stop("`meta` must be a data.table")
  if (!inherits(hand, "data.table")) stop("`hand` must be a data.table")
  check_required_cols(meta, c("Date", "pen", "Nestnumber", "Eggsignal", "Transponder", "End", "F_combined", "datelay"))
  check_required_cols(hand, c("Date", "Time_end", "Pen", "Nfloor", hand_nest_colnames))
  check_date(hand$Date)
  check_time(hand$Time_end)
  check_date(from)
  check_date(to)
  
  Nestnumbers <- as.numeric(Nestnumbers)
  minnest <- min(Nestnumbers)
  maxnest <- max(Nestnumbers)
  collect_min <- as.numeric(collect_min)
  
  # prepare hand
  time_cols <- c("Time_end")
  hand[, (time_cols) := lapply(.SD, as_hms), .SDcols = time_cols]
  hand[, Date := as.Date(Date, format = "%Y-%m-%d")]
  hand[, Time_start := NULL]
  
  # Subset meta & hand#NULL Subset meta & hand
  if (!is.null(from)) {
    cat("subsetting data: Date >= ", as.character(from), "\n")
    meta <- meta[Date >= as.Date(from)]
    hand <- hand[Date >= from]
  }
  if (!is.null(to)) {
    cat("subsetting data: Date <= ", as.character(to), "\n")
    meta <- meta[Date <= as.Date(to)]
    hand <- hand[Date <= to]
  }
  
  # Check hand-count data
  all_dates <- seq(min(hand$Date), max(hand$Date), by = "days")
  missd <- all_dates[!all_dates %in% hand$Date]
  if (length(missd) > 0) warning("Missing dates in hand: ", paste0(missd, ","))
  missd_hand <- missd
  
  # Hand counting reshaped 
  hand_long <- melt(hand[, c("Date", "Time_end", "Pen", hand_nest_colnames), with = FALSE],
                    id.vars = c("Date", "Pen", "Time_end"),
                    variable.name = "Nest", value.name = "Nhand")
  hand_long[, Nest := as.numeric(sub("Nest", "", Nest))]
  hand_long[, Nest := (Pen - 1) * num_per_pen + Nest]
  hand_long <- merge(hand_long, hand[, .(Date, Pen, Nfloor)],
                     by = c("Date", "Pen"), all = TRUE)

  # Input missing hand dates
  if (length(missd_hand) > 0) {
    tmp <- data.table(Date = missd_hand, 
                      Pen = max(hand_long$Pen),
                      Time_end = hms::as_hms(median(as.numeric(hand_long$Time_end), na.rm = TRUE)),
                      Nest = max(hand_long$Nest),
                      Nhand = 0, Nfloor = 0)
    hand_long <- rbind(hand_long, tmp, fill = TRUE)
    setorder(hand_long, Date)
  }
  
  
  # Prepare fake egg time 
  if (isTRUE(fakeegg)) {
  markid <- meta[Animalmark == 0 & Eggsignal < 1, .N, by = Transponder][
    order(-N)][N > 2 & !grepl("^0[ 0]*$", Transponder), Transponder][1]
  } else markid <- NA
  
  if (!is.na(markid)) {
    fakeegg <- meta[Transponder == markid, .(End), by = .(Date, pen, Nestnumber)]
    fakeegg <- fakeegg[, .SD[.N], by = .(Date, Nestnumber)]  # Avoid dups, get last record per nest/date
    
    fegg_dates <- sort(unique(fakeegg$Date))
    all_dates <- seq(min(hand$Date), max(hand$Date), by = "days")
    missd <- all_dates[!all_dates %in% fegg_dates]
    if (length(missd) != 0) warning("Dates missing in fakeegg: ", paste0(missd, collapse = ", "))
    missd_fe <- missd
    
    full_grid <- CJ(Date = all_dates, Nestnumber = Nestnumbers, unique = TRUE)
    fakeegg <- merge(full_grid, fakeegg, by = c("Date", "Nestnumber"), all.x = TRUE)
    fakeegg[, pen := NULL]
    fakeegg <- Make_pen_number(fakeegg, Nestnumbers, num_per_pen, verbose = FALSE)
    
    # Nestnumber should increase with time
    fakeegg[, End_sec := as.numeric(End)]
    fakeegg <- fakeegg[
      order(Date, End_sec),
      .SD[is.na(Nestnumber) | is.na(End_sec) |
            Nestnumber == cummax(Nestnumber)],
      by = Date
    ]
    
    # Fill in missing dates, using hand_time as the "end" time
    tmpt <- hand_long[Date %in% missd_fe, max(Time_end), by = Date]
    fakeegg[Date %in% missd & Nestnumber == max(Nestnumbers), 
            c("End_sec") := .(as.numeric(tmpt$V1))]
    
    setorder(fakeegg, Date, Nestnumber)
    global_fit <- fakeegg[, 
                          if (sum(!is.na(End_sec)) >= 2)
                            list(slope = coef(lm(End_sec ~ Nestnumber))[2])
                          else
                            list(slope = NA_real_),
                          by = Date]
    
    # Global slope = median of available slopes
    global_slope <- median(global_fit$slope, na.rm = TRUE)
    
    fakeegg[, End_filled_sec := {
      non_na <- !is.na(End_sec)
      k <- sum(non_na)
      if (k >= 2) {
        # Fit lm if records >= 2
        fit <- lm(End_sec ~ Nestnumber)
        pred <- predict(fit, newdata = .SD)
        ifelse(is.na(End_sec), pred, End_sec)
      } else if (k == 1) {
        # If only one record
        n0 <- Nestnumber[non_na]
        e0 <- End_sec[non_na]
        if (is.finite(global_slope)) {
          # Try global slope
          int <- e0 - n0 * global_slope
          pred <- global_slope * Nestnumber + int
          pred
        } else {
          # If no slope, assume time needed = collect_min
          used <- (n0 / maxnest) * collect_min * 60
          xs <- e0 - used
          xe <- e0 + (collect_min * 60 - used)
          
          # Fill only first and last Nestnumber
          tmp <- data.table(Nestnumber = Nestnumbers, tt = NA_real_)
          tmp[Nestnumber == minnest, tt := xs]
          tmp[Nestnumber == maxnest, tt := xe]
          
          fit2 <- lm(tmp$tt ~ tmp$Nestnumber)
          pred2 <- predict(fit2, newdata = tmp)
          ifelse(is.na(End_sec), pred2, End_sec)
        }
      } else {
        # Other situation
        rep(NA, .N)
      }
    }, by = Date]
    
    fakeegg[, End_filled := hms::as_hms(End_filled_sec)]
    fakeegg[, dtm_a := as.POSIXct(paste(Date, End_filled), format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Berlin")]
    fakeegg[, c("End", "End_sec", "End_filled_sec", "End_filled") := NULL]
    
    # Merge fake egg datetime
    tegg_hand <- merge(hand_long, fakeegg,
                       by.x = c("Date", "Pen", "Nest"),
                       by.y = c("Date", "pen", "Nestnumber"),
                       all = TRUE)
    
    tegg_hand[, dtm_h := as.POSIXct(paste(Date, Time_end), format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Berlin")]
    tegg_hand[!is.na(dtm_a), dtm_h := dtm_a]
  } else {
    tegg_hand <- hand_long
    tegg_hand[, dtm_h := as.POSIXct(paste(Date, Time_end), format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Berlin")]
  }
  
  setorder(tegg_hand, Pen, Nest, dtm_h)
  tegg_hand[, dtm_hpre := shift(dtm_h), by = .(Pen, Nest)]
  tegg_hand[is.na(dtm_hpre),
            dtm_hpre := as.POSIXct(paste(Date - 1, "18:00:00"), tz = timezone)]
  tegg_hand[, Time_end := NULL]
  
  # Merge hand & autoNest eggs Count
  tegg_m <- data.table::copy(tegg_hand)
  tegg_m[, Nauto_all :=
           meta[Eggsignal > 0 & pen == Pen & Nestnumber == Nest &
                  datelay >= dtm_hpre & datelay <= dtm_h,
                .N],
         by = .(Pen, Nest, dtm_hpre, dtm_h)]
  
  tegg_m[, Nauto_filt :=
           meta[Eggsignal > 0 & pen == Pen & Nestnumber == Nest &
                  datelay >= dtm_hpre & datelay <= dtm_h &
                  F_combined == FALSE,
                .N],
         by = .(Pen, Nest, dtm_hpre, dtm_h)]
  
  return(tegg_m)
}


#' Optimize time offset based on AutoNest and hand-count egg matches
#'
#' @description Tests a range of time offsets to identify the optimal offset that
#' maximizes matches between automatic nest records and manually collected hand-count data.
#'
#' @param meta_ori A `data.table` objects containing raw AutoNest data before prep_data().
#' @param hand `data.table` containing hand-count egg data, required columns: `Date`(format yyyy-mm-dd), 
#' `Time_end`(format hh:mm:ss), `pen`, `Nfloor`, and columns included in param `hand_nest_colnames`
#' @param from,to `Date` or `character(1)`, optional date range (format yyyy-mm-dd.
#' @param hand_nest_colnames Column names for nests per pen (default: `Nest1, Nest2, Nest3, Nest4`).
#' @param Nestnumbers Sequence of valid nest numbers (default: `1:64`).
#' @param num_per_pen Number of nests per pen (default: 4).
#' @param ot_min_range Range of minute offsets to test (default: `seq(-60, 60, by = 20)`).
#' @param timezone Time zone used for datetime conversion (default: `"Europe/Berlin"`).
#' @param collect_min Total time needed for egg collection (default: 20).
#' @param fakeegg TRUE/FALSE, if manual transponder for precise time record is available.
#'
#' @return A `data.table` with tested offsets and their match proportions.
#' @export
#'
find_best_ot_min <- function(meta_ori, hand, from = NULL, to = NULL,
                             hand_nest_colnames = c("Nest1", "Nest2", "Nest3", "Nest4"),
                             Nestnumbers = 1:64,
                             num_per_pen = 4,
                             ot_min_range = seq(-60, 60, by = 20),
                             timezone = "Europe/Berlin",
                             collect_min = 20,
                             fakeegg = FALSE) {
  
  best_fit <- data.table(ot_min = numeric(), prop_match = numeric())
  
  for (ot in ot_min_range) {
    cat("\nTesting ot_min =", ot, "minutes\n")
    
    # Format time, date, etc.
    meta <- data.table::copy(meta_ori)
    meta <- prep_data(meta, ot_min = ot)
    meta <- Make_pen_number(meta, Nestnumbers, num_per_pen, verbose = FALSE)
    meta <- meta[!is.na(pen)]
    
    meta <- Add_filter_flag(meta,
                            start_time = 4, end_time = 21,
                            thrd_laydiff = 22, timezone = timezone,
                            select_flags = 1:5,
                            verbose = FALSE)
    
    tegg_m <- get_good_hand_eggcount(meta, hand, from, to,
                                     hand_nest_colnames,
                                     Nestnumbers, num_per_pen,
                                     timezone, collect_min, fakeegg = fakeegg)
    tegg_m <- tegg_m[Pen != 16]
    tegg_m[, diff_ah := Nauto_filt - Nhand]
    prop_match <- sum(tegg_m$diff_ah == 0, na.rm = TRUE) / nrow(tegg_m)
    
    best_fit <- rbind(best_fit, data.table(ot_min = ot, prop_match = prop_match))
  }
  
  best_fit <- best_fit[order(-prop_match)]
  best_ot <- best_fit$ot_min[1]
  cat("\n✅ Best ot_min =", best_ot, 
      "min with match proportion =", round(best_fit$prop_match[1], 3), "\n")
  
  return(best_fit)
}


#' Add filter flags to egg-laying records
#'
#' Computes multiple quality/control flags on egg-laying data including late laying times,
#' duplicate records, short laying intervals, and secondary eggs that were never primary.
#'
#' @param meta A `data.table` containing egg-laying data.
#' @param start_time Numeric hour indicating start of normal laying window (default 4).
#' @param end_time Numeric hour indicating end of normal laying window (default 21).
#' @param thrd_laydiff Numeric threshold (hours) for short laying interval (default 22).
#' @param timezone Character string specifying timezone (default `"Europe/Berlin"`).
#' @param select_flags Integer vector indicating which flags to combine (default 1:5).
#' @param verbose Logical, if `TRUE` prints summary statistics (default TRUE).
#'
#' @return A `data.table` with added flag columns.
#' @export
#'
Add_filter_flag <- function(meta, 
                            start_time = 4, end_time = 21, 
                            thrd_laydiff = 22, timezone = "Europe/Berlin",
                            select_flags = c(1:5),
                            verbose = TRUE) {
  
  if (!inherits(meta, "data.table")) stop("`meta` must be a data.table")
  if (!is.numeric(start_time) || !is.numeric(end_time)) stop("`start_time` and `end_time` must be numeric hours")
  if (!is.numeric(thrd_laydiff) || thrd_laydiff <= 0) stop("`thrd_laydiff` must be a positive number")
  if (!is.character(timezone)) stop("`timezone` must be a character string")
  if (!is.numeric(select_flags) || any(select_flags < 1 | select_flags > 7)) stop("`select_flags` must be integer(s) between 1 and 7")
  
  setnames(meta, gsub(" ", "", names(meta)))
  check_required_cols(meta, c("ani", "Eggsignal", "Layingtime", "Date", "type"))
  
  fcols <- c("F1_noAni", "F2_latelay", "F3_ani_cbd_dup", "F4_laydiff_short", "Fx_laydiff_short_pri",
             "F5_noPri", "F6_onlyonce")
  
  if (verbose) {
    cat("Flag columns:", paste(c(1:length(fcols)), "->", fcols),
        "\nSelected flags to combine:", fcols[select_flags], "\n")
  }
  
  # 1 (flag all) no animal mark
  meta[, F1_noAni := ani == 0]
  
  # 2 (flag only eggs) laying time between 21-4
  meta[Eggsignal > 0, F2_latelay := (Layingtime > end_time * 3600 | Layingtime < start_time * 3600)]
  
  # 3 (flag only eggs) with >1 egg count per day
  meta[Eggsignal > 0, ani_cbd := .N, by = .(Date, ani, type)]
  meta[, F3_ani_cbd_dup := ani_cbd > 1]
  
  # 4 (flag only "sec" eggs) with laying time diff < thrd
  thrd <- thrd_laydiff
  meta[, datelay := as.POSIXct(paste(Date, Layingtime), format = "%Y-%m-%d %H:%M:%S", tz = timezone)]
  setorder(meta, ani, datelay)
  
  meta[Eggsignal > 0,
       laydiffh_pre_all := as.numeric((datelay - shift(datelay, type = "lag")), units = "hours"),
       by = ani]
  
  meta[Eggsignal > 0,
       laydiffh_nxt_all := as.numeric(abs(datelay - shift(datelay, type = "lead")), units = "hours"),
       by = ani]
  
  meta[Eggsignal > 0 & type == "sec",
       F4_laydiff_short := (laydiffh_pre_all < thrd | laydiffh_nxt_all < thrd)]
  
  meta[Eggsignal > 0 & type == "pri",
       laydiffh_pre_pri := as.numeric((datelay - shift(datelay, type = "lag")), units = "hours"),
       by = ani]
  
  meta[Eggsignal > 0 & type == "pri",
       laydiffh_nxt_pri := as.numeric(abs(datelay - shift(datelay, type = "lead")), units = "hours"),
       by = ani]
  
  meta[Eggsignal > 0 & type == "pri",
       Fx_laydiff_short_pri := (laydiffh_pre_pri < thrd | laydiffh_nxt_pri < thrd)]
  
  # 5 (flag only "sec" eggs) that were never "primary"
  pri_c <- meta[type == "pri" & Eggsignal > 0, .N, by = ani]
  meta[type == "sec" & Eggsignal > 0,
       pri_count := pri_c[.SD, on = .(ani), x.N]]
  meta[type == "sec" & Eggsignal > 0 & is.na(pri_count), pri_count := 0]
  meta[type == "sec" & Eggsignal > 0, F5_noPri := pri_count == 0]
  
  # 6 (flag only eggs) if never laid the 2nd egg
  all_c <- meta[Eggsignal > 0, .N, by = ani]
  meta[type == "sec" & Eggsignal > 0,
       all_count := all_c[.SD, on = .(ani), x.N]]
  meta[Eggsignal > 0, F6_onlyonce := all_count < 2]
  
  if (verbose) {
    cat("\nTable of Eggsignal > 0 and without animal mark (= 0): \n")
    print(table(meta[Eggsignal > 0, F1_noAni], useNA = "ifany"))
    
    cat("\nTable of eggs laid from", end_time, ":00 to", start_time, ":00 :\n")
    print(table(meta[Eggsignal > 0, F2_latelay], useNA = "ifany"))
    
    cat("\nTable of animal records with >1 egg on the day: \n")
    print(table(meta$F3_ani_cbd_dup, useNA = "ifany"))
    
    cat("\nTable of egg records with laying time difference <", thrd_laydiff, "hours: \n")
    print(table(meta$F4_laydiff_short, useNA = "ifany"))
    
    cat("\nTable of PRIMARY egg records with laying time difference <", thrd_laydiff, "hours: \n")
    print(table(meta$Fx_laydiff_short_pri, useNA = "ifany"))
    
    cat("\nTable of Secondary eggs that were never primary: \n")
    print(table(meta$F5_noPri, useNA = "ifany"))
    
    cat("\nTable of Secondary eggs that never lay the 2nd egg: \n")
    print(table(meta[, .(F6_onlyonce, type)], useNA = "ifany"))
  }
  
  # A flag combining all flags
  fcols <- fcols[select_flags]
  tmp <- rowSums(meta[, ..fcols], na.rm = TRUE) > 0
  meta[, F_combined := ..tmp]
  
  if (verbose) {
    cat("\nCombined selected flags into col F_combined. \n")
  }
  
  return(meta)
}


#' Identify trusted egg-animal pairs from autonest data
#'
#' @description Filters autonest data to identify "trusted" candidate animals for each egg
#' within a given datetime range.
#'
#' @param eggs `data.table` with columns: `Nest`, `eggid`.
#' @param pen_meta `data.table` with autonest metadata.
#' @param from POSIXct start datetime.
#' @param to POSIXct end datetime.
#'
#' @return `data.table` with columns: `eid`, `ani`, `layingtime`, `type`.
#' @export
#'
get_trusted_autonest <- function(eggs, pen_meta, from, to) {
  if (!inherits(pen_meta, "data.table")) stop("`pen_meta` must be a data.table")
  if (!inherits(eggs, "data.table")) stop("`eggs` must be a data.table")
  
  check_datetime_range(from, to)
  check_required_cols(eggs, c("Nest", "eggid"))
  check_required_cols(pen_meta, 
                      c("datelay", "Eggsignal", "F_combined", "ani", "type", "pri_count", "laydiffh_pre_pri", "Nestnumber"))
  
  check_datetime(pen_meta$datelay)
  
  if (!identical(attr(from, "tzone"), attr(pen_meta$datelay, "tzone"))) {
    warning("'from' and 'pen_meta$datelay' do NOT have the same timezone. 
         Current: from = ", attr(from, "tzone"), 
            ", pen_meta$datelay = ", attr(to, "tzone"))
  }
  if (!identical(attr(to, "tzone"), attr(pen_meta$datelay, "tzone"))) {
    warning("'to' and 'pen_meta$datelay' do NOT have the same timezone. 
         Current: to = ", attr(from, "tzone"), 
            ", pen_meta$datelay = ", attr(to, "tzone"))
  }
  
  autofilt_ani <- pen_meta[
    datelay >= from & datelay <= to &
      Eggsignal > 0 & F_combined != TRUE,
    .(ani, type, pri_count, laydiffh_pre_pri, Date, Layingtime),
    by = Nestnumber
  ]
  
  nestorder <- autofilt_ani[, .N, by = .(Nestnumber, type)][
    order(type, N), unique(Nestnumber)]
  
  ani_id <- c()
  eid <- c()
  laytime <- c()
  type <- c()
  
  for (n in nestorder) {
    neggs <- nrow(eggs[Nest == n])
    nani <- nrow(autofilt_ani[Nestnumber == n])
    
    if (neggs < nani) {
      pri_anis <- autofilt_ani[Nestnumber == n & type == "pri", ani]
      npri <- length(pri_anis)
      
      sec_quota <- max(0, neggs - npri)
      sec_ani_keep <- autofilt_ani[Nestnumber == n & type == "sec"][order(-pri_count)][seq_len(sec_quota), ani]
      
      if (neggs < npri) {
        pri_anis <- autofilt_ani[ani %in% pri_anis][order(laydiffh_pre_pri)][seq_len(neggs), ani]
      }
      ani_id <- c(ani_id, pri_anis, sec_ani_keep)
      eid <- c(eid, eggs[Nest == n, eggid])
      laytime <- c(laytime, autofilt_ani[ani %in% pri_anis, Layingtime])
      type <- c(type, autofilt_ani[ani %in% pri_anis, type])
      
    } else if (neggs > nani) {
      ani_id <- c(ani_id, autofilt_ani[Nestnumber == n, ani])
      eid <- c(eid, eggs[Nest == n, eggid][seq_len(nani)])
      laytime <- c(laytime, autofilt_ani[Nestnumber == n, Layingtime])
      type <- c(type, autofilt_ani[Nestnumber == n, type])
    } else {
      ani_id <- c(ani_id, autofilt_ani[Nestnumber == n, ani])
      eid <- c(eid, eggs[Nest == n, eggid])
      laytime <- c(laytime, autofilt_ani[Nestnumber == n, Layingtime])
      type <- c(type, autofilt_ani[Nestnumber == n, type])
    }
    
    # Remove selected ani
    autofilt_ani <- autofilt_ani[!ani %in% ani_id]
  }
  
  trusted <- data.table(eid = eid, ani = ani_id, layingtime = hms::as_hms(laytime), type = type)
  return(trusted)
}