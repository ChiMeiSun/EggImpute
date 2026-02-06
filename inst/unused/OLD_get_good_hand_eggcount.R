# This version make use of two fakeegg recorded time for two nests on the same day, 
# to compute simple linear model and calculate a slope of collection time,
# which can cause unreliable results when the recorded time interval between two close nests is long.

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
get_good_hand_eggcount <- function(meta_ori, hand_ori, from = NULL, to = NULL,
                                   hand_nest_colnames = c("Nest1", "Nest2", "Nest3", "Nest4"),
                                   Nestnumbers = 1:64, num_per_pen = 4,
                                   timezone = "Europe/Berlin",
                                   collect_min = 20,
                                   fakeegg = FALSE) {
  
  if (!inherits(meta_ori, "data.table")) stop("`meta_ori` must be a data.table")
  if (!inherits(hand_ori, "data.table")) stop("`hand_ori` must be a data.table")
  check_required_cols(meta_ori, c("Date", "pen", "Nestnumber", "Eggsignal", "Transponder", "End", "F_combined", "datelay"))
  check_required_cols(hand_ori, c("Date", "Time_end", "Pen", "Nfloor", hand_nest_colnames))
  check_date(hand_ori$Date)
  check_time(hand_ori$Time_end)
  check_date(from)
  check_date(to)
  
  Nestnumbers <- as.numeric(Nestnumbers)
  minnest <- min(Nestnumbers)
  maxnest <- max(Nestnumbers)
  collect_min <- as.numeric(collect_min)
  
  # prepare hand
  hand <- data.table::copy(hand_ori)
  
  time_cols <- c("Time_end")
  hand[, (time_cols) := lapply(.SD, as_hms), .SDcols = time_cols]
  hand[, Date := as.Date(Date, format = "%Y-%m-%d")]
  hand[, Time_start := NULL]
  
  meta <- data.table::copy(meta_ori)
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

  # Input missing hand dates with median Time_end
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
  markid <- meta[Animalmark == 0 & Eggsignal == 0, .N, by = Transponder][
    order(-N)][N > 2 & !grepl("^0[ 0]*$", Transponder), Transponder][1]
  print(sprintf("Fakeegg marker id: %s",markid))
  } else markid <- NA
  
  if (!is.na(markid)) {
    fakeegg <- meta[Transponder == markid, .(Start, End), by = .(Date, pen, Nestnumber)]
    fakeegg[, diff := difftime(End, Start)]
    if (nrow(fakeegg[diff < 0 | diff > 10]) > 0 ) warning("Fakeegg transponder last more than 10s")
    
    if (nrow(fakeegg[Start < as_hms("06:00:00") | Start > as_hms("21:00:00")]) > 0 ) {
      warning("Fakeegg transponder time < 6:00 or > 21:00")
    }
    
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
    fakeegg[, dtm_a := as.POSIXct(
      sprintf("%s %s", Date, End_filled),
      format = "%Y-%m-%d %H:%M:%S", tz = timezone)]
    fakeegg[, c("End", "End_sec", "End_filled_sec", "End_filled") := NULL]
    
    # Merge fake egg datetime
    tegg_hand <- merge(hand_long, fakeegg,
                       by.x = c("Date", "Pen", "Nest"),
                       by.y = c("Date", "pen", "Nestnumber"),
                       all = TRUE)
    
    tegg_hand[, dtm_h := as.POSIXct(
      sprintf("%s %s", Date, Time_end), 
      format = "%Y-%m-%d %H:%M:%S", tz = timezone)]
    tegg_hand[!is.na(dtm_a), dtm_h := dtm_a]
  } else {
    tegg_hand <- hand_long
    tegg_hand[, dtm_h := as.POSIXct(
      sprintf("%s %s", Date, Time_end), 
      format = "%Y-%m-%d %H:%M:%S", tz = timezone)]
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

