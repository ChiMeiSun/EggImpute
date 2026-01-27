#' @import data.table
#' @importFrom methods is
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