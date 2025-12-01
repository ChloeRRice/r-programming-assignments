#' @title Discord_TopDays
#' @name Discord_TopDays
#' @description Generates a time series showing activity levels each day.
#' @param df Message History Dataframe
#' @param datetime_col Datetime Column (Should be cleaned to proper format)
#' @import dplyr
#' @import ggplot2
#' @export
#' @examples
#' Discord_TopDays(df)

Discord_TopDays <- function(df, datetime_col = "Timestamp_clean") {

  # Ensure the column exists
  if (!datetime_col %in% names(df)) {
    stop("Datetime column not found in the data frame.")
  }

  # Convert to Date (drops time-of-day)
  df$Date <- as.Date(df[[datetime_col]])

  # Count messages per day
  daily_counts <- df %>%
    count(Date, name = "Messages")

  # Plot
  ggplot(daily_counts, aes(x = Date, y = Messages)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(
      title = "Messages Sent Per Day",
      x = "Date",
      y = "Number of Messages"
    ) +
    theme_dark(base_size = 14)
}
