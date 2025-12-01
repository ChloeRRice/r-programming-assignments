#' @title Discord_HourDensity
#' @name Discord_HourDensity
#' @description Generates a density plot to show which times of day messages are most frequently sent.
#' @param df Message History Dataframe
#' @param datetime_col Datetime Column (Should be cleaned to proper format)
#' @import dplyr
#' @import ggplot2
#' @export
#' @examples
#' Discord_HourDensity(df)
#'


Discord_HourDensity <- function(df, datetime_col = "Timestamp_clean") {

  # Check column exists
  if (!datetime_col %in% names(df)) {
    stop("Datetime column not found in the data frame.")
  }

  # Extract fractional hour (0-24)
  df <- df %>%
    mutate(
      HourFraction = as.numeric(format(.data[[datetime_col]], "%H")) +
        as.numeric(format(.data[[datetime_col]], "%M")) / 60 +
        as.numeric(format(.data[[datetime_col]], "%S")) / 3600
    )

  # Plot smoothed density
  ggplot(df, aes(x = HourFraction)) +
    geom_density(fill = "#1DB954", alpha = 0.6, linewidth = 1) +
    scale_x_continuous(breaks = 0:23, labels = sprintf("%02d", 0:23)) +
    labs(
      title = "Smoothed Density of Messages by Time of Day",
      x = "Hour of Day",
      y = "Density"
    ) +
    theme_dark(base_size = 14)
}
