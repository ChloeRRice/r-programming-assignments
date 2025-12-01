#' @title Discord_HeatMap
#' @name Discord_Heatmap
#' @description Generates a Heat Map of the Day x Hour messaging activity
#' @param df Message Dataframe
#' @param datetime_col Datetime Col (Should be cleaned to proper format)
#' @import dplyr
#' @import ggplot2
#' @export
#' @examples
#' Discord_HeatMap(df)
#'

Discord_HeatMap <- function(df, datetime_col = "Timestamp_clean") {

  # Check column exists
  if (!datetime_col %in% names(df)) {
    stop("Datetime column not found in the data frame.")
  }

  # Extract Day of Week and Hour
  df <- df %>%
    mutate(
      DayOfWeek = weekdays(as.Date(.data[[datetime_col]])),  # Monday, Tuesday, etc.
      Hour = as.integer(format(.data[[datetime_col]], "%H"))
    )

  # Count messages per Day x Hour
  heatmap_data <- df %>%
    group_by(DayOfWeek, Hour) %>%
    summarise(Messages = n(), .groups = "drop")

  # Order days of the week
  day_levels <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  heatmap_data$DayOfWeek <- factor(heatmap_data$DayOfWeek, levels = day_levels)

  # Plot heatmap
  ggplot(heatmap_data, aes(x = Hour, y = DayOfWeek, fill = Messages)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "red") +
    labs(
      title = "Messages by Day of Week and Hour",
      x = "Hour of Day",
      y = "Day of Week",
      fill = "Messages"
    ) +
    theme_dark(base_size = 14) +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5))
}

