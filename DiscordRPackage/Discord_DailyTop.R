#' @title Discord_DailyTop
#' @name Discord_DailyTop
#' @description Generates bar chart showing how many messages the top user sent each day.
#' @param df Message History Dataframe
#' @param datetime_col Datetime Col (Should be cleaned to proper format)
#' @param user_col Message Authors ("Author" by default)
#' @import dplyr
#' @import ggplot2
#' @export
#' @examples
#' Discord_DailyTop(df)
#'


Discord_DailyTop <- function(df, datetime_col = "Timestamp_clean", user_col = "Author") {

  # Check columns exist
  if (!datetime_col %in% names(df) | !user_col %in% names(df)) {
    stop("Datetime or Username column not found in the data frame.")
  }

  # Convert to date
  df <- df %>%
    mutate(Date = as.Date(.data[[datetime_col]]))

  # Count messages per user per day
  daily_counts <- df %>%
    group_by(Date, .data[[user_col]]) %>%
    summarise(DailyMessages = n(), .groups = "drop")

  # Identify top sender for each day
  top_senders <- daily_counts %>%
    group_by(Date) %>%
    slice_max(order_by = DailyMessages, n = 1, with_ties = FALSE) %>%
    ungroup()

  # Plot top sender per day
  ggplot(top_senders, aes(x = Date, y = DailyMessages, fill = .data[[user_col]])) +
    geom_col() +
    labs(
      title = "Top Message Sender per Day",
      x = "Date",
      y = "Number of Messages",
      fill = "Username"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")
}


