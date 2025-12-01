#' @title Discord_TopUsersActivity
#' @name Discord_TopUsersActivity
#' @description Identifies the most active users and tracks the number of messages per day throught the time period using a Time Series.
#' @param df Message History Dataframe
#' @param datetime_col Datetime Column (Should be cleaned to proper format)
#' @param user_col Message Authors ("Author" by default)
#' @param smooth Boolean, whether to add geom_smooth
#' @param top_n Number of users to include
#' @import dplyr
#' @import ggplot2
#' @export
#' @examples
#' Discord_TopUsersActivity(df)
#'

Discord_TopUserActivity <- function(df, datetime_col = "Timestamp_clean", user_col = "Author", top_n = 5, smooth = FALSE) {

  # Check columns exist
  if (!datetime_col %in% names(df) | !user_col %in% names(df)) {
    stop("Datetime or Username column not found in the data frame.")
  }

  # Identify top N users
  top_users <- df %>%
    count(.data[[user_col]], name = "TotalMessages") %>%
    arrange(desc(TotalMessages)) %>%
    slice_head(n = top_n) %>%
    pull(.data[[user_col]])

  # Filter for top users
  df_top <- df %>%
    filter(.data[[user_col]] %in% top_users) %>%
    mutate(Date = as.Date(.data[[datetime_col]]))

  # Aggregate messages per user per day
  df_daily <- df_top %>%
    group_by(.data[[user_col]], Date) %>%
    summarise(DailyMessages = n(), .groups = "drop")

  # Plot daily messages
  p <- ggplot(df_daily, aes(x = Date, y = DailyMessages, color = .data[[user_col]])) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 2, alpha = 0.7) +
    labs(
      title = paste("Daily Messages Over Time - Top", top_n, "Users"),
      x = "Date",
      y = "Daily Messages",
      color = "Username"
    ) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom")

  # Optional smoothing
  if (smooth) {
    p <- p + geom_smooth(se = FALSE, method = "loess", linewidth = 0.8, linetype = "dashed")
  }

  return(p)
}
