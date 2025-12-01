#' @title Discord_TopUsersCumulative
#' @name Discord_TopUsersCumulative
#' @description Generates a time series plot visualization of the top users, showing how their overall message account increases over time.
#' @param df Message History Dataframe
#' @param datetime_col Datetime Col (Should be cleaned to proper format)
#' @param user_col Message Authors ("Author" by default)
#' @param points Boolean, whether points will display on the chart
#' @param top_n Number of users to include
#' @import dplyr
#' @import ggplot2
#' @export
#' @examples
#' Discord_TopUsersCumulative(df)
#'


Discord_TopUsersCumulative <- function(df, datetime_col = "Timestamp_clean", user_col = "Author", top_n = 10, points = TRUE) {

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
  df_agg <- df_top %>%
    group_by(.data[[user_col]], Date) %>%
    summarise(DailyMessages = n(), .groups = "drop") %>%
    arrange(.data[[user_col]], Date) %>%
    group_by(.data[[user_col]]) %>%
    mutate(CumulativeMessages = cumsum(DailyMessages)) %>%
    ungroup()

  # Plot cumulative messages over time
  p <- ggplot(df_agg, aes(x = Date, y = CumulativeMessages, color = .data[[user_col]])) +
    geom_line(linewidth = 1.2) +
    labs(
      title = paste("Cumulative Messages Over Time - Top", top_n, "Users"),
      x = "Date",
      y = "Cumulative Messages",
      color = "Username"
    ) +
    theme_dark(base_size = 14) +
    theme(legend.position = "bottom")

  if (points) {
    p <- p + geom_point(size = 1.5, alpha = 0.7)
  }

  return(p)
}
