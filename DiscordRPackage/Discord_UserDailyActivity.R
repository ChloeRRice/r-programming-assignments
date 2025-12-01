Discord_UserDailyActivity <- function(df, datetime_col = "Timestamp_clean", user_col = "Author", top_n = 10) {

  # Check columns exist
  if (!datetime_col %in% names(df) | !user_col %in% names(df)) {
    stop("Datetime or Username column not found in the data frame.")
  }

  # Extract day of week
  df <- df %>%
    mutate(
      DayOfWeek = weekdays(as.Date(.data[[datetime_col]]))
    )

  # Identify top N users overall
  top_users <- df %>%
    count(.data[[user_col]], name = "TotalMessages") %>%
    arrange(desc(TotalMessages)) %>%
    slice_head(n = top_n) %>%
    pull(.data[[user_col]])

  # Filter for top users
  df_top <- df %>%
    filter(.data[[user_col]] %in% top_users)

  # Count messages per user per day of week
  user_day_counts <- df_top %>%
    group_by(.data[[user_col]], DayOfWeek) %>%
    summarise(MessageCount = n(), .groups = "drop")

  # Order days of the week
  day_levels <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
  user_day_counts$DayOfWeek <- factor(user_day_counts$DayOfWeek, levels = day_levels)

  # Plot using geom_count
  ggplot(user_day_counts, aes(x = DayOfWeek, y = .data[[user_col]])) +
    geom_count(aes(size = MessageCount), color = "#4E79A7", alpha = 0.7) +
    scale_size_area(max_size = 15) +
    labs(
      title = paste("Top", top_n, "Users Activity by Day of Week"),
      x = "Day of Week",
      y = "Username",
      size = "Message Count"
    ) +
    theme_minimal(base_size = 14)
}
