#' @title Discord_Record
#' @name Discord_Record
#' @description Identifies the record amount of messages sent within the specified time period by each user, and ranks the users with the highest record.
#' @param df Message History Dataframe
#' @param user_col Author Column, "Author" by default
#' @param time_col Datetime Column, "Timestamp_Clean" by default
#' @param search_window Time period to search (In Hours)
#' @import dplyr
#' @import ggplot2
#' @export
#' @examples
#' Discord_Record(df)

Discord_Record <- function(df, datetime_col = "Timestamp_clean", user_col = "Author", search_window = 36) {

  # Check columns exist
  if (!datetime_col %in% names(df) | !user_col %in% names(df)) {
    stop("Datetime or Username column not found in the data frame.")
  }

  # Sort dataset by datetime
  df <- df %>% arrange(.data[[datetime_col]])

  # Initialize result list
  result <- list()

  # Get unique users
  users <- unique(df[[user_col]])

  for (user in users) {
    # Subset data for this user
    user_times <- df %>%
      filter(.data[[user_col]] == user) %>%
      pull(.data[[datetime_col]])

    # Initialize vector to store max counts
    max_count <- 0
    start_idx <- 1

    # Sliding window approach
    for (end_idx in seq_along(user_times)) {
      # Move start_idx forward until window is within 36 hours
      while (difftime(user_times[end_idx], user_times[start_idx], units = "hours") > search_window) {
        start_idx <- start_idx + 1
      }
      # Number of messages in current window
      count_window <- end_idx - start_idx + 1
      if (count_window > max_count) {
        max_count <- count_window
      }
    }

    result[[user]] <- max_count
  }

  # Convert to data frame and rank
  result_df <- tibble(
    Username = names(result),
    MaxMessages = unlist(result)
  ) %>%
    arrange(desc(MaxMessages)) %>%
    mutate(Rank = row_number())

  return(result_df)
}
