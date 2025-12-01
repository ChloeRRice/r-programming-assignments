#' Discord Visualization Function
#' @title Discord_TopUsers
#' @name Discord_TopUsers
#' @description Creates ggplot visualization of top messagers. Can control column names, and amount of users that are shown
#' @param df Message History Dataframe
#' @param user_col Author Column, "Author" by default
#' @param time_col Datetime Column, "Timestamp_Clean" by default
#' @param n Number of users to sample
#' @return Bar plot of users ranked
#' @import dplyr
#' @import ggplot2
#' @export
#' @examples
#' Discord_TopUsers(df)

Discord_TopUsers <- function(df, user_col = "Author", time_col = "Timestamp_clean", n = 10) {

  # Count messages per user
  top_users <- df %>%
    count(.data[[user_col]], name = "MessageCount") %>%
    arrange(desc(MessageCount)) %>%
    slice_head(n = n)

  # Plot
  ggplot(top_users, aes(x = reorder(.data[[user_col]], MessageCount),
                        y = MessageCount)) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste("Top", n, "Messagers"),
      x = "Username",
      y = "Number of Messages"
    ) +
    theme_dark(base_size = 14)+
    geom_text(aes(label = MessageCount), hjust = -0.2)
}
