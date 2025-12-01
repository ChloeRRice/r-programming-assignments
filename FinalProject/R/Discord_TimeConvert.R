#' @title Discord_TimeConvert
#' @name Discord_TimeConvert
#' @description Converts the Generated Timestamps from ISO 8601 structure with fractional seconds into proper POSIXct datetimes. Suitable for generating Time Series Analysis
#' @param df Dataframe to be cleaned
#' @param column Date Column to be chosen. "Date" by default.
#' @export
#' @examples
#' Discord_TimeConvert(df, "Date")

Discord_TimeConvert <- function(df, column = "Date") {

  # Remove fractional seconds
  cleaned <- gsub("(\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}).*", "\\1", df[[column]])

  # Convert "T" to space for standard POSIX parsing
  cleaned <- gsub("T", " ", cleaned)

  # Convert to POSIXct
  df$Timestamp_clean <- as.POSIXct(
    cleaned,
    format = "%Y-%m-%d %H:%M:%S",
    tz = "EST"
  )

  return(df)
}
