#' Convert Numeric Gestational Age (GA) to "weeks+days" Format
#'
#' Converts a numeric vector of gestational ages (e.g., "weeks + days/7")
#' back into a character vector in the "weeks+days" format.
#'
#' @param ga_numeric A numeric vector where each element represents gestational age
#' as "weeks + days/7" (e.g., c(23.71429, 30.28571)).
#'
#' @return A character vector where each element is in the "weeks+days" format
#' (e.g., c("23+5", "30+2")).
#'
#' @details
#' The function splits the numeric value into its integer weeks and the fractional days,
#' calculates the days from the fraction, and formats them as a string.
#'
#' @examples
#' stringGA(23.71429) # Returns "23+5"
#' stringGA(c(23.71429, 30.28571, 18.00000))
#' # Returns: c("23+5", "30+2", "18+0")
#'
#' @export


stringGA <- function(ga_numeric) {
  # Ensure format is valid
 if(!is.numeric(ga_numeric)) {
    stop("Invalid input: input is not numeric")
  }

  ga_strings <- sapply(ga_numeric, function(ga) {

    # Separate weeks and days
    weeks <- floor(ga)  # Get the integer part as weeks
    days <- round((ga - weeks) * 7)  # Calculate days from the fractional part

    # Ensure days are valid (0 to 6)
    if (days < 0 || days > 6) {
      stop("Invalid numeric input: days out of range (0-6).")
    }

    # Format as "weeks+days"
    ga_str <- as.character(paste0(weeks, "+", days))
    return(ga_str)
  })
  return(ga_strings)
}
