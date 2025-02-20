#' Calculate Gestational Age (GA) in Numeric Format
#'
#' Converts a vector of gestational age strings from "weeks+days" (e.g., "23+5") into numeric format
#' as "weeks + days/7".
#'
#' @param ga_strings A character vector of gestational ages in "weeks+days" format
#' (e.g., c("23+5", "30+2")).
#'
#' @return A numeric vector where each element represents gestational age as "weeks + days/7".
#'
#' @details
#' This function parses each input string, splits the weeks and days, and calculates the numeric
#' value as weeks plus the fraction of days over 7. It ensures that days are between 0 and 6.
#'
#' @examples
#' metricGA("23+5") # Returns 23.71
#' metricGA("40") #Returns 40
#' metricGA(c("23+5", "30+2", "18+0"))
#' # Returns: 23.71429, 30.28571, 18.00000
#'
#' @export
metricGA <- function(ga_strings) {
  # Apply the calculation to each element in the vector
  ga_numeric <-
  sapply(ga_strings, function(ga_string) {
    # Split the input string into weeks and days
    parts <- strsplit(ga_string, "\\+")[[1]]

    # Convert the parts into numeric values
    weeks <- as.numeric(parts[1])
    days <- as.numeric(parts[2])
    if(is.na(days)) days=0

    # Validate input
    if (is.na(weeks) || is.na(days) || days < 0 || days > 6) {
      stop("Invalid input format or days out of range (0-6). Use 'weeks+days', e.g., '23+5'.")
    }

    # Calculate numeric GA
    ga_num <- as.vector(weeks + days / 7)
    return(ga_num)
  })
  ga_numeric <- as.numeric(as.vector(ga_numeric))
  return(ga_numeric)
}
