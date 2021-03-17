#' Cleans a member's country of residence/location.
#'
#' @param df a tibble of demographic data
#'
#' @return a tibble with cleaned column Country
#' @export
#'
#' @examples #demo_raw <- step_country(df = demo_raw)
step_country <- function(df) {

  state_abb <- as.factor(
    c(
      "AL","AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL",
      "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT",
      "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
      "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY",
      "DC"
      #, "GU", "VI"
    )
  )

  df %>%
    mutate(Country =
             ifelse(
               .data$State_US %in% state_abb,
               "UNITED STATES",
               toupper(.data$Country)
             )
    ) %>%
    return()
}
