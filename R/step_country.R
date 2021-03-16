step_country <- function(df) {

  df %>%
    mutate(Country =
             ifelse(
               State_US %in% state_abb,
               "UNITED STATES",
               toupper(Country)
             )
    ) %>%
    return()
}
