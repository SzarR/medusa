#' Cleans a member's primary employment sector.
#'
#' @param df a tibble of demographic data
#'
#' @return a tibble with cleaned column Primary_Career
#' @export
#'
#' @importFrom tidyr unite
#'
#' @examples #demo_raw <- step_sector(df = demo_raw)
step_sector <- function(df) {

  df <-
    df %>%
    unite(col = "Primary_Career",
          contains("Primary"),
          remove = FALSE,
          na.rm = TRUE) %>%
    mutate(Academy = ifelse(is.na(.data$`Academic Sector (Primary)`), 0, 'Academy'),
           Govern = ifelse(is.na(.data$`Government Sector (Primary)`), 0, 'Govern'),
           Private = ifelse(is.na(.data$`Private Sector (Primary)`), 0, 'Private'),
           Other = ifelse(is.na(.data$`Other Sector (Primary)`), 0, 'Other'),
           NotProv = ifelse(is.na(.data$`Not Provided (Primary)`), 0, 'NotProv')
           #        ) %>%
           # unite(col = "Primary_Value",
           #       Academy:NotProv,
           #       remove=TRUE,
           #       na.rm = FALSE) %>%
           # mutate(Primary_Value = case_when(
           #   Primary_Value == "0_0_0_0_0" ~ "NA",
           #   Primary_Value == "0_0_0_Other_0" ~ "Other",
           #   Primary_Value == "0_0_Private_0_0" ~ "Private",
           #   Primary_Value == "0_Govern_0_0_0" ~ "Government",
           #   Primary_Value == "Academy_0_0_0_0" ~ "Academy"
    )
  # ) %>%
  # select(-Primary_Value) #Code/figure this out at a later date.

  return(df)
}
