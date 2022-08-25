#' Categorize whether a member works in an applied or academic setting.
#'
#' @param df a tibble of demographic data
#'
#' @return a tibble
#' @export
#'
#' @examples #demo_raw <- step_academicapplied(df = demo_raw)
step_academicapplied <- function(df) {

  df %>%
    mutate(
      Academic_Applied =
        case_when(
          .data$`Academic Sector (Primary)` != "" ~ 'Academic',
          .data$`Government Sector (Primary)` != "" ~ 'Applied',
          .data$`Private Sector (Primary)` != "" ~ 'Applied',
          .data$`Other Sector (Primary)` == "Other" ~ 'Applied'
        )
    ) %>%
    mutate(
      Academic_Applied = as.factor(.data$Academic_Applied)
    ) %>%
    return()

}
