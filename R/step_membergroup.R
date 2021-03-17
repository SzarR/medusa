#' Cleans a member's categorization as individual or prospect
#'
#' @param df a tibble of demographic data
#'
#' @return a tibble with cleaned column MemberGroup
#' @export
#'
#' @examples #demo_raw <- step_membergroup(df = demo_raw)
step_membergroup <- function(df) {

  df %>%
    mutate(
      MemberGroup =
        recode_factor(
          .data$MemberGroup,
          "Individual" = "Individual",
          "INDIVIDUAL" = "Individual",
          "PROSPECT" = "Prospect"
        )
    ) %>%
    return()

}
