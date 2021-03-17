#' Cleans a member's gender
#'
#' @param df a tibble of demographic data
#'
#' @return a tibble with cleaned column Gender
#' @export
#'
#' @examples #demo_raw <- step_gender(df = demo_raw)
step_gender <- function(df) {

  df %>%
    mutate(
      Gender =
        recode_factor(
          .data$Gender,
          "0" = "NA",
          "#N/A" = "NA"
        )
    ) %>%
    return()

}
