#' Cleans a member's school categorization.
#'
#' @param df a tibble of demographic data
#'
#' @return a tibble with cleaned column Profit_Non
#' @export
#'
#' @examples #demo_raw <- step_profitnon(df = demo_raw)
step_profitnon <- function(df) {

  df %>%
    mutate(
      Profit_Non =
        case_when(
          .data$Control == "Private for-profit" ~ 'Profit',
          .data$Control == "Private not-for-profit" ~ 'Non-Profit',
          .data$Control == "Public not-for-profit" ~ 'Non-Profit',
          .data$Control == "public not-for-profit" ~ 'Non-Profit',
          .data$Control == 'International' ~ 'International',
          (.data$School_Country != 'United States' &
             .data$School_Country != 'NA') ~ 'International'
        )
    ) %>%
    mutate(Profit_Non =
             factor(.data$Profit_Non)) %>%
    select(-.data$Control) %>%
    return()

}
