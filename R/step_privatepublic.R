#' Cleans a member's school categorization
#'
#' @param df a tibble of demographic data
#'
#' @return a tibble with cleaned column Priv_Publ
#' @export
#'
#' @examples #demo_raw <- step_privatepublic(df = demo_raw)
step_privatepublic <- function(df) {

  df %>%
    mutate(
      Priv_Publ =
        case_when(
          .data$Control == "Private for-profit" ~ "Private",
          .data$Control == "Private (For-Profit)" ~ "Private",
          .data$Control == "Private (Not-For-Profit)" ~ "Private",
          .data$Control == "Private not-for-profit" ~ 'Private',
          .data$Control == "Public not-for-profit" ~ "Public",
          .data$Control == "public not-for-profit" ~ 'Public',
          .data$Control == 'International' ~ 'International',
          (.data$School_Country != 'United States' &
             .data$School_Country != 'NA') ~ 'International'
        )
    ) %>%
    mutate(Priv_Publ =
             factor(.data$Priv_Publ)) %>%
    return()

}
