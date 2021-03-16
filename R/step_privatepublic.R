step_privatepublic <- function(df) {

  df %>%
    mutate(
      Priv_Publ =
        case_when(
          Control == "Private for-profit" ~ "Private (Not-For-Profit)",
          Control == "Private not-for-profit" ~ 'Private (For-Profit)',
          Control == "Public not-for-profit" ~ "Public",
          Control == "public not-for-profit" ~ 'Public',
          Control == 'International' ~ 'International',
          (School_Country != 'United States' &
             School_Country != 'NA') ~ 'International'
        )
    ) %>%
    mutate(Priv_Publ =
             factor(Priv_Publ)) %>%
    return()

}
