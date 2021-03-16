step_profitnon <- function(df) {

  df %>%
    mutate(
      Profit_Non =
        case_when(
          Control == "Private for-profit" ~ 'Profit',
          Control == "Private not-for-profit" ~ 'Non-Profit',
          Control == "Public not-for-profit" ~ 'Non-Profit',
          Control == "public not-for-profit" ~ 'Non-Profit',
          Control == 'International' ~ 'International',
          (School_Country != 'United States' &
             School_Country != 'NA') ~ 'International'
        )
    ) %>%
    mutate(Profit_Non =
             factor(Profit_Non)) %>%
    select(-Control) %>%
    return()

}
