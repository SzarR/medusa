step_membergroup <- function(df) {

  df %>%
    mutate(
      MemberGroup =
        recode_factor(
          MemberGroup,
          "Individual" = "Individual",
          "INDIVIDUAL" = "Individual",
          "PROSPECT" = "Prospect"
        )
    ) %>%
    return()

}
