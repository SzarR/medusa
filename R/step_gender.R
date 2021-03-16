step_gender <- function(df) {

  df %>%
    mutate(
      Gender =
        recode_factor(
          Gender,
          "0" = "NA",
          "#N/A" = "NA"
        )
    ) %>%
    return()

}
