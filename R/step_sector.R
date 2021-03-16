step_sector <- function(df) {

  df <-
    df %>%
    unite(col = "Primary_Career",
          contains("Primary"),
          remove = FALSE,
          na.rm = TRUE) %>%
    mutate(Academy = ifelse(is.na(`Academic Sector (Primary)`), 0, 'Academy'),
           Govern = ifelse(is.na(`Government Sector (Primary)`), 0, 'Govern'),
           Private = ifelse(is.na(`Private Sector (Primary)`), 0, 'Private'),
           Other = ifelse(is.na(`Other Sector (Primary)`), 0, 'Other'),
           NotProv = ifelse(is.na(`Not Provided (Primary)`), 0, 'NotProv')
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
