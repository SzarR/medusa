get_conf_attendance <- function(df, merge_demo_from, year) {

  df <-
    df %>%
    select(SID,
           Category,
           Description,
           Canceled) %>%
    filter(Canceled == 0) %>%
    mutate(
      Year = as.integer(stringr::str_extract(Category, "\\d+")),
      Conference = case_when(
        str_detect(string = Description,
                   pattern = paste(year, "Annual")) ~ "Annual",
        str_detect(string = Description,
                   pattern = "Spouse/Guest") ~ "Annual",
        str_detect(string = Description,
                   pattern = paste("SIOP",year, "Complimentary")) ~ "Annual",
        str_detect(string = Description,
                   pattern = "Exhibit Hall Only Registration") ~ "Annual",
        str_detect(string = Description,
                   pattern = "AC") ~ "Annual",
        TRUE ~ "Other"
      )
    ) %>%
    filter(Year == year & Conference == 'Annual') %>%
    group_by(SID) %>%
    slice_max(n = 1, with_ties = FALSE, order_by = SID)


  merge_demo_from %>%
    right_join(df, by = "SID") %>%
    select(
      SID,
      Membership_Dues,
      MD_Number,
      City,
      State_US,
      Country,
      Profit_Non,
      Field,
      AgeGroup,
      Gender,
      Ethnicity,
      Birthdate,
      Highest_Degree,
    ) %>%
    mutate(Conference = paste(year, "Annual Conference")) %>%
    return()

}
