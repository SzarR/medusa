make_final_data <- function(demo, dues) {

  df <-
    left_join(x = dues,
              y = demo,
              by = "SID")

  df <- step_sid(df)

  df <-
    df %>%
    select(
      SID,
      City,
      State_US,
      Country,
      MemberGroup,
      Membership_Dues,
      MD_Number,
      School,
      School_Country,
      Highest_Degree,
      Priv_Publ,
      AcademicApplied,
      Field,
      Profit_Non,
      Gender,
      Birthdate,
      Ethnicity,
      Age,
      AgeGroup,
      Primary_Career
    ) %>%
    return()
}
