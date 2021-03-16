make_demo_data <- function(df,...) {

  # Geography Cleaning Stage
  df <- step_city(df)
  df <- step_state(df)
  df <- step_country(df)

  # Membership Cleaning Stage
  df <- step_sector(df)
  df <- step_membergroup(df)
  df <- step_academicapplied(df)

  # Schools db merge
  df <-
    df %>%
    mutate(School = `Highest Degree Institution`,
           School_Country = `Institution Country`) %>%
    left_join(y = schools_db,
              by = 'School')

  # Education Cleaning Stage
  df <- step_highestdegree(df)
  df <- step_privatepublic(df)
  df <- step_field(df)
  df <- step_profitnon(df)

  # Demographics Cleaning Stage
  df <- step_gender(df)
  df <- step_birthdate(df)
  df <- step_ethnicity(df, ...)
  df <- step_agegroup(df)

  df <-
    df %>%
    select(
      SID,
      City,
      State_US,
      Country,
      AcademicApplied,
      MemberGroup,
      School,
      School_Country,
      Priv_Publ,
      Primary_Career,
      Profit_Non,
      Field,
      AgeGroup,
      Age,
      Gender,
      Ethnicity,
      Birthdate,
      Highest_Degree
    )

  return(df)
}
