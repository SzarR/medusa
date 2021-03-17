#' A function wrapper that executes all functions related to cleaning the
#' demographic xlsx dataset.
#'
#' @param df a tibble of demographic data
#' @param ... detailed_types for ethnicity breakdown, default is FALSE
#'
#' @return a tibble with all demographic columns cleaned
#' @export
#'
#' @examples #make_demo_data(df = demo_raw)
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
    mutate(School = .data$`Highest Degree Institution`,
           School_Country = .data$`Institution Country`) %>%
    left_join(y = schools_db,
              by = .data$'School')

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
      .data$SID,
      .data$City,
      .data$State_US,
      .data$Country,
      .data$AcademicApplied,
      .data$MemberGroup,
      .data$School,
      .data$School_Country,
      .data$Priv_Publ,
      .data$Primary_Career,
      .data$Profit_Non,
      .data$Field,
      .data$AgeGroup,
      .data$Age,
      .data$Gender,
      .data$Ethnicity,
      .data$Birthdate,
      .data$Highest_Degree
    )

  return(df)
}
