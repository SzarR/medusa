#' A function wrapper that serially executes all steps related to demographic
#' cleaning of raw XLSX membership data.
#'
#' @param df a tibble of demographic data in raw format
#' @param ...
#'
#' @return a cleaned tibble with a subset of relevant features
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

  # Schools database merge
  df <-
    df %>%
    mutate(School = .data$`Highest Degree Institution`,
           School_Country = .data$`Institution Country`) %>%
    left_join(y = schools_db %>% select(-City),
              by = "School")

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

  # Interests and Certifications Area
  # New area added 3/1/2022
  df <- step_interests(df)
  df <- step_other_orgs(df)
  df <- step_involvement(df)

  df <-
    df %>%
    select(
      .data$SID,
      .data$City,
      .data$State_US,
      .data$Country,
      .data$ZipCode,
      .data$Primary_Sector,
      .data$Primary_Occupation,
      .data$Job_Type,
      .data$MemberGroup,
      .data$Academic_Applied,
      .data$School,
      .data$School_Country,
      .data$Highest_Degree,
      .data$Priv_Publ,
      .data$Field,
      .data$Profit_Non,
      .data$Gender,
      .data$Birthdate,
      .data$Ethnicity,
      .data$AgeGroup,
      .data$Age,
      .data$`Graduated Month/Year*`,
      .data$`Year Started in I-O Field`,
      .data$int_1,
      .data$int_2,
      .data$int_3,
      .data$org_aff_AOM,
      .data$org_aff_APA,
      .data$org_aff_APS,
      .data$org_aff_SHRM,
      .data$org_aff_EAWOP,
      .data$org_aff_IAAP,
      .data$org_aff_CPA,
      .data$org_aff_HRCI,
      .data$org_aff_SHRM,
      .data$inv_share_knowledge,
      .data$inv_collaborate_on_projects,
      .data$inv_conduct_research,
      .data$inv_join_interdisciplinary_teams,
      .data$inv_other,
      .data$inv_present_at_meetings_conferences,
      .data$inv_join_communities_of_interest,
      .data$inv_network,
      .data$inv_consult_to_business
    )
  return(df)
}
