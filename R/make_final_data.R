#' A function wrapper that takes the cleaned dues and demo data and merges it,
#' and cleans by SID, and finalizes a cleaned dataset ready for further analysis.
#'
#' @param demo a tibble of demo data. The output from make_demo_data()
#' @param dues a tibble of dues data. The output from make_dues_data()
#'
#' @return a tibble of SIOP members for the current year.
#' @export
#'
#' @examples #final_data <- make_final_data(demo = demo, dues = dues, staff = staff)
make_final_data <- function(demo, dues, staff) {

  df <-
    left_join(x = dues,
              y = demo,
              by = "SID")

  # update Academic_Applied based on Membership_Dues
  df <-
    df %>%
    mutate(
      .data$Academic_Applied = case_when(
        .data$Membership_Dues == "Student Affiliate" ~ "Student",
        TRUE ~ .data$Academic_Applied
      )
    )

  df <- step_sid(df,
                 staff = staff)

  df <-
    df %>%
    select(
      .data$SID,
      .data$City,
      .data$State_US,
      .data$Country,
      .data$ZipCode,
      .data$Membership_Dues,
      .data$MD_Number,
      .data$MemberGroup,
      .data$Primary_Sector,
      .data$Primary_Occupation,
      .data$Job_Type,
      .data$Academic_Applied,
      .data$Student_Prof,
      .data$School,
      .data$School_Country,
      .data$Highest_Degree,
      .data$Priv_Publ,
      .data$Field,
      .data$Profit_Non,
      .data$Gender,
      .data$Birthdate,
      .data$Ethnicity,
      .data$Age,
      .data$AgeGroup,
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
      .data$licensed,
      .data$inv_share_knowledge,
      .data$inv_collaborate_on_projects,
      .data$inv_conduct_research,
      .data$inv_join_interdisciplinary_teams,
      .data$inv_other,
      .data$inv_present_at_meetings_conferences,
      .data$inv_join_communities_of_interest,
      .data$inv_network,
      .data$inv_consult_to_business


    ) %>%
    return()
}
