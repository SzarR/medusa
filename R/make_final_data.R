#' A function wrapper that takes the cleaned dues and demo data and merges it,
#' and cleans by SID, and finalizes a cleaned dataset ready for further analysis.
#'
#' @param demo a tibble of demo data. The output from make_demo_data()
#' @param dues a tibble of dues data. The output from make_dues_data()
#'
#' @return a tibble of SIOP members for the current year.
#' @export
#'
#' @examples #final_data <- make_final_data(demo = demo, dues = dues)
make_final_data <- function(demo, dues) {

  df <-
    left_join(x = dues,
              y = demo,
              by = "SID")

  df <- step_sid(df)

  df <-
    df %>%
    select(
      .data$SID,
      .data$City,
      .data$State_US,
      .data$Country,
      .data$MemberGroup,
      .data$Membership_Dues,
      .data$MD_Number,
      .data$School,
      .data$School_Country,
      .data$Highest_Degree,
      .data$Priv_Publ,
      .data$AcademicApplied,
      .data$Field,
      .data$Profit_Non,
      .data$Gender,
      .data$Birthdate,
      .data$Ethnicity,
      .data$Age,
      .data$AgeGroup,
      .data$Primary_Career
    ) %>%
    return()
}
