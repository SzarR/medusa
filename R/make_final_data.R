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
    mutate(Academic_Applied = as.character(Academic_Applied)) %>%
    mutate(
      Academic_Applied =
        case_when(
        Membership_Dues == "Student Affiliate" ~ "Student",
        TRUE ~ Academic_Applied)
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
      starts_with("org_aff_"),
      .data$licensed,
      starts_with("inv_")


    ) %>%
    return()
}
