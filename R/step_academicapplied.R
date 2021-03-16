step_academicapplied <- function(df) {

  df %>%
    mutate(
      AcademicApplied =
        case_when(
          `Academic Sector (Primary)` != "" ~ 'Academic',
          `Government Sector (Primary)` != "" ~ 'Applied',
          `Private Sector (Primary)` != "" ~ 'Applied',
          `Other Sector (Primary)` == "Other" ~ 'Applied'
        )
    ) %>%
    mutate(
      AcademicApplied = as.factor(AcademicApplied)
    ) %>%
    return()

}
