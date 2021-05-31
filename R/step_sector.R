#' Cleans a member's primary employment sector.
#'
#' @param df a tibble of demographic data
#'
#' @return a tibble with two cleaned columns: "Primary_Occupation" which is
#' a more thorough breakdown of a member's career, and "Primary_Sector", which
#' is categorized into four main buckets: Academy, Government, Private, Other
#' @export
#'
#' @importFrom tidyr unite
#'
#' @examples #demo_raw <- step_sector(df = demo_raw)
step_sector <- function(df) {

  df <-
    df %>%
    unite(col = "Primary_Occupation",
          contains("Primary"),
          remove = FALSE,
          na.rm = TRUE) %>%
    mutate(Primary_Sector = case_when(
      !is.na(`Academic Sector (Primary)`) ~ 'Academy',
      !is.na(`Government Sector (Primary)`) ~ 'Government',
      !is.na(`Private Sector (Primary)`) ~ 'Private',
      !is.na(`Other Sector (Primary)`) ~ 'Other',
      !is.na(`Not Provided (Primary)`) ~ 'NotProvided',
    )
    ) %>%
    mutate(
      Job_Type =
        recode(
          .data$`Primary_Occupation`,
          "Consulting firm" = "External Consulting",
          "Independent practice" = "External Consulting",
          "Consumer products" = "Internal Practice",
          "Consumer products, Manufacturing" = "Internal Practice",
          "Consumer products, Other private sector" = "Internal Practice",
          "Financial services" = "Internal Practice",
          "Manufacturing" = "Internal Practice",
          "Natural resources" = "Internal Practice",
          "Non-financial services" = "Internal Practice",
          "Technology/Telecommunications" = "Internal Practice",
          "Transportation" = "Internal Practice",
          "Utilities" = "Internal Practice",
          "Other Private Practice" = "External Consulting",
          "Other private sector" = "Internal Practice",
          "Private or non-profit research org." = "Non-Profit",
          "Business school or department" = "Academic Business Department",
          "Federal government agency" = "Government",
          "Human services (e.g., hospital)" = "Internal Practice",
          "Local government agency" = "Government",
          "Military service" = "Government",
          "Other educational institution" = "Academic - Other",
          "Other educational institution_Federal government agency" = "Academic - Other",
          "Professional school" = "Academic - Other",
          "Psychology department" = "Academic Psychology Department",
          "State government agency" = "Government",
          "Two-year college" = "Academic - Other",
          "Univ. research center/institute" = "Academic - Other"
        )
    ) %>%
    mutate(Job_Type = na_if(Job_Type, ""))

  return(df)
}
