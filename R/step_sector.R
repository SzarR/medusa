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
    ))

  return(df)
}
