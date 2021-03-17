#' Clean and categorize a member's age. Bins have been pre-established according
#' to best practices. Age is calculated from the date the function was ran.
#' Results may therefore vary.
#'
#' @param df a tibble of demographic data
#'
#' @return Age, AgeGroup
#' @export
#'
#' @examples #step_agegroup(df = demo_raw)
step_agegroup <- function(df) {

  df$Birthdate <-
    as.Date(df$Birthdate,
            format= "%m/%d/%Y")

  df$todaysdate <-
    as.Date(format(Sys.Date(), "%m/%d/%Y"), "%m/%d/%Y")

  df$Age <-
    as.numeric(difftime(df$todaysdate, df$Birthdate, units = "weeks")) / 52.25

  df$Age <-
    round(df$Age, digits = 0)

  df$AgeGroup <-
    cut(df$Age,
        breaks =
          c(
            -Inf,
            20,
            24,
            29,
            34,
            39,
            44,
            49,
            54,
            59,
            64,
            69,
            Inf
          ),
        labels =
          c(
            "20 and under",
            "20-24",
            "25-29",
            "30-34",
            "35-39",
            "40-44",
            "45-49",
            "50-54",
            "55-59",
            "60-64",
            "65-69",
            "70+")
    )
  return(df)
}
