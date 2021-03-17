#' Re-factor the Birthdate column to be a true Date variable
#'
#' @param df a tibble of demographic data
#'
#' @return a tibble
#' @export
#'
#' @examples #demo_raw <- step_birthdate(df = demo_raw)
step_birthdate <- function(df) {

  df <-
    df %>%
    rename(Birthdate = .data$`Birth Date`)

  df[['Birthdate']] <- as.Date(df$Birthdate, "%m/%d/%Y")

  return(df)
}
