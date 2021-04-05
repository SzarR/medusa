#' Wrapper function for binding several raw tibbles of dues data that could
#' exist by month, into a single tibble that is then inputted as the dues
#' argument in make_final_data
#'
#'
#' @param ... Specify each raw tibble of dues data
#' @param year Specify the SIOP membership year
#' @import magrittr
#' @import dplyr
#'
#' @return tibble
#' @export
#'
#' @examples #make_dues(dues_0120, dues_0220, dues_0320, year = 2020)
make_dues <- function(..., year) {

  df <-
    bind_rows(...) %>%
    select(
      .data$SID,
      .data$InvoiceDate,
      .data$ItemDescription,
      .data$Start,
      .data$Expiration,
      .data$Canceled,
      .data$CanceledDate,
      .data$Appeal,
      .data$ControlDate
    )

  df <- step_dues(df, year)

  return(df)

}
