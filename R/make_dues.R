#' Bind multiple rows from dues files
#'
#' Takes and merges multiple tibbles into a single tibble in order to
#' then run make_final_data().
#'
#' @param ... Specify each tibble containing dues data
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
