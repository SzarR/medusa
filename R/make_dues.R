#' Bind multiple rows from dues files
#'
#' Takes and merges multiple tibbles into a single tibble in order to
#' then run make_final_data().
#'
#' @param ... Specify each tibble containing dues data
#' @param year Specify the SIOP membership year
#'
#' @return tibble
#' @export
#'
#' @examples
make_dues <- function(..., year) {

  df <-
    bind_rows(...) %>%
    select(
      SID,
      InvoiceDate,
      ItemDescription,
      Start,
      Expiration,
      Canceled,
      CanceledDate,
      Appeal,
      ControlDate
    )

  df <- step_dues(df, year)

  return(df)

}
