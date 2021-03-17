#' Removes SID duplicates
#'
#' @param df a tibble of dues data
#'
#' @return a tibble of rows with unique SID values
#' @export
#'
#' @examples
step_sid <- function(df) {

  df <-
    df %>%
    group_by(.data$SID) %>%
    arrange(.data$MD_Number) %>%
    arrange(desc(.data$InvoiceDate)) %>%
    slice_max(n = 1, with_ties = FALSE, order_by = .data$MD_Number) %>%
    ungroup() %>%
    return()

}
