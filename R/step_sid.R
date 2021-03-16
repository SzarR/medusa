step_sid <- function(df) {

  df <-
    df %>%
    group_by(SID) %>%
    arrange(MD_Number) %>%
    arrange(desc(InvoiceDate)) %>%
    slice_max(n = 1, with_ties = FALSE, order_by = MD_Number) %>%
    ungroup() %>%
    return()

}
