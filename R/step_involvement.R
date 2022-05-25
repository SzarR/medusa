#' Cleans Involvement Sought field
#'
#' @param df a tibble of demographic data
#'
#' @return tibble
#' @export
#'
#' @examples #step_involvement(df = demo_raw)

step_involvement <- function(df) {

  involvements <-
    df %>%
    mutate(Invs = str_split(`Involvement Sought`, ",")) %>%
    unnest(Invs) %>%
    mutate(Invs = trimws(Invs)) %>%
    mutate(Invs = str_replace_all(Invs, " ", "_")) %>%
    mutate(Invs = str_replace_all(Invs, "/", "_")) %>%
    mutate(Invs = tolower(Invs)) %>%
    group_by(SID) %>%
    count(Invs) %>%
    pivot_wider(
      names_from = Invs,
      names_prefix = "inv_",
      values_from = n,
      values_fill = 0
    ) %>%
    ungroup()

  df %>%
    left_join(involvements,
              by='SID') %>%
    select(-`Involvement Sought`,
           -inv_NA) %>%
    return()
}
