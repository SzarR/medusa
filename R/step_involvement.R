#' Function that cleans the Involvement Sought field of the original
#' demographics dataset. Based from the code compiled in step_other_orgs
#'
#' @param df a tibble of demographic data
#'
#' @return tibble
#' @export
#'
#' @examples #step_involvement(df = demo_raw)

step_involvement <- function(df) {

  df_out <-
    df %>%
    rename(involve = `Involvement Sought`)

  inv_count_max <-
    str_count(df_out$involve, ",") %>%
    max(na.rm = TRUE)

  inv_count_names <- str_c("inv_sought_", 1:inv_count_max)

  inv_sought_list <-
    df_out %>%
    separate(involve, ",", into = inv_count_names) %>%
    pivot_longer(starts_with("inv_")) %>%
    pull(value) %>%
    unique() %>%
    na.omit()

  inv_names <- str_c("", inv_sought_list)
  inv_names <- trimws(inv_names)
  inv_names <- inv_names %>% unique()

  for (i in seq_along(orgs_names)) {
    df_out <- df_out %>%
      mutate(!!orgs_names[i] :=
               case_when(str_detect(other_orgs, orgs_list[i]) ~ 1, TRUE ~ 0)) # note that 0 = FALSE and 1 = TRUE
  }

  df_out <-
    df_out %>%
    mutate(cert_HRCI = ifelse(cert_HRCI == "Yes", 1, 0),
           cert_SHRM = ifelse(cert_SHRM == "Yes", 1, 0))

  return(df_out)
}
