#' Function that cleans the Other Organizational Affiliations field of the original
#' demographics dataset.
#'
#' @import dplyr, stringr, tidyr
#'
#' @param df a tibble of demographic data
#'
#' @return tibble
#' @export
#'
#' @examples #step_other_orgs(df = demo_raw)

step_other_orgs <- function(df) {

  df_out <- df %>%
    rename(other_orgs = `Other Organizational Affiliations`)

  orgs_count_max <- str_count(df_out$other_orgs, ",") %>%
    max(na.rm = TRUE)

  orgs_count_names <- str_c("org_", 1:orgs_count_max)

  orgs_list <- df_out %>%
    separate(other_orgs, into = orgs_count_names) %>%
    pivot_longer(starts_with("org_")) %>%
    pull(value) %>%
    unique() %>%
    na.omit()

  orgs_names <- str_c("org_aff_", orgs_list)

  for (i in seq_along(orgs_names)) {
    df_out <- df_out %>%
      mutate(!!orgs_names[i] :=
               case_when(str_detect(other_orgs, orgs_list[i]) ~ 1, TRUE ~ 0)) # note that 0 = FALSE and 1 = TRUE
  }
  return(df_out)
}