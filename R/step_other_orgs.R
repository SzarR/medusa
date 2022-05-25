#' Function that cleans the Other Organizational Affiliations field of the original
#' demographics dataset.
#'
#' @param df a tibble of demographic data
#'
#' @return tibble
#' @export
#'
#' @examples #step_other_orgs(df = demo_raw)

step_other_orgs <- function(df) {

  df_out <- df %>%
    rename(other_orgs = `Other Organizational Affiliations`,
           org_aff_HRCI = `HRCI Certification: Do you hold PHR, SPHR, or GPHR certification?`,
           org_aff_SHRM = `SHRM Certification: Do you hold SHRM-CP or SHRM-SCP certification?`,
           licensed = `Are you a licensed psychologist?`
           )

  orgs_count_max <- str_count(df_out$other_orgs, ",") %>%
    max(na.rm = TRUE)

  orgs_count_names <- str_c("org_", 1:orgs_count_max)

  orgs_list <-
    df_out %>%
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

  # Code chunk that deals with licensed psychologists. from 51:55
  # Basically a yes is coded as a 1 automatically,
  # while a no is only coded as a no if two adjacent variables
  # are both NOT na. This is because the licensed psych question
  # only has Yes or NA coded, and we do not want to lump together
  # NA and no when people are paying attention to the question.

  df_out <-
    df_out %>%
    mutate(
      licensed = case_when(
        licensed == 'Yes, I am a licensed psychologist.' ~ '1',
        !is.na(org_aff_SHRM) & is.na(org_aff_HRCI) & is.na(licensed) ~ '0',
        is.na(org_aff_SHRM) & !is.na(org_aff_HRCI) & is.na(licensed) ~ '0',
        TRUE ~ NA_character_
      )
    ) %>%
    mutate(
      org_aff_HRCI = ifelse(org_aff_HRCI == "No" |
                              is.na(org_aff_HRCI), 0, 1),
      org_aff_SHRM = ifelse(org_aff_SHRM == "No" |
                              is.na(org_aff_SHRM), 0, 1)
    )

  return(df_out)

}
