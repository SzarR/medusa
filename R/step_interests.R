#' Function that cleans the primary interests fields of the original
#' demographics dataset, up to a total of 3 unique interest fields.
#'
#' @param ...
#' @param df Specify the raw xlsx demographics file
#' @import magrittr
#' @import dplyr
#'
#' @return tibble
#' @export
#'
#' @examples #step_interests(df)

step_interests <- function(df) {

  df %>%
    rename(
      'Interest_1' = `Interest Area (first choice)`,
      'Interest_2' = `Interest Area (second choice)`,
      'Interest_3' = `Interest Area (third choice)`,
    ) %>%
    mutate(across(starts_with('Interest'), ~ str_replace(., " \\s*\\([^\\)]+\\)", ""))) %>% # Drop ()
    mutate(across(starts_with('Interest'), ~ gsub("(.*),.*", "\\1", .))) %>% # Drop > 1 Interest
    mutate(Interest_1 = as.factor(Interest_1),
           Interest_2 = as.factor(Interest_2),
           Interest_3 = as.factor(Interest_3)) %>%
    return()

  # The rest of this code was developed for a pivot_longer configuration
  # But I went for appending the three interest areas to the primary
  # cleaned output file instead.

  # pivot_longer(cols = -SID, names_to = "Rank") %>%
  #   mutate(Rank = as.double(str_replace(Rank, "Interest_", ""))) %>%
  #   rename('Interest' = value) %>%
  #   filter(Interest != 'NA')

  }






