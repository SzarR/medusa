#' Cleans a member's highest earned degree.
#'
#' @param df a tibble of demographic data
#'
#' @return a tibble with cleaned column Highest_Degree
#' @export
#'
#' @examples
#'
step_highestdegree <- function(df) {
  df %>%
    mutate(
      Highest_Degree =
        case_when(

          # PhD Recoding
          str_detect(Degree, "(?i)PhD") |
            str_detect(Degree, "DBA") |
            str_detect(Degree, "DM") | str_detect(Degree, "EdD") |
            str_detect(Degree, "EdS") |
            str_detect(Degree, "DrPH") |
            str_detect(Degree, "Dr.") | str_detect(Degree, "PsyD") |
            str_detect(Degree, "JD") |
            str_detect(Degree, "JD/MBA") ~ "Doctorate",

          # Master's Recoding
          str_detect(Degree, "ALM") |
            str_detect(Degree, "MA[:alpha:]*") |
            str_detect(Degree, "MS[:alpha:]*") |
            str_detect(Degree, "MEd") |
            str_detect(Degree, "MHRIR") |
            str_detect(Degree, "MOD") | str_detect(Degree, "ABD") |
            str_detect(Degree, "MSW") |
            str_detect(Degree, "MEd") |
            str_detect(Degree, "Mundi") |
            str_detect(Degree, "(?i)master[:alpha:]*") |
            str_detect(Degree, "M.Litt") |
            str_detect(Degree, "M.S") |
            str_detect(Degree, "MC") | str_detect(Degree, "MP") |
            str_detect(Degree, "MP(?i)h") |
            str_detect(Degree, "MBA") |
            str_detect(Degree, "MOP") ~ "Masters",

          # Bachelor's Recoding
          str_detect(Degree, "(?i)bachelor[:alpha:]*") |
            str_detect(Degree, "BCom") ~ "Bachelors",

          # Associate's Recoding
          str_detect(Degree, "(?i)associates") ~ "Associates",

          str_detect(Degree, "(?i)diploma") |
            str_detect(Degree, "(?i)certificat[:alpha:]*") |
            str_detect(Degree, "Executive") |
            str_detect(Degree, "Prerequisites") |
            str_detect(Degree, "Other") |
            str_detect(Degree, "PgDip") |
            str_detect(Degree, "grad program") ~ "Other",
          is.na(Degree) ~ "Not Specified",
          TRUE ~ 'Other'
        )
    ) %>%
    mutate(Highest_Degree = as.factor(Highest_Degree)) %>%
    return()

}
