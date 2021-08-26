#' Cleans a member's highest earned degree.
#'
#' @param df a tibble of demographic data
#' @param include_ABD logical specifying if to include ABD degree
#'
#' @return a tibble with cleaned column Highest_Degree
#' @export
#'
#' @examples
step_highestdegree <- function(df, include_ABD = FALSE) {

  if(include_ABD == TRUE) {

    df <- df %>%
      mutate(
        Highest_Degree =
          case_when(
            str_detect(Degree, "ABD") | str_detect(Degree, "grad program") ~ "ABD",

            str_detect(Degree, "(?i)PhD") | str_detect(Degree, "DBA") | str_detect(Degree, "DM") | str_detect(Degree, "EdD") |
            str_detect(Degree, "EdS") | str_detect(Degree, "DrPH") | str_detect(Degree, "Dr.") | str_detect(Degree, "PsyD") |
            str_detect(Degree, "JD") | str_detect(Degree, "JD/MBA") ~ "Doctorate",

            str_detect(Degree, "ALM") | str_detect(Degree, "MA[:alpha:]*") | str_detect(Degree, "MS[:alpha:]*") |
            str_detect(Degree, "MEd") | str_detect(Degree, "MHRIR") | str_detect(Degree, "MOD") |
            str_detect(Degree, "MSW") | str_detect(Degree, "MEd") | str_detect(Degree, "Mundi") | str_detect(Degree, "(?i)master[:alpha:]*") |
            str_detect(Degree, "M.Litt") | str_detect(Degree, "M.S") | str_detect(Degree, "MC") | str_detect(Degree, "MP") |
            str_detect(Degree, "MP(?i)h") | str_detect(Degree, "MBA") ~ "Masters",

            str_detect(Degree, "(?i)bachelor[:alpha:]*") | str_detect(Degree, "BCom") ~ "Bachelors",

            str_detect(Degree, "(?i)associates") ~ "Associates",

            str_detect(Degree, "(?i)diploma") | str_detect(Degree, "(?i)certificat[:alpha:]*") | str_detect(Degree, "Executive") |
            str_detect(Degree, "Prerequisites") | str_detect(Degree, "Other") | str_detect(Degree, "PgDip") ~ "Other",

            is.na(Degree) ~ "Not Specified"
          )
      ) %>%
      mutate(Highest_Degree = as.factor(Highest_Degree)) %>%
      return()
  } else if (include_ABD == FALSE) {

    df <- df %>%
      mutate(
        Highest_Degree =
          case_when(
            str_detect(Degree, "ABD") | str_detect(Degree, "grad program") | str_detect(Degree, "(?i)PhD") | str_detect(Degree, "DBA") |
            str_detect(Degree, "DM") | str_detect(Degree, "EdD") | str_detect(Degree, "EdS") | str_detect(Degree, "DrPH") |
            str_detect(Degree, "Dr.") | str_detect(Degree, "PsyD") ~ "Doctorate",
            str_detect(Degree, "JD") | str_detect(Degree, "JD/MBA") | str_detect(Degree, "MBA") ~ "Professional Degree",
            str_detect(Degree, "ALM") | str_detect(Degree, "MA[:alpha:]*") | str_detect(Degree, "MS[:alpha:]*") |
            str_detect(Degree, "MEd") | str_detect(Degree, "MHRIR") | str_detect(Degree, "MOD") |
            str_detect(Degree, "MSW") | str_detect(Degree, "MEd") | str_detect(Degree, "Mundi") | str_detect(Degree, "(?i)master[:alpha:]*") |
            str_detect(Degree, "M.Litt") | str_detect(Degree, "M.S") | str_detect(Degree, "MC") | str_detect(Degree, "MP") |
            str_detect(Degree, "MP(?i)h") ~ "Masters",
            str_detect(Degree, "(?i)bachelor[:alpha:]*") | str_detect(Degree, "BCom") ~ "Bachelors",
            str_detect(Degree, "(?i)associates") ~ "Associates",
            str_detect(Degree, "(?i)diploma") | str_detect(Degree, "(?i)certificat[:alpha:]*") | str_detect(Degree, "Executive") |
            str_detect(Degree, "Prerequisites") | str_detect(Degree, "Other") | str_detect(Degree, "PgDip") ~ "Other",
            is.na(Degree) ~ "Not Specified"
          )
      ) %>%
      mutate(Highest_Degree = as.factor(Highest_Degree)) %>%
      return()
  }
}
