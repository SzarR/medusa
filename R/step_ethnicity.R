#' Cleans a member's racial ethnicity.
#'
#' @param df a tibble of demographic data
#' @param detailed_types If ethnicity should be reported by country,
#' or according to the new simplified schema such as Black, Hispanic, White, etc.
#'
#' @return a tibble with cleaned column Ethnicity
#' @export
#'
#' @examples #demo_raw <- step_ethnicity(df = demo_raw, detailed_types = FALSE)
step_ethnicity <- function(df, detailed_types=FALSE) {

  if(detailed_types == TRUE) {

    df %>%
      mutate(
        Ethnicity =
          case_when(
            str_detect(Ethnicity, "(?i)half") | str_detect(Ethnicity, "Multi-racial") | str_detect(Ethnicity, " / ") ~ "Multi-racial",
            str_detect(Ethnicity, "Arabic") ~ "Arabic",
            str_detect(Ethnicity, "Black") ~ "Black or African American",
            str_detect(Ethnicity, "Chin[:alpha:]*") ~ "Chinese",
            str_detect(Ethnicity, "Korea[:alpha:]*") ~ "Korean",
            str_detect(Ethnicity, "Japan[:alpha:]*") ~ "Japanese",
            str_detect(Ethnicity, "(?i)Indian") ~ "Indian",
            str_detect(Ethnicity, "(?i)pakist[:alpha:]*") ~ "Pakistani",
            str_detect(Ethnicity, "(?i)vietn[:alpha:]*") ~ "Vietnamese",
            str_detect(Ethnicity, "(?i)Hmong[:alpha:]*") ~ "Hmong",
            str_detect(Ethnicity, "(?i)Malay[:alpha:]*") ~ "Malaysian",
            str_detect(Ethnicity, "(?i)Nepal[:alpha:]*") ~ "Nepali",
            str_detect(Ethnicity, "(?i)Philippi[:alpha:]*") | str_detect(Ethnicity, "(?i)Filipino[:alpha:]*") ~ "Filipino",
            str_detect(Ethnicity, "(?i)Taiwan[:alpha:]*") ~ "Taiwanese",
            str_detect(Ethnicity, "Cambodian") ~ "Cambodian",
            str_detect(Ethnicity, "(?i)Singa[:alpha:]*") ~ "Singaporean",
            str_detect(Ethnicity, "(?i)Indo[:alpha:]*") ~ "Indonesian",
            str_detect(Ethnicity, "Middle Eastern") ~ "Middle Eastern",
            str_detect(Ethnicity, "Hispanic") ~ "Hispanic or Latino",
            str_detect(Ethnicity, "White/Caucasian") ~ "White/Caucasian",
            str_detect(Ethnicity, "(?)Native American") ~ "American Indian or Alaska Native",
            str_detect(Ethnicity, "Pacific Islander*") ~ "Native Hawaiian or Other Pacific Islander",
            str_detect(Ethnicity, "(?i)half") | str_detect(Ethnicity, "Multi-racial") | str_detect(Ethnicity, " / ") ~ "Multi-racial",
            str_detect(Ethnicity, "Oth") ~ "Other",
            str_detect(Ethnicity, "0") | str_detect(Ethnicity, "#N/A") | str_detect(Ethnicity, "Not Reported") ~ NA_character_,
            TRUE ~ NA_character_
          )
      ) %>%
          mutate(Ethnicity = as.factor(Ethnicity)) %>%
          return()
  } else if (detailed_types == FALSE) {

    df %>%
      mutate(
        Ethnicity =
          case_when(
            str_detect(Ethnicity, "(?i)half") | str_detect(Ethnicity, "Multi-racial") | str_detect(Ethnicity, " / ") ~ "Multi-racial",
            str_detect(Ethnicity, "Arabic") | str_detect(Ethnicity, "Middle Eastern") | str_detect(Ethnicity, "White/Caucasian") ~ "White",
            str_detect(Ethnicity, "Black") ~ "Black or African American",
            str_detect(Ethnicity, "Chin[:alpha:]*") | str_detect(Ethnicity, "Korea[:alpha:]*") | str_detect(Ethnicity, "Japan[:alpha:]*") |
              str_detect(Ethnicity, "(?i)india[:alpha:]*") | str_detect(Ethnicity, "(?i)pakist[:alpha:]*") | str_detect(Ethnicity, "(?i)vietn[:alpha:]*") |
              str_detect(Ethnicity, "(?i)Hmong[:alpha:]*") | str_detect(Ethnicity, "(?i)Malay[:alpha:]*") | str_detect(Ethnicity, "(?i)Nepal[:alpha:]*") |
              str_detect(Ethnicity, "(?i)Filipino[:alpha:]*") | str_detect(Ethnicity, "(?i)Philippi[:alpha:]*") | str_detect(Ethnicity, "(?i)Thai[:alpha:]*") |
              str_detect(Ethnicity, "(?i)Taiwan[:alpha:]*") | str_detect(Ethnicity, "(?i)Asia[:alpha:]*") | str_detect(Ethnicity, "Cambodian") |
              str_detect(Ethnicity, "(?i)Singa[:alpha:]*") | str_detect(Ethnicity, "(?i)Indo[:alpha:]*") 
             ~ "Asian",
            str_detect(Ethnicity, "Hispanic") ~ "Hispanic or Latino",
            str_detect(Ethnicity, "(?)Native American") ~ "American Indian or Alaska Native",
            str_detect(Ethnicity, "Pacific Islander*") ~ "Native Hawaiian or Other Pacific Islander",
            str_detect(Ethnicity, "Oth") ~ "Other",
            str_detect(Ethnicity, "0") | str_detect(Ethnicity, "#N/A") | str_detect(Ethnicity, "Not Reported") ~ NA_character_,
            TRUE ~ NA_character_
          )
     ) %>%
      mutate(Ethnicity = as.factor(Ethnicity)) %>%
      return()
  }
}
