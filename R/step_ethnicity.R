#' Cleans a member's racial ethnicity.
#'
#' @param df a tibble of demographic data
#' @param detailed_types If ethnicity should be reported by country, or according
#' to the new simplified schema such as Black, Hispanic, White, etc.
#'
#' @return a tibble with cleaned column Ethnicity
#' @export
#'
#' @examples #demo-raw <- step_ethnicity(df = demo_raw, detailed_types = FALSE)
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
            str_detect(Ethnicity, "Middle Eastern") ~ "Middle Eastern",
            str_detect(Ethnicity, "Vietnamese") ~ "Vietnamese",
            str_detect(Ethnicity, "Hispanic") ~ "Hispanic or Latino",
            str_detect(Ethnicity, "White/Caucasian") ~ "White/Caucasian",
            str_detect(Ethnicity, "(?i)half") | str_detect(Ethnicity, "Multi-racial") | str_detect(Ethnicity, " / ") ~ "Multi-racial",
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
            str_detect(Ethnicity, "Black") | str_detect(Ethnicity, "Cambodian") ~ "Black or African American",
            str_detect(Ethnicity, "Chin[:alpha:]*") | str_detect(Ethnicity, "Korea[:alpha:]*") | str_detect(Ethnicity, "Japan[:alpha:]*") |
              str_detect(Ethnicity, "(?i)india[:alpha:]*") | str_detect(Ethnicity, "(?i)pakist[:alpha:]*") | str_detect(Ethnicity, "(?i)vietn[:alpha:]*") |
              str_detect(Ethnicity, "(?i)Hmong[:alpha:]*") | str_detect(Ethnicity, "(?i)Malay[:alpha:]*") | str_detect(Ethnicity, "(?i)Nepal[:alpha:]*") |
              str_detect(Ethnicity, "(?i)Filipino[:alpha:]*") | str_detect(Ethnicity, "(?i)Philippi[:alpha:]*") | str_detect(Ethnicity, "(?i)Thai[:alpha:]*") |
              str_detect(Ethnicity, "(?i)Taiwan[:alpha:]*") | str_detect(Ethnicity, "(?i)Asia[:alpha:]*") ~ "Asian",
            str_detect(Ethnicity, "Hispanic") ~ "Hispanic or Latino",
            str_detect(Ethnicity, "Oth") ~ "Other",
            str_detect(Ethnicity, "Reported") ~ "Other",
            TRUE ~ NA_character_
          )
     ) %>%
    # df %>%
    #   mutate(
    #     Ethnicity =
    #       recode_factor(
    #         Ethnicity,
    #         "Arabic" = "White",
    #         "Black or African-American" = "Black or African American",
    #         "China" = "Asian",
    #         "Chinese" = "Asian",
    #         "Korean" = "Asian",
    #         "Japanese" = "Asian",
    #         "Republic of Korea" = "Asian",
    #         "Nepali" = "Asian",
    #         "Republic of Korea (South)" = "Asian",
    #         "Indian " = "Asian",
    #         "Pakistani Muslim" = "Asian",
    #         "VIetnamese" = "Asian",
    #         "Asian Pakistan" = "Asian",
    #         "Cambodian" = "Black or African American",
    #         "INDIAN" = "Asian",
    #         "East Asian (Korean)" = "Asian",
    #         "East Indian" = "Asian",
    #         "Filipino / Caucasian" = "Two or more races",
    #         "Half Caucasian/Half Asian" = "Two or more races",
    #         "Hispanic/Latino(a)/or other Spanish origin" = "Hispanic or Latino",
    #         "Asian Indian" = "Asian",
    #         "Hmong" = "Asian",
    #         "Hmong American" = "Asian",
    #         "Indian" = "Asian",
    #         "Japanese American" = "Asian",
    #         "Korean American" =	"Asian",
    #         "Malaysia" =	"Asian",
    #         "Malaysian" =	"Asian",
    #         "Middle Eastern" = "White",
    #         "Multi-racial" = "Two or more races",
    #         "Other" = "Other",
    #         "Filipino" = "Asian",
    #         "Pakistan" =	"Asian",
    #         "Pakistani" =	"Asian",
    #         "Philippines" =	"Asian",
    #         "Singapore Eurasian" =	"Asian",
    #         "South east Asian" = "Asian",
    #         "South Korea" =	"Asian",
    #         "South Korean" =	"Asian",
    #         "Taiwanese-American" =	"Asian",
    #         "Taiwanese" =	"Asian",
    #         "Thai and Chinese" =	"Asian",
    #         "Thai" =	"Asian",
    #         "Thai-Chinese" =	"Asian",
    #         "Vietnamese Chinese American" =	"Asian",
    #         "Vietnamese" =	"Asian",
    #         "Vietnamese-American" = 	"Asian",
    #         "White/Caucasian" = "White",
    #         "0" = 'Other',
    #         "#N/A" = 'Other',
    #         "Not Reported" = 'Other',
    #         "Not reported" = 'Other'
    #       )
    #   )
      mutate(Ethnicity = as.factor(Ethnicity)) %>%
      return()
  }
}
