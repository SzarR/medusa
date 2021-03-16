#' Title
#'
#' @param df a raw demographic tibble
#' @param detailed_types If ethnicity should be reported by country, or according
#' to the new simplified schema such as Black, Hispanic, White, etc.
#'
#' @return tibble
#' @export
#'
#' @examples
step_ethnicity <- function(df, detailed_types=FALSE) {

  if(detailed_types == TRUE) {

    df %>%
      mutate(
        Ethnicity =
          recode_factor(
            Ethnicity,
            "Arabic" = "Arabic",
            "Black or African-American" = "Black or African American",
            "China" = "Chinese",
            "East Asian (Korean)" = "Korean",
            "East Indian" = "Indian",
            "Filipino / Caucasian" = "Multi-racial",
            "Half Caucasian/Half Asian" = "Multi-racial",
            "Hispanic/Latino(a)/or other Spanish origin " = "Hispanic/Latino(a)/or other Spanish origin",
            "Hmong" = "Other",
            "Hmong American" = "Other",
            "Indian " = "Indian",
            "Japanese American" = "Japanese",
            "Korean American" =	"Korean",
            "Malaysia" =	"Other",
            "Malaysian" =	"Other",
            "Middle Eastern" = "Middle Eastern",
            "Multi-racial" = "Multi-racial",
            "Other" = "Other",
            "Pakistan" =	"Other",
            "Pakistani" =	"Other",
            "Philippines" =	"Filipino",
            "Singapore Eurasian" =	"Other",
            "South east Asian" = "Other",
            "South Korea" =	"Korean",
            "South Korean" =	"Korean",
            "Taiwanese-American" =	"Other",
            "Taiwanese" =	"Other",
            "Thai and Chinese" =	"Other",
            "Thai" =	"Other",
            "Thai-Chinese" =	"Other",
            "Vietnamese Chinese American" =	"Other",
            "Vietnamese" =	"Vietnamese",
            "Vietnamese-American" = 	"Vietnamese",
            "White/Caucasian" = "White/Caucasian",
            "0" = "NA",
            "#N/A" = "NA",
            "Not Reported" = "NA"
          )
      ) %>%
      return()
  } else if (detailed_types == FALSE) {

    df %>%
      mutate(
        Ethnicity =
          recode_factor(
            Ethnicity,
            "Arabic" = "White",
            "Black or African-American" = "Black or African American",
            "China" = "Asian",
            "Chinese" = "Asian",
            "Korean" = "Asian",
            "Japanese" = "Asian",
            "Republic of Korea" = "Asian",
            "Nepali" = "Asian",
            "Republic of Korea (South)" = "Asian",
            "Indian " = "Asian",
            "Pakistani Muslim" = "Asian",
            "VIetnamese" = "Asian",
            "Asian Pakistan" = "Asian",
            "Cambodian" = "Black or African American",
            "INDIAN" = "Asian",
            "East Asian (Korean)" = "Asian",
            "East Indian" = "Asian",
            "Filipino / Caucasian" = "Two or more races",
            "Half Caucasian/Half Asian" = "Two or more races",
            "Hispanic/Latino(a)/or other Spanish origin" = "Hispanic or Latino",
            "Asian Indian" = "Asian",
            "Hmong" = "Asian",
            "Hmong American" = "Asian",
            "Indian" = "Asian",
            "Japanese American" = "Asian",
            "Korean American" =	"Asian",
            "Malaysia" =	"Asian",
            "Malaysian" =	"Asian",
            "Middle Eastern" = "White",
            "Multi-racial" = "Two or more races",
            "Other" = "Other",
            "Filipino" = "Asian",
            "Pakistan" =	"Asian",
            "Pakistani" =	"Asian",
            "Philippines" =	"Asian",
            "Singapore Eurasian" =	"Asian",
            "South east Asian" = "Asian",
            "South Korea" =	"Asian",
            "South Korean" =	"Asian",
            "Taiwanese-American" =	"Asian",
            "Taiwanese" =	"Asian",
            "Thai and Chinese" =	"Asian",
            "Thai" =	"Asian",
            "Thai-Chinese" =	"Asian",
            "Vietnamese Chinese American" =	"Asian",
            "Vietnamese" =	"Asian",
            "Vietnamese-American" = 	"Asian",
            "White/Caucasian" = "White",
            "0" = 'Other',
            "#N/A" = 'Other',
            "Not Reported" = 'Other',
            "Not reported" = 'Other'
          )
      ) %>%
      return()
  }
}
