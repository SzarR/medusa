#' Cleans a variety of columns in dues data
#'
#' @param df a tibble of dues data
#' @param year the SIOP year to filter data on
#'
#' @return a tibble of SIDs applicable for the given year selected.
#' @export
#'
#' @examples #dues_cleaned <- step_dues(df = dues_cleaned, year = 2020)
step_dues <- function(df, year) {

  year <- as.numeric(year)

  df <-
    df %>%
    mutate(
      Membership_Dues =
        case_when(
          str_detect(ItemDescription, "Retired") & str_detect(ItemDescription, "International") ~ "Retired International",
          str_detect(ItemDescription, "Retired") & str_detect(ItemDescription, "Associate") ~ "Retired Associate",
          str_detect(ItemDescription, "Retired") & str_detect(ItemDescription, "Member") ~ "Retired Member",
          str_detect(ItemDescription, "Retired") & str_detect(ItemDescription, "Fellow") ~ "Retired Fellow",
          str_detect(ItemDescription, "International") & str_detect(ItemDescription, "Affiliate") ~ "International Affiliate",
          str_detect(ItemDescription, "International") & str_detect(ItemDescription, "Associate") ~ "International Associate",
          str_detect(ItemDescription, "Upgrade to Associate") ~ "Associate",
          str_detect(ItemDescription, "Upgrade to Member") ~ "Member",
          str_detect(ItemDescription, "Student") ~ "Student Affiliate",
          str_detect(ItemDescription, "Affiliate") ~ "Affiliate",
          str_detect(ItemDescription, "Associate") ~ "Associate",
          str_detect(ItemDescription, "Member") ~ "Member",
          str_detect(ItemDescription, "Fellow") ~ "Fellow"
        )
    ) %>%
    mutate(
      MD_Number =
        case_when(
          Membership_Dues == "Student Affiliate" ~ 1,
          Membership_Dues == "Associate" ~ 2,
          Membership_Dues == "Affiliate" ~ 3,
          # Do not need to differentiate across US vs. international
          #Membership_Dues == "International Affiliate" ~ 3,
          #Membership_Dues == "International Associate" ~ 3,
          Membership_Dues == "Member" ~ 4,
          Membership_Dues == "Fellow" ~ 5,
          Membership_Dues == "Retired International" ~ 6,
          Membership_Dues == "Retired International Affiliate" ~ 6,
          Membership_Dues == "Retired Associate" ~ 7,
          Membership_Dues == "Retired Member" ~ 8,
          Membership_Dues == "Retired Fellow" ~ 9
        )
    ) %>%
    mutate(
      Student_Prof =
        case_when(
          MD_Number == 1 ~ 'Student Affiliate',
          TRUE ~ 'Professional'
        )
    )

  # Count membership per specific year.
  df <-
    df %>%
    mutate(InvoiceDate = as.Date(.data$InvoiceDate)) %>%
    mutate(
      year_filter =
        case_when(
          .data$InvoiceDate >= paste0(year - 1, '-03-01') &
            .data$InvoiceDate <= paste0(year, '-06-30') ~ 1,
          TRUE ~ 0
        )
    ) %>%
    filter(.data$year_filter == 1)

  df <-
    df %>%
    filter(.data$Canceled == 0) #%>%

  df <-
    df %>%
    mutate(Expire_Year = lubridate::year(.data$Expiration))

  df <-
    df %>%
    select(.data$SID,
           .data$InvoiceDate,
           .data$Membership_Dues,
           .data$MD_Number,
           .data$Student_Prof,
           .data$Expire_Year,
           .data$InvoiceDate,
           .data$Canceled,
           .data$CanceledDate
)

  return(df)

}
