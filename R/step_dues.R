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
          ItemDescription == "Associate Dues" ~ 'Associate',
          ItemDescription == "Associate Returning Dues" ~ 'Associate',
          ItemDescription == "Associate Renewal Dues" ~ 'Associate',
          ItemDescription == "Fellow Dues" ~ 'Fellow',
          ItemDescription == "Fellow Renewal Dues" ~ 'Fellow',
          ItemDescription == "Fellow Returning Dues" ~ 'Fellow',
          ItemDescription == "International Affiliate Dues" ~ 'International Affiliate',
          ItemDescription == "International Affiliate Renewal Dues" ~ 'International Affiliate',
          ItemDescription == "New International Affiliate Dues" ~ 'International Affiliate',
          ItemDescription == "Member Dues" ~ 'Member',
          ItemDescription == "Member Renewal Dues" ~ 'Member',
          ItemDescription == "Member Returning Dues" ~ 'Member',
          ItemDescription == "New Associate Dues" ~ 'Associate',
          ItemDescription == "New Member Dues" ~ 'Member',
          ItemDescription == "New Student Affiliate Dues" ~ 'Student Affiliate',
          ItemDescription == "Pathway Upgrade to Member From Associate" ~ 'Member',
          ItemDescription == "Retired Associate Dues" ~ 'Associate',
          ItemDescription == "Retired Associate Renewal Dues" ~ 'Retired Associate',
          ItemDescription == "Retired Associate Returning Dues" ~ 'Retired Associate',
          ItemDescription == "Retired Dues" ~ 'Retired Member', #RIGHT?
          ItemDescription == "Retired Fellow Dues" ~ 'Retired Fellow',
          ItemDescription == "Retired Fellow Renewal Dues" ~ 'Retired Fellow',
          ItemDescription == "Retired Fellow Returning Dues" ~ 'Retired Fellow',
          ItemDescription == "Retired International Affiliate Dues" ~ 'Retired International Affiliate',
          ItemDescription == "Retired International Affiliate Renewal Dues" ~ 'Retired International Affiliate',
          ItemDescription == "Retired Member Dues" ~ 'Retired Member',
          ItemDescription == "Retired Member Renewal Dues" ~ 'Retired Member',
          ItemDescription == "Retired Member Returning Dues" ~ 'Retired Member',
          ItemDescription == "Student Affiliate Dues" ~ 'Student Affiliate',
          ItemDescription == "Student Affiliate Renewal Dues" ~ 'Student Affiliate',
          ItemDescription == "Student Affiliate Returning Dues" ~ 'Student Affiliate',
          ItemDescription == "Upgrade to Associate from Student" ~ 'Associate',
          ItemDescription == "Upgrade to Member from Associate" ~ 'Member',
          ItemDescription == "Upgrade to Member from International Affiliate" ~ 'Member',
          ItemDescription == "Upgrade to Member from Student" ~ 'Member'
        )
    ) %>%
    mutate(
      MD_Number =
        case_when(
          Membership_Dues == "Student Affiliate" ~ 1,
          Membership_Dues == "Associate" ~ 2,
          Membership_Dues == "International Affiliate" ~ 3,
          Membership_Dues == "International Associate" ~ 3,
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
