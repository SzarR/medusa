#' Cleans a tibble containing SIOP annual conference participants
#'
#' @param df a tibble of Annual Conference participants
#' @param merge_demo_from The resulting tibble from  make_final_data
#' @param year Specify conference year as numeric vector
#'
#' @return a tibble of conference attendees
#' @export
#'
#' @examples
get_conf_attendance <- function(df, merge_demo_from, year) {

  df <-
    df %>%
    select(.data$SID,
           .data$Category,
           .data$Description,
           .data$Canceled) %>%
    filter(.data$Canceled == 0) %>%
    mutate(
      Year = as.integer(stringr::str_extract(.data$Category, "\\d+")),
      Conference = case_when(
        str_detect(string = Description,
                   pattern = paste(year, "Annual")) ~ "Annual",
        str_detect(string = Description,
                   pattern = "Spouse/Guest") ~ "Annual",
        str_detect(string = Description,
                   pattern = paste("SIOP",year, "Complimentary")) ~ "Annual",
        str_detect(string = Description,
                   pattern = "Exhibit Hall Only Registration") ~ "Annual",
        str_detect(string = Description,
                   pattern = "AC") ~ "Annual",
        TRUE ~ "Other"
      )
    ) %>%
    filter(.data$Year == year & .data$Conference == 'Annual') %>%
    group_by(.data$SID) %>%
    slice_max(n = 1, with_ties = FALSE, order_by = .data$SID)


  merge_demo_from %>%
    right_join(df, by = "SID") %>%
    select(
      .data$SID,
      .data$Membership_Dues,
      .data$MD_Number,
      .data$City,
      .data$State_US,
      .data$Country,
      .data$Profit_Non,
      .data$Field,
      .data$AgeGroup,
      .data$Gender,
      .data$Ethnicity,
      .data$Birthdate,
      .data$Highest_Degree,
    ) %>%
    mutate(Conference = paste(year, "Annual Conference")) %>%
    return()

}
