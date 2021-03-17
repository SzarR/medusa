#' Cleans a member's state of residence, USA only.
#'
#' @param df a tibble of demographic data
#'
#' @return a tibble with cleaned column State_US
#' @export
#'
#' @examples #demo_raw <- step_state(df = demo_raw)
step_state <- function(df) {

  state_abb <- as.factor(
    c(
      "AL","AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL",
      "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT",
      "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI",
      "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY",
      "DC"
      #, "GU", "VI"
    )
  )

  df <-
    df %>%
    mutate(
      STATE =
        case_when(
          .data$Country == 'Australia' & .data$STATE == 'WA' ~ 'NA',
          TRUE ~ .data$STATE
        )
    )

  # Impute Missing State Acronyms
  # for one-off cases.
  df[df$SID == 227392,'STATE'] <- 'IL'
  df[df$SID == 223150,'STATE'] <- 'IL'
  df[df$SID == 219042,'STATE'] <- 'PA'
  df[df$SID == 220947,'STATE'] <- 'MA'
  df[df$SID == 218993,'STATE'] <- 'CO'
  df[df$SID == 227987,'STATE'] <- 'CT'
  df[df$SID == 222591,'STATE'] <- 'AR'
  df[df$SID == 219733,'STATE'] <- 'FL'
  df[df$SID == 219923,'STATE'] <- 'FL'
  df[df$SID == 221295,'STATE'] <- 'NY'
  df[df$SID == 214853,'STATE'] <- 'NJ'
  df[df$SID == 157171,'STATE'] <- 'NY'
  df[df$SID == 215795,'STATE'] <- 'TX'
  df[df$SID == 215155,'STATE'] <- 'MD'
  df[df$SID == 212937,'STATE'] <- 'CA'
  df[df$SID == 215144,'STATE'] <- 'CA'
  df[df$SID == 172888,'STATE'] <- 'LA'

  df %>%
    rename('State_US' = .data$STATE) %>%
    mutate(
      State_US =
        recode(
          .data$State_US,
          `Puerto Rico` = 'PR',
          Florida = 'FL',
          Illinois = 'IL'
        )
    ) %>%
    mutate(State_US =
             factor(
               .data$State_US,
               levels = state_abb)) %>%
    return()
}
