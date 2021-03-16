step_state <- function(df) {

  df <-
    df %>%
    mutate(
      STATE =
        case_when(
          Country == 'Australia' & STATE == 'WA' ~ 'NA',
          TRUE ~ STATE
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
    rename('State_US' = STATE) %>%
    mutate(
      State_US =
        recode(
          State_US,
          `Puerto Rico` = 'PR',
          Florida = 'FL',
          Illinois = 'IL'
        )
    ) %>%
    mutate(State_US =
             factor(
               State_US,
               levels = state_abb)) %>%
    return()
}
