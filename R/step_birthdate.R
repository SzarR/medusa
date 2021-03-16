step_birthdate <- function(df) {

  df <-
    df %>%
    rename(Birthdate = `Birth Date`)

  df[['Birthdate']] <- as.Date(df$Birthdate, "%m/%d/%Y")

  return(df)
}
