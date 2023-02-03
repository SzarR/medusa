#' Runs the entire data cleaning procedure for a given year
#'
#' @param year The SIOP year to filter on
#' @param set_root_dir point the function to the folder that contains each year of SIOP data to clean
#' @param save_output_to set the CSV save out location
#'
#' @return CSV file
#' @export
#'
#' @examples build_dataset(year = '2017', set_root_dir =/data/Raw/", save_output_to = "/Users/robs/Desktop/")
build_dataset <- function(year, set_root_dir, save_output_to) {
  # List the components
  list_years <- list.files(set_root_dir)
  list_files <- list.files(paste0(set_root_dir, year))

  # Find files in given year
  demo_file <-
    list_files[list_files %>% str_detect(fixed('demo', ignore_case = TRUE))]
  dues_file <-
    list_files[list_files %>% str_detect(fixed('dues', ignore_case = TRUE))]
  staff_file <-
    list_files[list_files %>% str_detect(fixed('staff', ignore_case = TRUE))]

  # demographics
  df_demo <-
    read_xlsx(path = paste0(set_root_dir, year, "/", demo_file))

  if (year < '2021') {
    df_demo <- df_demo %>%
      mutate(ZipCode = '99999')
  }

  # dues
  dues_master_file <- NULL
  for (i in 1:length(dues_file)) {
    temp_file <-
      read_xlsx(path = paste0(set_root_dir, year, "/", dues_file[i]))
    dues_master_file <- bind_rows(dues_master_file, temp_file)

  }

  # staff
  df_staff <-
    read_xlsx(path = paste0(set_root_dir, year, "/", staff_file))

  # Demographic cleaning
  df_demo_clean <- medusa::make_demo_data(df_demo)

  # Dues cleaning
  df_dues <- medusa::make_dues(dues_master_file, year = year)

  final_data <- medusa::make_final_data(demo = df_demo_clean,
                                         dues = df_dues,
                                         staff = df_staff)

  final_output <-
    paste0(save_output_to,
           "demo_cleaned_",
           year,
           "_",
           as.character(today()),
           ".csv")
  readr::write_csv(final_data, final_output)

}
