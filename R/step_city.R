#' Cleans and categorizes open-ended city name data and corrects errors.
#'
#' @param df a tibble of demographic data
#'
#' @return a tibble
#' @import stringr
#' @export
#'
#' @examples #demo_raw <- step_city(df = demo_raw)
step_city <- function(df) {

  abbrev_table <-
    c("-" = " ",
      "Mt. " = "Mount ",
      "Jct." = "Junction",
      "Ft. " = "Fort ",
      "Bch." = "Beach",
      "Bch" = "Beach",
      "Twp." = "Township",
      "Twp" = "Township",
      "Hts." = "Heights",
      "St. " = "Saint ",
      "Ft " = "Fort ",
      "St " = "Saint ",
      "Mt " = "Mount "
      )

  df %>%
    mutate(
      City =
        recode(
           .data$City,
          '20037' =  'Washington',
          `East York` = 'York',
          `East Orange` = 'Orange',
          Allingtown = 'West Haven',
          `District of columbia` = 'Washington',
          `District Of Columbia` = 'Washington',
          `Washington DC` = 'Washington',
          Washingtonshington = 'Washington',
          `Washingtn Cross` = 'Washington Cross',
          `University Hts.` = 'University Heights',
          `06870` = 'Stamford',
          `Shelby Twp` = 'Shelby Township',
          `North Seaford` = 'Seaford',
          `St. Petersburg,` = 'Saint Petersburg',
          `Roaring Brook Twp` = 'Roaring Brook Township',
          `University of Richmind` = 'Richmond',
          `East Providence` = 'Providence',
          `North Providence` = 'Providence',
          `Pt Pleasant Bch` = 'Point Pleasant Beach',
          `Omaha NE` = 'Omaha',
          `Oakland TWP` = 'Oakland Township',
          `New York City` = 'New York',
          `New York, Ny` = 'New York',
          `New York, NY` = 'New York',
          `New. York` = 'New York',
          `NYC` = 'New York',
          `Queens` = 'New York',
          `Monroe Twp` = 'Monroe Township',
          `Monroe Township.` = 'Monroe Township',
          `Mc Donough` = 'McDonough',
          `Alhambra, Los Angeles` = 'Los Angeles',
          `Los Angeles102` = 'Los Angeles',
          `Longwood FL` = 'Longwood',
          `Liberty Twp` = 'Liberty Township',
          `N. Las Vegas` = 'Las Vegas',
          `Lk Forest Park` = 'Forest Park',
          `Egin` = 'Elgin',
          `De Kalb` = 'DeKalb',
          `Colorado Spr.` = 'Colorado Springs',
          `Colorado Spring` = 'Colorado Springs',
          `U S A F Academy` = 'Colorado Springs',
          `USAF Academy` = 'Colorado Springs',
          `Chatswprth` = 'Chatsworth',
          `Cardiff by the Sea` = 'Cardiff',
          `Broken  Arrow` = 'Broken Arrow',
          `Allston` = 'Boston',
          `Beverly Hills, CA` = 'Beverly Hills',
          `Bellingham,` = 'Bellingham',
          `Arlington Hghts` = 'Arlington Heights',
          `Arlington Hts` = 'Arlington Heights',
          `EastAmherst` = 'Amherst',
          `Agoura` = 'Agoura Hills',
          `Dadaphne` = 'Daphne',
          `DaDaphne` = 'Daphne',
          #`Ft Belvoir` = 'Fort Belvoir',
          #`St Paul` = 'Saint Paul',
          #`St Paul Park` = 'Saint Paul Park',
          #`St Michaels` = 'Saint Michaels',
          # `St Louis` = 'Saint Louis',
          # `St. Louis` = 'Saint Louis',
          # `St Louis Park` = 'Saint Louis Park',
          # `St Joseph` = 'Saint Joseph',
          # `St Cloud` = 'Saint Cloud',
          # `St Clair Shores` = 'Saint Clair Shores',
          # `St Augustine` = 'Saint Augustine',
          `Port St Lucie` = 'Port Saint Lucie',
          `E. Northport` = 'Northport',
          #`Mt Prospect` = 'Mount Prospect',
          #`Mt Holly` = 'Mount Holly',
          #`Ft Belvoir` = 'Fort Belvoir',
          `Altus Afb` = 'Altus',
          `Bayamón` = 'Bayamon',
          `Charleston Afb` = 'Charleston',
          `Connecticut` = 'Hartford',
          `E. Flagstaff` = 'Flagstaff',
          `Eastamherst` = 'Amherst',
          `Hopewell Jct` = 'Hopewell Junction',
          `Huntington Beach.` = 'Huntington Beach',
          `Joint Base Lewis Mcchord` = 'Seattle',
          `Longwood Fl` = 'Longwood',
          `Maxwell Afb` = 'Montgomery',
          `Monroe Township.` = 'Monroe Township',
          `MountPleasant` = 'Mount Pleasant',
          `University Of Richmond` = 'Richmond',
          `O Fallon` = "O'Fallon",
          `East Islip` = 'Islip',
          `Twin City` = 'Minneapolis',
          `South Orange` = 'Orange',
          `Land O' Lakes` = 'Land O Lakes',
          `Capital Heights` = 'Capitol Heights',
          `Santa  Barbara` = 'Santa Barbara',
          `Saint Pete Beach ` = 'Saint Petersburg',
          `South Glastonbury` = 'Glastonbury',
          `Lees Summit` = "Lee's Summit",
          `Chrstiansted` = 'Christiansted'
        )
    ) %>%
    mutate(City = str_replace_all(.data$City, abbrev_table)) %>%
    mutate(City = toupper(.data$City)) %>%
    return()
}
