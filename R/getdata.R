
#' Gets relevant data from Google Sheets.
#'
#' @return an uncleaned `data.frame`
#' @import gsheet
#' @import tidyr
#' @import dplyr
#' @export
#' @keywords internal
#'
#' @examples
#' getdata()
getdata <- function() {
  # Set filename and URLs
  google_sheet_url_1 <-
    "https://docs.google.com/spreadsheets/d/1KrPJe9OOGuix2EAvcTfuspAe3kqN-y20Huyf5ImBEl4/edit#gid=804798874"
  google_sheet_url_2 <-
    "https://docs.google.com/spreadsheets/d/1l7wuOt0DZp0NHMAriedbG4EJkl06Mh-G01CrVh8ySJE/edit#gid=804798874"
  google_sheet_url_3 <-
    "https://docs.google.com/spreadsheets/d/1duPtC8L9KL51YQzQ1rZ5785iH3RjABZFCYu8SWjAPTU/edit#gid=1458650276"
  google_sheet_url_4 <- # Soil data
    "https://docs.google.com/spreadsheets/d/109xYUM48rjj33B76hZ3bNlrm8u-_S6uyoE_3wSCp0r0/edit#gid=1458650276"
  soil_file <- "soil_data.csv"

  # Check if the file is/has been written.
  # If not, read it in from google sheets, save as `soil_data`
  if (!(file.exists(soil_file))) {
    # Google Authentication not needed because "anyone can view"
    # Do the data wrangling
    soil_data <-
      inner_join(
        gsheet2tbl(google_sheet_url_1),
        gsheet2tbl(google_sheet_url_2),
        by = c(`What is your site name?` = 'site_name'),
        keep = TRUE
      ) %>%
      inner_join(gsheet2tbl(google_sheet_url_3), by = "full_name") %>%
      inner_join(
        gsheet2tbl(google_sheet_url_4),
        by = c("full_name", "site_id")
      ) %>% # Special characters
      rename("type" = `Which best describes your site?`) %>%
      separate("type", into = c("type", "type2"), sep = ":")
    write.csv(soil_data, soil_file)
  } else {
    soil_data <- read.csv(soil_file)[,-1]
  }

  return(soil_data)

}


#' Cleans data from Google Sheets to make GPS coordinates consistent.
#'
#' @param the_data
#'
#' @return Cleaned `data.frame`
#' @import tidyr
#' @import dplyr
#' @import stringr
#' @export
#' @keywords internal
#'
#' @examples
#' clean_gps_points(getdata())
clean_gps_points <- function(the_data) {
  # Clean up GPS points:
  # Remove parentheses, split column, fix negatives, and make numeric
  the_data <- the_data %>%
    rename("coord" = 4) %>%
    mutate(coord = str_replace_all(coord, "[//(,//)]*", "")) %>%
    tidyr::separate(coord,
                    into = c("latitude", "longitude"),
                    sep = " ") %>%
    mutate(longitude = case_when(
      as.numeric(longitude) > 0 ~ as.numeric(longitude) * -1,
      TRUE ~ as.numeric(longitude)
    )) %>%
    mutate(latitude = as.numeric(latitude))

  return(the_data)
}


#' Produce data in a nice clean format for use in R.
#'
#' @return a `data.frame`
#' @import dplyr
#' @import lubridate
#' @export
#'
#' @examples
#' BioDIGS_metadata()
BioDIGS_metadata <- function() {
  metadata_ <-
    BioDIGSData:::clean_gps_points(BioDIGSData:::getdata())

  metadata_ <-
    metadata_ %>%
    mutate(timestamp = lubridate::as_datetime(Timestamp.x)) %>%
    mutate(date_sampled = lubridate::date(timestamp)) %>%
    select(site_id, site_name, type, date_sampled, latitude, longitude)

  return(metadata_)
}


#' Produce DNA concentration data in a clean format for use in R.
#'
#' @return a `data.frame`
#' @import dplyr
#' @export
#'
#' @examples
#' BioDIGS_DNA_conc_data()
BioDIGS_DNA_conc_data <- function() {
  # Read in from Google, clean GPS points
  dna_data <- BioDIGSData:::getdata()

  dna_data_ <-
    dna_data %>%
    select(site_id,
           site_name,
           ul_hydration,
           qubit_concentration_ng_ul,
           total_ng,
           type)

  return(dna_data_)
}


#' Produce soil testing data in a clean format for use in R.
#'
#' @return a `data.frame`
#' @import dplyr
#' @export
#'
#' @examples
#' BioDIGS_soil_data()
BioDIGS_soil_data <- function() {
  testing_data_ <- BioDIGSData:::getdata()

  testing_data_ <-
    testing_data_ %>%
    select(
      site_id,
      site_name,
      type,
      tidyr::ends_with("EPA3051"),
      water_pH,
      OM_by_LOI_pct,
      tidyr::ends_with("Mehlich3"),
      Est_CEC,
      Base_Sat_pct,
      P_Sat_ratio
    ) %>%
    mutate(As_EPA3051 = as.numeric(case_when(As_EPA3051 == "< 3.0" ~ "0",
                                             TRUE ~ As_EPA3051))) %>%      # As can't be detected lower than 3.0
    mutate(Cd_EPA3051 = as.numeric(case_when(Cd_EPA3051 == "< 0.2" ~ "0",
                                             TRUE ~ Cd_EPA3051))) %>%      # Cd can't be detected lower than 0.2
    mutate(
      region = case_when(
        startsWith(site_id, "M") ~ "Montgomery County",
        startsWith(site_id, "B") ~ "Baltimore City",
        startsWith(site_id, "S") ~ "Seattle"
      )
    )

  message("Note: Arsenic (As_EPA3051) is not detectable below 3.0 mg/kg. Cadmium (Cd_EPA3051) is not detectable below 0.2 mg/kg.")
  return(testing_data_)
}
