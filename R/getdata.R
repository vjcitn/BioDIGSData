
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
        by = c(`What.is.your.site.name.` = 'site_name'),
        keep = TRUE
      ) %>%
      inner_join(gsheet2tbl(google_sheet_url_3), by = "full_name") %>%
      inner_join(
        gsheet2tbl(google_sheet_url_4),
        by = c("full_name", "site_id")
      ) %>% # Special characters
      rename("type" = `Which.best.describes.your.site.`) %>%
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
#' @param info if set to TRUE, print important information to the console. This can be disabled with `info = FALSE`.
#'
#' @return a `data.frame`
#' @import dplyr
#' @import lubridate
#' @export
#'
#' @examples
#' BioDIGS_metadata()
BioDIGS_metadata <- function(info = TRUE) {
  metadata_ <-
    BioDIGSData:::clean_gps_points(BioDIGSData:::getdata())

  metadata_ <-
    metadata_ %>%
    mutate(timestamp = lubridate::as_datetime(Timestamp.x)) %>%
    mutate(date_sampled = lubridate::date(timestamp)) %>%
    select(site_id, site_name, type, date_sampled, latitude, longitude)

  if(info){
    cli_alert(col_cyan("See the data dictionary by typing {.code ?BioDIGS_metadata()}."))
    cli_alert(col_cyan("Visit us at {.url https://biodigs.org/}"))
  }

  return(metadata_)
}


#' Produce DNA concentration data in a clean format for use in R.
#'
#' @param info if set to TRUE, print important information to the console. This can be disabled with `info = FALSE`.
#'
#'
#' @return a `data.frame`
#' @import dplyr
#' @import cli
#' @export
#'
#' @examples
#' BioDIGS_DNA_conc_data()
BioDIGS_DNA_conc_data <- function(info = TRUE) {
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

  if(info){
    cli_alert(col_cyan("See the data dictionary by typing {.code ?BioDIGS_DNA_conc_data()}."))
    cli_alert(col_cyan("Visit us at {.url https://biodigs.org/}"))
  }

  return(dna_data_)
}


#' Produce soil testing data in a clean format for use in R.
#'
#' @details
#' | **Field Name** |      | **Description** |
#' |----------------|------|:-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
#' |  site_id       | `  ` | Unique letter and number site name |
#' |  full_name     | `  ` | Full site name                                                    |
#' |  As_EPA3051    | `  ` | Arsenic (mg/kg), EPA Method 3051A. Quantities < 3.0 are not detectable.  |
#' |  Cd_EPA3051    | `  ` | Cadmium (mg/kg), EPA Method 3051A. Quantities < 0.2 are not detectable.  |
#' |  Cr_EPA3051    | `  ` | Chromium (mg/kg), EPA Method 3051A                           |
#' |  Cu_EPA3051    | `  ` | Copper (mg/kg), EPA Method 3051A                             |
#' |  Ni_EPA3051    | `  ` | Nickel (mg/kg), EPA Method 3051A                             |
#' |  Pb_EPA3051    | `  ` | Lead (mg/kg), EPA Method 3051A                               |
#' |  Zn_EPA3051    | `  ` | Zinc (mg/kg), EPA Method 3051A                              |
#' |  water_pH      | `  ` | Water pH                                                     |
#' |  A-E_Buffer_pH | `  ` | Buffer pH                                                   |
#' |  OM_by_LOI_pct | `  ` | Organic Matter by Loss on Ignition                       |
#' |  P_Mehlich3    | `  ` | Phosphorus (mg/kg), using the Mehlich 3 soil test extractant |
#' |  K_Mehlich3    | `  ` | Potassium (mg/kg), using the Mehlich 3 soil test extractant |
#' |  Ca_Mehlich3   | `  ` | Calcium (mg/kg), using the Mehlich 3 soil test extractant |
#' |  Mg_Mehlich3   | `  ` | Magnesium (mg/kg), using the Mehlich 3 soil test extractant |
#' |  Mn_Mehlich3   | `  ` | Manganese (mg/kg), using the Mehlich 3 soil test extractant |
#' |  Zn_Mehlich3   | `  ` | Zinc (mg/kg), using the Mehlich 3 soil test extractant  |
#' |  Cu_Mehlich3   | `  ` | Copper (mg/kg), using the Mehlich 3 soil test extractant |
#' |  Fe_Mehlich3   | `  ` | Iron (mg/kg), using the Mehlich 3 soil test extractant |
#' |  B_Mehlich3    | `  ` | Boron (mg/kg), using the Mehlich 3 soil test extractant |
#' |  S_Mehlich3    | `  ` | Sulfur (mg/kg), using the Mehlich 3 soil test extractant |
#' |  Na_Mehlich3   | `  ` | Sodium (mg/kg), using the Mehlich 3 soil test extractant |
#' |  Al_Mehlich3   | `  ` | Aluminum (mg/kg), using the Mehlich 3 soil test extractant |
#' |  Est_CEC       | `  ` | Cation Exchange Capacity (meq/100g) at pH 7.0 (CEC) |
#' |  Base_Sat_pct  | `  ` | Base saturation (BS). This represents the percentage of CEC occupied by bases (Ca2+, Mg2+, K+, and Na+). The %BS increases with increasing soil pH (Figure 5). The availability of Ca2+, Mg2+, and K+ increases with increasing %BS. |
#' |  P_Sat_ratio   | `  ` | Phosphorus saturation ratio. This is the ratio between the amount of phosphorus present in the soil and the total capacity of that soil to retain phosphorus. The ability of phosphorus to be bound in the soil is primary a function of iron (Fe) and aluminum (Al) content in that soil. |
#'
#' @param info if set to TRUE, print important information to the console. This can be disabled with `info = FALSE`.
#'
#' @return a `data.frame`
#' @import dplyr
#' @import cli
#' @export
#'
#' @examples
#' BioDIGS_soil_data()
BioDIGS_soil_data <- function(info = TRUE) {
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

  if(info){
    cli_alert_info(
      col_magenta(
        "Arsenic (As_EPA3051) is not detectable below 3.0 mg/kg. Cadmium (Cd_EPA3051) is not detectable below 0.2 mg/kg."
      )
    )
    cli_alert(col_cyan("See the data dictionary by typing {.code ?BioDIGS_soil_data()}."))
    cli_alert(col_cyan("Visit us at {.url https://biodigs.org/}"))
  }

  return(testing_data_)
}

