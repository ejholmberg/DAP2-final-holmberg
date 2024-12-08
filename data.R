library(readxl)
library(janitor)
library(tidyverse)
library(sf)
library(tidycensus)

# Writing function to process Medicaid dual-eligible data

process_excel <- function(file_path, sheets) {
  combined_df <- tibble()
  
  for (sheet in sheets) {
    if (str_detect(sheet, "County")) {
      # Extracting date from sheet name
      date_str <- str_extract(sheet, "\\d{2}\\.\\d{4}")
      if (!is.na(date_str)) {
        date_parts <- str_split(date_str, "\\.")[[1]]
        month <- date_parts[1]
        year <- date_parts[2]
        
        # Reading sheet data
        df <- read_xlsx(file_path, sheet = sheet, skip = 2) %>%
          clean_names() %>%
          filter(
            if ("state_of_beneficiary" %in% colnames(.)) {
              state_of_beneficiary == "MA"
            } else if ("state_of_beneficiary_abbreviation" %in% colnames(.)) {
              state_of_beneficiary_abbreviation == "MA"
            } else if ("state_ofbeneficiaryabbreviation" %in% colnames(.)) {
              state_ofbeneficiaryabbreviation == "MA"
            } else if ("state_ofbeneficiary" %in% colnames(.)) {
              state_ofbeneficiary == "MA"
            } else {
              FALSE
            }
          )
        
        if (nrow(df) > 0) {
          df <- df %>%
            mutate(month = month, year = year)
          
          combined_df <- bind_rows(combined_df, df)
        }
      }
    }
  }
  
  # Combining inconsistently named county name columns
  combined_df <- combined_df %>%
    mutate(county_of_beneficiary = coalesce(county_of_beneficiary, county_of_beneficiary_name)) %>%
    select(-county_of_beneficiary_name)
  
  return(combined_df)
}

# EDIT FILE PATH HERE: the file path and sheet titles for dual-eligible data from CMS
file_path <- "~/Mirror/2024-2025/DAP 2/final/MMEnrolleeStateCountyQtrly092023.xlsx"
sheets <- c("County09.2023", "County06.2023", "County03.2023", "County12.2022", "County09.2022", 
            "County06.2022", "County03.2022", "County12.2021", "County09.2021", "County06.2021",
            "County03.2021", "County12.2020", "County09.2020", "County06.2020", "County03.2020",
            "County12.2019", "County09.2019", "County06.2019", "County03.2019", "County12.2018",
            "County09.2018", "County06.2018", "County03.2018", "County12.2017", "County09.2017",
            "County06.2017", "County03.2017", "County12.2016", "County09.2016", "County06.2016",
            "County03.2016", "County12.2015", "County09.2015", "County06.2015")

# Process the Excel file with function and perform cleaning
# Cleaning includes consolidating redundant columns, selecting columns, making strings into numeric values
combined_df <- process_excel(file_path, sheets) 

combined_df <- combined_df |> 
  filter(county_of_beneficiary != "Statewide")|> 
  mutate(qmb_only = if_else(is.na(qmb_only), qualified_medicare_beneficiaries_qmb_only, qmb_only),
          slmb_only = if_else(is.na(slmb_only), specified_low_income_medicare_beneficiaries_slmb_only, slmb_only),
          qdwi = if_else(is.na(qdwi), qualified_disabled_and_working_individuals_qdwi, qdwi),
          qi = if_else(is.na(qi), qualifying_individuals_qi, qi)) |> 
  select(2:12) |> 
  mutate(across(2:9, as.numeric))

# Option to export the combined dataframe to a CSV file
write_csv(combined_df, "combined_ma_county_data.csv")

# Adding spatial data

zippath <- "~/Mirror/2024-2025/DAP 2/final/data"
zipF <- paste0(zippath, "counties.zip")
unzip(zipF,exdir=zippath)

MA_county_shape <- st_read(file.path(zippath, 
                                  "/COUNTIESSURVEY_POLYM_GENCOAST.shp"))

combined_df_geom <- combined_df |> 
  mutate(county = str_to_upper(county_of_beneficiary),
         total = as.numeric(total),
         year = as.double(year)) |> 
  left_join(MA_county_shape,
            join_by(county == COUNTY))

# Option to save combined dataframe to CSV
write_csv(combined_df_geom, "/Users/ericholmberg/Mirror/2024-2025/DAP 2/final/combined_ma_counties.csv")

# Creating stat: MSP enrollees per 100,000 Medicare beneficiaries aged 65+
census_api_key("e7cb55b95c5e449b66c21c99912980b3c978c4d3") # replace with your own API key

  # Gathering 65+ Medicare beneficiary counts by county for each year
census_medicare <- list()

for (year in 2015:2022) {
age_acs <- get_acs(geography = "county",
                    state = "MA",
                    variables = c("C27006_010", "C27006_020"),
                    year = year,
                    survey = "acs5") |> 
  group_by(GEOID) |> 
  mutate(beneficiaries = sum(estimate),
         year = year)

  census_medicare[[as.character(year)]] <- age_acs
}

census_medicare[[as.character(2023)]] <- get_acs(geography = "county",
                                 state = "MA",
                                 variables = c("C27006_010", "C27006_020"),
                                 year = 2023,
                                 survey = "acs1") |> 
  group_by(GEOID) |> 
  mutate(beneficiaries = sum(estimate),
         year = 2023)

combined_census_medicare <- bind_rows(census_medicare) |> 
  distinct(GEOID, year, .keep_all = TRUE) |> 
  select(GEOID, beneficiaries, year) |> 
  mutate(GEOID = as.double(GEOID))

  # Gathering race by county for each year
census_race <- list()

for (year in 2015:2022) {
  race_acs <- get_acs(geography = "county",
                                 state = "MA",
                                 variables = c(white = "B03002_003"),
                                 summary_var = "B03002_001",
                                 year = year,
                                 survey = "acs5") |> 
    group_by(GEOID) |> 
    mutate(white = sum(estimate),
           white_percent = 100 * (white / summary_est),
           nonwhite_percent = 100 - white_percent,
           year = year)
  
  census_race[[as.character(year)]] <- race_acs
}

census_race[[as.character(2023)]] <- get_acs(geography = "county",
                                                 state = "MA",
                                                 variables = c(white = "B03002_003"),
                                                 summary_var = "B03002_001",
                                                 year = 2023,
                                                 survey = "acs1") |> 
  group_by(GEOID) |> 
  mutate(white = sum(estimate),
         white_percent = 100 * (white / summary_est),
         nonwhite_percent = 100 - white_percent,
         year = 2023)

combined_census_race <- bind_rows(census_race) |> 
  rename(total_pop = summary_est) |> 
  select(GEOID, total_pop, white, white_percent, nonwhite_percent, year) |> 
  mutate(GEOID = as.double(GEOID))


  # Merging MSP/geom data with census data and creating total_msp stat for just MSPs

data_full <- combined_df_geom |> 
  left_join(combined_census_medicare, join_by(FIPS_STCO == GEOID, year)) |> 
  left_join(combined_census_race, join_by(FIPS_STCO == GEOID, year)) |> 
  mutate(total_msp = total-other_dual_full_medicaid_benefit)

  # Creating stats / 100,000 people (borrowed this metric from AARP)
data_full <- data_full |>
  mutate(total_msp_per_100k = ((total_msp / beneficiaries)*100000),
         total_qmb_per_100k = ((qmb_only+qmb_plus_full_medicaid_benefits) / beneficiaries)*100000,
         total_slmb_per_100k = ((slmb_only+slmb_plus_full_medicaid_benefits) / beneficiaries)*100000,
         total_qi_per_100k = (qi / beneficiaries)*100000,
         date = as_date(paste(year, month, "30", sep = "-"), "%Y-%m-%d"))

# SAVE .RDATA - can be accessed at https://drive.google.com/file/d/1C4G0NUO2CfAASyI3VD6w92veP3LbY5EO/view?usp=sharing

saveRDS(data_full, "data_full.RData")

# Reading in API-retrieved census data (fill in directory to `data` folder)
directory <- 
combined_census_medicare <- read_rds(paste0(directory, "/census_medicare.RData"))
combined_census_race <- read_rds(paste0(directory, "/census_race.Rdata"))
