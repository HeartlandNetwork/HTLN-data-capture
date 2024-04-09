# This script is to detect FeatureIDs not found in the LocationID 
# lookup table. Need to run these by Sonia...

library(tidyverse)

#setwd("./src")

# load the Survey123 data

load_file1 <- read_csv("CUVA_VIBI_herb1.csv")
load_file2 <- read_csv("CUVA_VIBI_herb2.csv")
load_file3 <- read_csv("CUVA_VIBI_herb3.csv")

glimpse(load_file1)
glimpse(load_file2)
glimpse(load_file3)

Access_data <- bind_rows(load_file1,load_file2)

glimpse(Access_data)

Access_data <- bind_rows(Access_data,load_file3)

glimpse(Access_data)

# record count is 1731. 
# From the spreadsheets: 304 + 280 + 1147 = 1731

# select columns from Survey 123 and create FeatureID column

Access_data <- Access_data |> 
  select(Species, Module, CoverClass_LT_6m, CoverClassAll, EditDate, HerbSiteName, 
         Other_species_not_on_dropdown_list) |>
  mutate( 
    FeatureID = HerbSiteName,
    CoverClass = CoverClass_LT_6m,
    Other_species = Other_species_not_on_dropdown_list,
    EditDate = (EditDate <- as.Date(EditDate, format = "%m/%d/%Y"))
  )

glimpse(Access_data)

# Substitute NA with -9999 in CoverClass and CoverClassAll

Access_data$CoverClass <- Access_data$CoverClass |> replace_na(-9999)

Access_data$CoverClassAll <- Access_data$CoverClassAll |> replace_na(-9999)


# Generate EventID from EditDate

Access_data <- Access_data |>
  mutate( EventID = str_c( 'CUVAWetlnd', EditDate)) |>
  mutate(EventID = str_replace_all(EventID, "-", ""))

glimpse(Access_data)

# create the LocationID column from the FeatureID column
# and a lookup table from HTLNWetlands
# ERROR - FeatureIDs not joining 


Locations_LUT <- read_csv("Locations_LUT.csv")

glimpse(Locations_LUT)

Access_data <- Access_data |>
  left_join(Locations_LUT, join_by(FeatureID))

glimpse(Access_data)

#Looking for featureIDs associated with NAs in LocationID

Access_data |>
  select(HerbSiteName, FeatureID, LocationID) |>
  filter(is.na(LocationID)) |>
  distinct(HerbSiteName)



