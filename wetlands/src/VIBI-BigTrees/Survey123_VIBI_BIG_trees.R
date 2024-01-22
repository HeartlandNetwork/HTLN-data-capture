
##########
#
# This script takes woody VIBI data from Survey123 and 
#   transforms the BIG tree (> 40 cm DBH) portion to be
#   loaded into the tbl_BigTree table in the HTLNWetlands
#   database
#
##########


##########
#
# Step 1 - Load data
#
##########


library(tidyverse)

#setwd("../HTLN-Data-Capture-Scripts/wetlands/src")


# load the Survey123 data

load_file1 <- read_csv("CUVA_VIBI_woody1.csv")
problems(load_file1)

load_file2 <- read_csv("CUVA_VIBI_woody2.csv")
problems(load_file2)

load_file3 <- read_csv("CUVA_VIBI_woody3.csv")
problems(load_file3)

glimpse(load_file1)
glimpse(load_file2)
glimpse(load_file3)

Access_data <- bind_rows(load_file1,load_file2)

glimpse(Access_data)

Access_data <- bind_rows(Access_data,load_file3)

load_file <- Access_data

glimpse(Access_data)

view(Access_data)

##########
#
# Step 2 - select columns for Access import
# for big trees, need columns 40_1 onwards
#
##########

Access_data <- Access_data |>
	select(WoodyModule, WoodySpecies, EditDate, WoodySiteName, 
	       Dgt40_1, Dgt40_2, Dgt40_3, Dgt40_4, Dgt40_5)

 glimpse(Access_data)



##########
#
# Step 3 - create columns for FeatureID and Module_No
#   and convert date format
#
##########

Access_data <- Access_data |>
  mutate( FeatureID = WoodySiteName) |>
  mutate(EditDate = (EditDate <- as.Date(EditDate, format = "%m/%d/%Y"))) |>
  mutate( Module_No = WoodyModule ) 

Access_data


##########
#
# Step 4 - Generate EventID from EditDate
#
##########

Access_data <- Access_data |>
  mutate( EventID = str_c( 'CUVAWetlnd', EditDate)) |>
  mutate(EventID = str_replace_all(EventID, "-", ""))

glimpse(Access_data)


##########
#
# Step 5 - create the LocationID column from the FeatureID column
#   and a lookup table from HTLNWetlands
#
##########

Locations_LUT <- read_csv("Locations_LUT.csv")

glimpse(Locations_LUT)

Access_data <- Access_data |>
	  left_join(Locations_LUT, join_by(FeatureID))

glimpse(Access_data)

##########
#
# Step 6 - Load species look-up table and join to data. 
#   WARNING - Many-to-many indicates multiple species
#   for a given species code. see scripts:
#   WoodySpecies_cleanup.R
#   
##########

WoodySpecies_LUT <- read_csv("WoodySpecies_LUT2.csv")

glimpse(WoodySpecies_LUT)

Access_data <- Access_data |>
	  left_join(WoodySpecies_LUT, join_by(WoodySpecies))

glimpse(Access_data)


##########
#
# Step 7 - Normalize DBH columns
#
#   
##########

# Rename columns using DiamID values for pivot_longer

Access_data$Tree1 <- Access_data$Dgt40_1 
Access_data$Tree2 <- Access_data$Dgt40_2 
Access_data$Tree3 <- Access_data$Dgt40_3
Access_data$Tree4 <- Access_data$Dgt40_4 
Access_data$Tree5 <- Access_data$Dgt40_5

Access_data <- Access_data |>
  mutate( SpeciesCode = WoodySpecies) |>
  mutate( SampleDate = EditDate) |>
  select(EventID, LocationID, Module_No, Scientific_Name, Tree1, 
         Tree2, Tree3, Tree4, Tree5)


glimpse(Access_data)


# pivot longer (normalize)

Access_data <- Access_data |> 
	  pivot_longer( 
	           cols = starts_with("Tree"),
			       names_to = "TreeName",
			         values_to = "DBH",
			         values_drop_na = TRUE
			       )

glimpse(Access_data)

view(Access_data)

writexl::write_xlsx(Access_data, "Load_VIBI_BigTrees.xlsx")
