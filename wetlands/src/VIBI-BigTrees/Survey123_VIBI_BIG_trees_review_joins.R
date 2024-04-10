
#setwd("./HTLN-Data-Capture-Scripts/wetlands/src/VIBI-BigTrees")



library(tidyverse)


# load the Survey123 data
#
#
# species codes were only used in CUVA_VIBI_woody1.csv
# join to create WoodySpecies

load_file1 <- read_csv("CUVA_VIBI_woody1.csv")
problems(load_file1)

glimpse(load_file1)

WoodySpecies_LUT <- read_csv("WoodySpecies_LUT2.csv")
problems(WoodySpecies_LUT)

glimpse(WoodySpecies_LUT)

load_file1 <- load_file1 |>
  left_join(WoodySpecies_LUT, join_by(SpeciesCode))

glimpse(load_file1)

view(load_file1)

##view(load_file1)

# check for NAs in WoodySpecies 


load_file1 |>
  select(SpeciesCode, WoodySpecies) |>
  filter(is.na(WoodySpecies)) |>
  distinct()


load_file2 <- read_csv("CUVA_VIBI_woody2.csv")
problems(load_file2)

view(load_file2)

load_file3 <- read_csv("CUVA_VIBI_woody3.csv")
problems(load_file3)

view(load_file3)

glimpse(load_file1)
glimpse(load_file2)
glimpse(load_file3)

Access_data <- bind_rows(load_file1,load_file2)

glimpse(Access_data)

Access_data <- bind_rows(Access_data,load_file3)

load_file <- Access_data

glimpse(Access_data)

#view(Access_data)

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

# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
Locations_LUT <- read_csv("tbl_Locations_20230316.csv")

glimpse(Locations_LUT)

Access_data <- Access_data |>
	  left_join(Locations_LUT, join_by(FeatureID))

glimpse(Access_data)

# Show the WoodySiteName, FeatureID where there's no 
# match in LocationsID

Access_data |>
  select(WoodySiteName, FeatureID, LocationID) |>
  filter(is.na(LocationID)) |>
  distinct()

view(Access_data)

