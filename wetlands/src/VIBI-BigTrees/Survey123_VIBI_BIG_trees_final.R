
#setwd("./HTLN-Data-Capture-Scripts/wetlands/src/VIBI-BigTrees")

################################################################################
#
#  Survey123_VIBI_Big_trees_final.R
#
#  Gareth Rowell, 2/29/2024
#
#  This script converts csv files exported from Survey123 VIBI woody big tree
#  data to create a file that can be loaded into MS Access and directly
#  appended to the tbl_BigTrees table.
#
################################################################################

library(tidyverse)

##########
#
# Step 1 - load the Survey123 data
#          species codes were only used in CUVA_VIBI_woody1.csv
#          join to create WoodySpecies
#
##########

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
  mutate(EventID = str_replace_all(EventID, "-", "")) |>
  mutate(NumMonth = str_sub(EventID, start = 15L, end = -3L)) 

glimpse(Access_data)

#################
#
# Step 4b - Replace numeric month with text month abbreviation
#
#################

Months_LUT <- read_csv("Months_LUT.csv")

Access_data <- Access_data |>
  left_join(Months_LUT, join_by(NumMonth))

Access_data <- Access_data |>
  mutate(EventID_left = str_sub(EventID, start = 1L, end = -5)) |>
  mutate(EventID_right = str_sub(EventID, start = 17, end = -1)) 

Access_data <- Access_data |>
  mutate(EventID = str_c(EventID_left, TxtMonth, EventID_right))


##########
#
# Step 5 - create the LocationID column from the FeatureID column
#   and a lookup table from HTLNWetlands
#
##########


Locations_LUT <- read_csv("tbl_Locations_20230316.csv")

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
  mutate( Scientific_Name = WoodySpecies) |>
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

writexl::write_xlsx(Access_data, "Load_VIBI_BigTrees_2023.xlsx")
