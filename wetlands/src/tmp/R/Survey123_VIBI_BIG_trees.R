
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

setwd("../HTLN-Data-Capture-Scripts/wetlands/src")

load_file <- read_csv("Woody.csv")

# glimpse(load_file)



##########
#
# Step 2 - select columns for Access import
# for big trees, need columns 40_1 onwards
#
##########

Access_data <- load_file |>
	select(WoodyModule, WoodySpecies, EditDate, WoodySiteName, 
	       Dgt40_1, Dgt40_2, Dgt40_3, Dgt40_4, Dgt40_5)

# glimpse(Access_data)



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



##########
#
# Step 4 - Generate EventID from EditDate
#
##########

Access_data <- Access_data |>
  mutate( EventID = str_c( 'CUVAWetlnd', EditDate)) |>
  mutate(EventID = str_replace_all(EventID, "-", ""))

# glimpse(Access_data)


##########
#
# Step 5 - create the LocationID column from the FeatureID column
#   and a lookup table from HTLNWetlands
#
##########

Locations_LUT <- read_csv("Locations_LUT.csv")

# glimpse(Locations_LUT)

Access_data <- Access_data |>
	  left_join(Locations_LUT, join_by(FeatureID))

# glimpse(Access_data)

##########
#
# Step 6 - Load species look-up table and join to data. 
#   WARNING - Many-to-many indicates multiple species
#   for a given species code. Duplicate species need to 
#   be resolved by project lead
#   
##########

WoodySpecies_LUT <- read_csv("tlu_WetlndSpeciesList2.csv")

glimpse(WoodySpecies_LUT)

WoodySpecies_LUT <- WoodySpecies_LUT |>
  mutate( WoodySpecies = ACRONYM) |>
  mutate( Common_Name = COMMON_NAME) |>
  mutate(Scientific_Name = SCIENTIFIC_NAME) |>
  select(WoodySpecies, Scientific_Name, Common_Name)

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


# pivot longer (normalize)

Access_data <- Access_data |> 
  pivot_longer( 
    cols = starts_with("Col"),
    names_to = "DiamID",
    values_to = "Count",
    values_drop_na = TRUE
  )

glimpse(Access_data)




# Join the diameter information from a LUT

Diam_LUT <- read_csv("Diam_LUT.csv")

glimpse(Diam_LUT)

Access_data <- Access_data |>
  left_join(Diam_LUT, join_by(DiamID))

# Need an end-to-end test after all the column manipulations ------------------- # Sum of counts in initial load file
in the diameter information from a LUT

Diam_LUT <- read_csv("Diam_LUT.csv")

glimpse(Diam_LUT)

Access_data <- Access_data |>
	  left_join(Diam_LUT, join_by(DiamID))

Initial_load <- load_file |>
  select(ShrubClump, D0to1,
         D1to2_5, D2_5to5, D5to10, D10to15, D15to20, D20to25, D25to30, D30to35,
         D35to40, Dgt40)



colSums(Initial_load, na.rm=TRUE)


Access_data |>
  group_by(Diam_Desc) |> 
  summarize(
    total_count = sum(Count)
  )

#-------------------------------------------------------------------------------

# Substitute NA with -9999 in Count data 
  
glimpse(Access_data)

Access_data$Count <- Access_data$Count |> replace_na(-9999)



Access_data <- Access_data |>
  select( EventID, LocationID, FeatureID, Module_No, 
          Scientific_Name, Diam_Code, Count
  )

glimpse(Access_data)

writexl::write_xlsx(Access_data, "Load_VIBI_woody.xlsx")
