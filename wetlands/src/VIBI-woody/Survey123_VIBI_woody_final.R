

################################################################################
#
#  Survey123_VIBI_woody_final.R
#
#  Gareth Rowell, 2/16/2024
#
#  This script converts csv files exported from Survey123 VIBI woody
#  data to create a file that can be loaded into MS Access and directly
#  appended to the tbl_VIBI_woody table.
#
################################################################################

library(tidyverse)

#setwd("../HTLN-Data-Capture-Scripts/wetlands/src")


#setwd("./src")

#################
#
# Step 1 - load spreadsheet csv files and appended them
# Note - species codes were only used in CUVA_VIBI_woody1.csv
# and also CUVA_VIBI_woody3.csv
# They were joined to WoodySpecies_LUT2.csv to create WoodySpecies
#
#################


# species codes were only used in CUVA_VIBI_woody1.csv
# join to WoodySPecies_LUT2.csv to create WoodySpecies

load_file1 <- read_csv("CUVA_VIBI_woody1.csv")
problems(load_file1)

WoodySpecies_LUT <- read_csv("WoodySpecies_LUT2.csv")

load_file1 <- load_file1 |>
  left_join(WoodySpecies_LUT, join_by(SpeciesCode))

glimpse(load_file1)

glimpse(WoodySpecies_LUT)

# view(load_file1)

# check for NAs in WoodySpecies 


load_file1 |>
  select(SpeciesCode, WoodySpecies) |>
  filter(is.na(WoodySpecies)) |>
  distinct()


load_file2 <- read_csv("CUVA_VIBI_woody2.csv")
problems(load_file2)

load_file3 <- read_csv("CUVA_VIBI_woody3.csv")
problems(load_file3)

load_file3 <- load_file3 |>
  left_join(WoodySpecies_LUT, join_by(SpeciesCode))

glimpse(load_file3)

load_file4 <- read_csv("CUVA_VIBI_woody4.csv")
problems(load_file4)

glimpse(load_file1)
glimpse(load_file2)
glimpse(load_file3)
glimpse(load_file4)

Access_data <- bind_rows(load_file1,load_file2)

glimpse(Access_data)

Access_data <- bind_rows(Access_data,load_file3)

glimpse(Access_data)

Access_data <- bind_rows(Access_data,load_file4)

glimpse(Access_data)

load_file <- Access_data # for normalization test in step 8

#################
#
# Step 2 - select and rename columns, convert date to yyyy-mm-dd
#
#################


Access_data <- Access_data |>
	select(WoodyModule, WoodySpecies, EditDate, WoodySiteName, ShrubClump, D0to1,
	       D1to2_5, D2_5to5, D5to10, D10to15, D15to20, D20to25, D25to30, D30to35,
	       D35to40, Dgt40, Dgt40_1, Dgt40_2, Dgt40_3, Dgt40_4, Dgt40_5)

Access_data <- Access_data |>
  mutate( FeatureID = WoodySiteName) |>
  mutate(EditDate = (EditDate <- as.Date(EditDate, format = "%m/%d/%Y"))) |>
  mutate( Module_No = WoodyModule ) 

glimpse(Access_data)

#################
#
# Step 3 - Rename columns using DiamID values for pivot_longer
#
#################


Access_data$Col1 <- Access_data$ShrubClump 
Access_data$Col2<- Access_data$D0to1 
Access_data$Col3 <- Access_data$D1to2_5 
Access_data$Col4 <- Access_data$D2_5to5 
Access_data$Col5 <- Access_data$D5to10 
Access_data$Col6 <- Access_data$D10to15
Access_data$Col7 <- Access_data$D15to20 
Access_data$Col8 <- Access_data$D20to25 
Access_data$Col9 <- Access_data$D25to30 
Access_data$Col10 <- Access_data$D30to35 
Access_data$Col11 <- Access_data$D35to40 
Access_data$Col12 <- Access_data$Dgt40 

glimpse(Access_data)



#################
#
# Step 4 - Generate EventID from EditDate
#
#################


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


#################
#
# Step 5 - create the LocationID column from the FeatureID column
#          and a lookup table from HTLNWetlands
#
#################

Locations_LUT <- read_csv("tbl_Locations_20230316.csv")

glimpse(Locations_LUT)

Access_data <- Access_data |>
  left_join(Locations_LUT, join_by(FeatureID))

glimpse(Access_data)

#################
#
# Step 6 - set up columns before normalization, then pivot longer
#
#################


Access_data <- Access_data |>
	select(EventID, LocationID, FeatureID, Module_No, WoodySpecies, 
	       EditDate, WoodySiteName, Col1, Col2, Col3, Col4, Col5,
	       Col6, Col7, Col8, Col9, Col10, Col11, Col12)

glimpse(Access_data)
# pivot longer (normalize)

Access_data <- Access_data |> 
	  pivot_longer( 
	           cols = starts_with("Col"),
			       names_to = "DiamID",
			         values_to = "Count",
			         values_drop_na = TRUE
			       )

glimpse(Access_data)


#################
#
# Step 7 - Join the diameter information from a LUT
#
#################



Diam_LUT <- read_csv("Diam_LUT.csv")

glimpse(Diam_LUT)

Access_data <- Access_data |>
  left_join(Diam_LUT, join_by(DiamID))


#################
#
# Step 8 - Validate normalization and join using
#          Sum of counts in initial load file
#          against total_counts for each diameter 
#          in final version
#
#################



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
          WoodySpecies, Diam_Code, Count
  )

glimpse(Access_data)
view(Access_data)

writexl::write_xlsx(Access_data, "Load_VIBI_woody_2023.xlsx")




