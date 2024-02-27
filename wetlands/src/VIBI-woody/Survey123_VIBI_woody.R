

library(tidyverse)

setwd("../HTLN-Wetlands-Survey123/src")

load_file <- read_csv("Woody.csv")

glimpse(load_file)

# select columns for Access import

Access_data <- load_file |>
	select(WoodyModule, WoodySpecies, EditDate, WoodySiteName, ShrubClump, D0to1,
	       D1to2_5, D2_5to5, D5to10, D10to15, D15to20, D20to25, D25to30, D30to35,
	       D35to40, Dgt40, Dgt40_1, Dgt40_2, Dgt40_3, Dgt40_4, Dgt40_5)

Access_data <- Access_data |>
  mutate( FeatureID = WoodySiteName) |>
  mutate(EditDate = (EditDate <- as.Date(EditDate, format = "%m/%d/%Y")))

glimpse(Access_data)

# Substitute NA with -9999 in all dbl variable

Access_data$ShrubClump <- Access_data$ShrubClump |> replace_na(-9999)
Access_data$D0to1 <- Access_data$D0to1 |> replace_na(-9999)
Access_data$D1to2_5 <- Access_data$D1to2_5 |> replace_na(-9999)
Access_data$D2_5to5 <- Access_data$D2_5to5 |> replace_na(-9999)
Access_data$D5to10 <- Access_data$D5to10 |> replace_na(-9999)
Access_data$D10to15 <- Access_data$D15to20 |> replace_na(-9999)
Access_data$D15to20 <- Access_data$D15to20 |> replace_na(-9999)
Access_data$D20to25 <- Access_data$D20to25 |> replace_na(-9999)
Access_data$D25to30 <- Access_data$D25to30 |> replace_na(-9999)
Access_data$D30to35 <- Access_data$D30to35 |> replace_na(-9999)
Access_data$D35to40 <- Access_data$D35to40 |> replace_na(-9999)
Access_data$Dgt40 <- Access_data$Dgt40 |> replace_na(-9999)

glimpse(Access_data)

# Rename columns using DiamID values

Access_data$C0 <- Access_data$ShrubClump 
Access_data$C1 <- Access_data$D0to1 
Access_data$C2 <- Access_data$D1to2_5 
Access_data$C3 <- Access_data$D2_5to5 
Access_data$C4 <- Access_data$D5to10 
Access_data$C5 <- Access_data$D15to20 
Access_data$C6 <- Access_data$D15to20 
Access_data$C7 <- Access_data$D20to25 
Access_data$C8 <- Access_data$D25to30 
Access_data$C9 <- Access_data$D30to35 
Access_data$C10 <- Access_data$D35to40 
Access_data$BIG <- Access_data$Dgt40 

glimpse(Access_data)


#################
#
# Step 4a - Generate EventID from EditDate
#
#################

Access_data <- Access_data |>
  mutate( EventID = str_c( 'CUVAWetlnd', EditDate)) |>
  mutate(EventID = str_replace_all(EventID, "-", "")) |>
  mutate(NumMonth = str_sub(EventID, start = 15L, end = -3L)) 

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



# create the LocationID column from the FeatureID column
# and a lookup table from HTLNWetlands

Locations_LUT <- read_csv("Locations_LUT.csv")

glimpse(Locations_LUT)

Access_data <- Access_data |>
  left_join(Locations_LUT, join_by(FeatureID))

glimpse(Access_data)


# set up columns before normalization

Access_data <- Access_data |>
	select(EventID, LocationID, FeatureID, WoodyModule, WoodySpecies, 
	       EditDate, WoodySiteName, ShrubClump, C0, 
	       C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, BIG)

glimpse(Access_data)









###########################################################################
#Access_data <- Access_data |> 
#	  pivot_longer( cols = starts_with("C0"),
#			       names_to = "DiamID", 
#			       values_to = "Count")
#billboard %>%
#	  pivot_longer(
#		           cols = starts_with("wk"),
#			       names_to = "week",
#			       names_prefix = "wk",
#			           values_to = "rank",
#			           values_drop_na = TRUE
#				     )
#
#glimpse(Access_data)
#
#write_csv(Access_data, Access_data$Outfile[1])

#writexl::write_xlsx(Access_data, "Load_VIBI_herb.xlsx")
  



