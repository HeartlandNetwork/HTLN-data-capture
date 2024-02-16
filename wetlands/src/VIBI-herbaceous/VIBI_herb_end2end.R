
################################################################################
#
#  VIBI_herb_end2end.R
#
#  Gareth Rowell, 2/16/

#  This end2end test compares the original 2023 data against
#  the exported table tbl_VIBI_herb after its been appended with the 
#  2023 data.
#
#
################################################################################


library(tidyverse)

#setwd("./")


#################
#
# Step 1 - load spreadsheet csv files and appended them
#
#################


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

1017 + 1281 + 1780

#################
#
# Step 2 - select and rename columns, convert date to yyyy-mm-dd
#
#################


Access_data <- Access_data |> 
  select(Species, SpeciesComments, Module, CoverClass_LT_6m, 
         CoverClassAll, EditDate, HerbSiteName) |>
  mutate( 
    FeatureID = HerbSiteName,
    CoverClass = CoverClass_LT_6m,
    EditDate = (EditDate <- as.Date(EditDate, format = "%m/%d/%Y"))
  )

glimpse(Access_data)


#################
#
# Step 3 - Substitute NA with -9999 in CoverClass and CoverClassAll
#
#################


Access_data$CoverClass <- Access_data$CoverClass |> replace_na(-9999)

Access_data$CoverClassAll <- Access_data$CoverClassAll |> replace_na(-9999)


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


#################
#
# Step 5 - Create the LocationID column from the FeatureID column
#
#################


Locations_LUT <- read_csv("tbl_Locations_20230316.csv")

glimpse(Locations_LUT)

Access_data <- Access_data |>
  left_join(Locations_LUT, join_by(FeatureID))


#################
#
# Step 6 - Clean up columns and write load file
#
#################

# clean up columns

Access_data <- Access_data |>
  select(EventID, FeatureID, LocationID, Species, SpeciesComments, Module,
         CoverClass, CoverClassAll, EditDate )

# writexl::write_xlsx(Access_data, "Load_VIBI_herb_2023.xlsx")

#------------------------------------------------------------------------------
# End2End test begins here

end2end <- read_csv("qrye2e_VIBI_herb.csv")
  
problems(end2end)

glimpse(end2end)

glimpse(Access_data)

# 7 records got dropped. Which ones?

# left_join(d1, d2, by = c("x" = "x2", "y" = "y2"))

left_join(end2end, Access_data, 
    by = c("EventID" = "EventID", "LocationID" = "LocationID", ))

