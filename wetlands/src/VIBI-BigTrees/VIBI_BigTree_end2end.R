

################################################################################
#
#  VIBI_BigTree_end2end.R
#
#  Gareth Rowell, 2/28/2024
#
#  This end2end test compares the original 2023 data against
#  the exported table tbl_BigTrees after its been appended with the 
#  2023 data.
#
#
################################################################################


library(tidyverse)

# setwd("./VIBI-BigTrees")


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

# n = 1731




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

glimpse(Months_LUT)

Access_data <- Access_data |>
  left_join(Months_LUT, join_by(NumMonth))

Access_data <- Access_data |>
  mutate(EventID_left = str_sub(EventID, start = 1L, end = -5)) |>
  mutate(EventID_right = str_sub(EventID, start = 17, end = -1)) 

Access_data <- Access_data |>
  mutate(EventID = str_c(EventID_left, TxtMonth, EventID_right))

glimpse(Access_data)

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
# Step 6 - Normalize DBH columns
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

##########
#
# Step 7 - check for duplicates
#
##########

# test for dups

Access_data |> 
  select(EventID, LocationID, Module_No, Scientific_Name, DBH)

Access_data |> 
  group_by(EventID, LocationID, Module_No, Scientific_Name, DBH) |> 
  summarize(
    n = n(),
  ) |> 
  filter(n > 1)

# Remove dups with distinct() 

Access_data <- Access_data |>
  distinct(EventID, LocationID, Module_No, Scientific_Name, DBH)

Access_data

# test for dups

Access_data |> 
  group_by(EventID, LocationID, Module_No, Scientific_Name, DBH) |> 
  summarize(
    n = n(),
  ) |> 
  filter(n > 1)



#view(Access_data)

#writexl::write_xlsx(Access_data, "Load_VIBI_BigTrees_2023.xlsx")




#------------------------------------------------------------------------------
# End2End test begins here

# need to test for duplicate records

Access_data |>
  count(EventID, LocationID, Module_No, Scientific_Name, DBH) |>
  filter(n > 1)

# test for presence of -9999 in anything

Access_data |>
  filter(DBH == -9999)


end2end <- read_csv("qrye2e_bigtrees.csv")

problems(end2end)

glimpse(end2end)

glimpse(Access_data)


# matching column names

Access_data <- Access_data |>
  mutate(
    ModNo = Module_No
  )

Access_data <- Access_data |>
  select(EventID, LocationID, ModNo, Scientific_Name, DBH) 

# testing for PK - unique no-nulls

Access_data |>
  count(EventID, LocationID, ModNo, Scientific_Name, DBH)  |>
  filter(n > 1)

end2end |>
  count(EventID, LocationID, ModNo, Scientific_Name, DBH) |>
  filter(n > 1)

end2enddups <- end2end |>
  count(EventID, LocationID, Module_No, Scientific_Name, DiamID, Count) |>
  filter(n > 1) |>
  print(n = 45)

writexl::write_xlsx(end2enddups, "End2End_Dups.xlsx")

