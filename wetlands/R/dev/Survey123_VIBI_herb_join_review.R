
library(tidyverse)

# setwd("./wetlands/src/VIBI-herbaceous")

# load the Survey123 data

load_file1 <- read_csv("CUVA_VIBI_herb1.csv")
load_file2 <- read_csv("CUVA_VIBI_herb2.csv")
load_file3 <- read_csv("CUVA_VIBI_herb3.csv")

glimpse(load_file1)
glimpse(load_file2)
glimpse(load_file3)

load_file1 <- load_file1 |> 
  select(
    Species, SpeciesComments, Module, 
    CoverClass_LT_6m, CoverClassAll,
    EditDate, HerbSiteName
  )

load_file2 <- load_file2 |>
  select(
    Species, SpeciesComments, Module, 
    CoverClass_LT_6m, CoverClassAll,
    EditDate, HerbSiteName
  )

load_file3 <- load_file3 |>
  select(
    Species, SpeciesComments, Module, 
    CoverClass_LT_6m, CoverClassAll,
    EditDate, HerbSiteName
  )

glimpse(load_file1)
glimpse(load_file2)
glimpse(load_file3)


Access_data <- bind_rows(load_file1,load_file2)

glimpse(Access_data)

Access_data <- bind_rows(Access_data,load_file3)

glimpse(Access_data)

view(Access_data)

1017 + 1281 + 1780


# Create some column names and fix date

Access_data <- Access_data |>
  mutate( 
    FeatureID = HerbSiteName,
    CoverClass = CoverClass_LT_6m,
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

# view(Access_data)


# <<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# create the LocationID column from the FeatureID column
# and a lookup table from HTLNWetlands

# many-to-many with left_join
# is FeatureID unique with Locations_LUT


Locations_LUT <- read_csv("tbl_Locations_20230316.csv")

glimpse(Locations_LUT)

# view(Locations_LUT)


duplicates <- Locations_LUT |> 
  group_by(FeatureID) |> 
  summarize(
    n = n()
  ) |> 
  filter( n > 1) 

# view(duplicates)

# view(Locations_LUT)

# view(Access_data)


Access_data <- Access_data |>
  left_join(Locations_LUT, join_by(FeatureID))

glimpse(Access_data)

# view(Access_data)

# Show the HerbSiteName, FeatureID where there's no 
# match in LocationsID

Access_data |>
  select(HerbSiteName, FeatureID, LocationID) |>
  filter(is.na(LocationID)) |>
  distinct()

