
library(tidyverse)

#setwd("./src")

# load the Survey123 data

load_file <- read_csv("CUVA_VIBI_herb.csv")

glimpse(load_file)


# select columns from Survey 123 and create FeatureID column

Access_data <- load_file |> 
  select(Species, Module, CoverClass, CoverClassAll, EditDate, HerbSiteName) |>
  mutate( FeatureID = HerbSiteName) |>
  mutate(EditDate = (EditDate <- as.Date(EditDate, format = "%m/%d/%Y")))

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

Locations_LUT <- read_csv("Locations_LUT.csv")

glimpse(Locations_LUT)

Access_data <- Access_data |>
  left_join(Locations_LUT, join_by(FeatureID))

glimpse(Access_data)

# create an output file with a timestamp
# and include it in the dataframe for reference


Access_data <- Access_data |>
  mutate(
    My_timestamp = now(),
    My_timestamp = as.character(My_timestamp),
    My_timestamp = str_replace_all(My_timestamp, " ", "_"),
    My_timestamp = str_replace_all(My_timestamp, ":", ""),
    Timestamp = str_c("VIBI_herb_", My_timestamp)
  ) 

glimpse(Access_data)

# clean up columns

Access_data <- Access_data |>
  select(EventID, FeatureID, LocationID, Species, Module,
         CoverClass, CoverClassAll, EditDate, Timestamp )

glimpse(Access_data)


#write_csv(Access_data, Access_data$Outfile[1])

writexl::write_xlsx(Access_data, "Load_VIBI_herb.xlsx")
  



