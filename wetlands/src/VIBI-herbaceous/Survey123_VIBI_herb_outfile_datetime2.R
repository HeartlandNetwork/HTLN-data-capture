
library(tidyverse)

#setwd("./src")

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

# record count is 1731. 
# From the spreadsheets: 304 + 280 + 1147 = 1731

# select columns from Survey 123 and create FeatureID column

Access_data <- Access_data |> 
  select(Species, SpeciesComments, Module, CoverClass_LT_6m, 
         CoverClassAll, EditDate, HerbSiteName) |>
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

# create the LocationID column from the FeatureID column
# and a lookup table from HTLNWetlands
# ERROR - FeatureIDs not joining 


Locations_LUT <- read_csv("tbl_Locations_20230316.csv")

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
    Outfile = str_c("VIBI_herb_", My_timestamp,".csv")
  ) 

glimpse(Access_data)

# clean up columns

Access_data <- Access_data |>
  select(EventID, FeatureID, LocationID, Species, SpeciesComments, Module,
         CoverClass, CoverClassAll, EditDate, Outfile )

glimpse(Access_data)


write_csv(Access_data, Access_data$Outfile[1])

writexl::write_xlsx(Access_data, "Load_VIBI_herb_2023.xlsx")
  



