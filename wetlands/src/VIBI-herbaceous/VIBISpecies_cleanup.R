
# Clean up the Herb species list

library(tidyverse)


HerbSpecies_LUT <- read_csv("tbl_Species_20230316.csv")

problems(HerbSpecies_LUT)

glimpse(HerbSpecies_LUT)

# Remove NAs from HerbSpecies column

HerbSpecies_LUT |>
  filter(is.na(HerbSpecies)) |>
  print(n = 88)

HerbSpecies_LUT <- HerbSpecies_LUT |>
  filter(!is.na(HerbSpecies))

HerbSpecies_LUT

glimpse(HerbSpecies_LUT)

# Show all duplicates

duplicates <- HerbSpecies_LUT |> 
  group_by(HerbSpecies, ) |> 
  summarize(
    n = n()
  ) |> 
  filter( n > 1) 

view(duplicates)


# Find out which duplicates are needed to join to Herb data

load_file1 <- read_csv("CUVA_VIBI_Herb1.csv")
problems(load_file1)
load_file2 <- read_csv("CUVA_VIBI_Herb2.csv")
problems(load_file2)
load_file3 <- read_csv("CUVA_VIBI_Herb3.csv")
problems(load_file3)

glimpse(load_file1)
glimpse(load_file2)
glimpse(load_file3)

Access_data <- bind_rows(load_file1,load_file2)

glimpse(Access_data)

Access_data <- bind_rows(Access_data,load_file3)

glimpse(Access_data)


Access_data |>
  inner_join(duplicates, join_by(HerbSpecies)) |>
  view()

# FRAPEN is the only duplicate 
# so edit that one in HerbSpecies_LUT
# so edit FRAPEN and
# rerun scripts and export to HerbSpecies_LUT

write_csv(HerbSpecies_LUT, "HerbSpecies_LUT2.csv")






