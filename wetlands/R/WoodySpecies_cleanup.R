
# Clean up the woody species list

library(tidyverse)


WoodySpecies_LUT <- read_csv("WoodySpecies_LUT.csv")

glimpse(WoodySpecies_LUT)

# Remove NAs from WoodySpecies column

WoodySpecies_LUT |>
  filter(is.na(WoodySpecies)) |>
  print(n = 88)

WoodySpecies_LUT <- WoodySpecies_LUT |>
  filter(!is.na(WoodySpecies))

WoodySpecies_LUT

glimpse(WoodySpecies_LUT)

# Show all duplicates

duplicates <- WoodySpecies_LUT |> 
  group_by(WoodySpecies, ) |> 
  summarize(
    n = n()
  ) |> 
  filter( n > 1) 

view(duplicates)


# Find out which duplicates are needed to join to woody data

load_file1 <- read_csv("CUVA_VIBI_woody1.csv")
problems(load_file1)
load_file2 <- read_csv("CUVA_VIBI_woody2.csv")
problems(load_file2)
load_file3 <- read_csv("CUVA_VIBI_woody3.csv")
problems(load_file3)

glimpse(load_file1)
glimpse(load_file2)
glimpse(load_file3)

Access_data <- bind_rows(load_file1,load_file2)

glimpse(Access_data)

Access_data <- bind_rows(Access_data,load_file3)

glimpse(Access_data)


Access_data |>
  inner_join(duplicates, join_by(WoodySpecies)) |>
  view()

# FRAPEN is the only duplicate 
# so edit that one in WoodySpecies_LUT
# so edit FRAPEN and
# rerun scripts and export to WoodySpecies_LUT

write_csv(WoodySpecies_LUT, "WoodySpecies_LUT2.csv")






