
################################################################################
#
#  VIBI_herb_final.R
#
#  Gareth Rowell, 4/26/2024
#
#  Generates loadfile for tbl_VIBI_herb and provides end2end
#  tests for exported table tbl_VIBI_herb after its been appended with the 
#  2023 data.
#
#
################################################################################


library(tidyverse)

#setwd("./VIBI-herbaceous")


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



#################
#
# Step 2 - select and rename columns, convert date to yyyy-mm-dd
#
#################


Access_data <- Access_data |> 
  select(Species, Comments, Module, CoverClass_LT_6m, 
         CoverClassAll, EditDate, HerbSiteName) |>
  mutate( 
    FeatureID = HerbSiteName,
    CoverClass = CoverClass_LT_6m,
    EditDate = (EditDate <- as.Date(EditDate, format = "%m/%d/%Y"))
  )

glimpse(Access_data)


#################
#
# Step 3a - Substitute NA with -9999 in CoverClass and CoverClassAll
#  Then remove those with -9999 in CoverClass
#
#################


Access_data$CoverClass <- Access_data$CoverClass |> replace_na(-9999)

Access_data$CoverClassAll <- Access_data$CoverClassAll |> replace_na(-9999)

Access_data |>
  filter(CoverClass == -9999)

Access_data <- Access_data |>
  filter(CoverClass != -9999)

# then test

Access_data |>
  filter(CoverClass == -9999)


##########
#
# Step 3b - check for duplicates
#
##########



# test for dups


Access_data |>
  count(Species, Comments, Module, CoverClass_LT_6m, CoverClassAll,
        EditDate, HerbSiteName, FeatureID, CoverClass
  ) |>
  filter(n > 1)

# Remove dups with distinct() 

Access_data <- Access_data |>
  distinct(Species, Comments, Module, CoverClass_LT_6m, CoverClassAll,
           EditDate, HerbSiteName, FeatureID, CoverClass
  )
Access_data

# test for dups


Access_data |>
  count(Species, Comments, Module, CoverClass_LT_6m, CoverClassAll,
        EditDate, HerbSiteName, FeatureID, CoverClass
  ) |>
  filter(n > 1)


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

# First test for NA's in FeatureID column

# subset(Access_data,is.na(FeatureID))

# there are no NAs in FeatureID column

# Locations_LUT <- read_csv("tbl_Locations_20230316.csv")

# glimpse(Locations_LUT)

# repeat above steps using corrected locations ---------------------------------

# First test for NA's in FeatureID column

subset(Access_data,is.na(FeatureID))

# there are no NAs in FeatureID column

Locations_LUT <- read_csv("tbl_Locations_fixed.csv")

glimpse(Locations_LUT)


Access_data <- Access_data |>
  left_join(Locations_LUT, join_by(FeatureID))

# test for NA's in LocationID

df <- subset(Access_data,is.na(LocationID))

glimpse(df)

# which FeatureIDs aren't matching up

df <- df |>
  distinct(LocationID, FeatureID)

df


Access_data <- Access_data |>
  filter(!is.na(LocationID))

# should be zero rows for the above tests once locations are fixed



#################
#
# Step 6 - Clean up columns and write load file
#
#################

# clean up columns

Access_data <- Access_data |>
  select(EventID, LocationID, FeatureID, Species, Comments, Module,
         CoverClass, CoverClassAll, EditDate )

#writexl::write_xlsx(Access_data, "Load_VIBI_herb_2023.xlsx")




#------------------------------------------------------------------------------
# End2End test begins here

end2end <- read_csv("qrye2e_VIBI_herb.csv")


glimpse(Access_data)

glimpse(end2end)

# matching column names

Access_data <- Access_data |>
  mutate(
    ModNo = Module,
    CovCode = CoverClass
  )

# select only shared columns

Access_data <- Access_data |>
  select(EventID, LocationID, FeatureID, Species, ModNo, CovCode, Comments
  )

glimpse(Access_data)

glimpse(end2end)

# removing voucherno and comments from end2end

end2end <- end2end |>
  select(EventID, LocationID, FeatureID, Species, ModNo, CovCode
  )

glimpse(Access_data)

glimpse(end2end)

# record counts match


Access_data |>
  count(EventID, LocationID, FeatureID, Species, ModNo, CovCode) |>
  filter(n > 1)


# Duplicates in Access_data caused by Carex sp.1, which should have been 
# Carex sp. 1 and sp. 2 just going to remove these to complete end2end testing

Access_data <- Access_data |>
  distinct(EventID, LocationID, FeatureID, Species, ModNo, CovCode) 


Access_data |>
  count(EventID, LocationID, FeatureID, Species, ModNo, CovCode) |>
  filter(n > 1)

glimpse(Access_data)

glimpse(end2end)

# return to record counts issue 
# Access_data n = 3994
# end2end n = 3835

# what are the record differences between these two dataframes

my_columns = c('EventID', 'LocationID', 'FeatureID', 'Species', 'ModNo', 'CovCode')

view(anti_join(Access_data, end2end, by=my_columns))

# all the extract Access_data records have the result of corrected locations
# verify this

df <- anti_join(end2end, Access_data, by=my_columns)

df |>
  distinct(FeatureID)

# The corrected locations for VIBI were 
# VIBI_herbaceous:
  
#   HerbSiteName FeatureID LocationID
# <chr>        <chr>     <chr>    database/geodatabase name:
#  1 242VK4       242VK4    NA        (241VK4)
#  2 1627KR2      1627KR2   NA        (1627KR)
#  3 1622KR1      1622KR1   NA        (1622KR)
#  4 1627KR1      1627KR1   NA         (1627KR)


# there are no records. So, need to filter out the N/A locations in the original
# Access_data / load file and repeat this process.

# example code
# flights2 |> 
#   anti_join(airports, join_by(dest == faa)) |> 
#   distinct(dest) 

# anti_join(df1, df2, by=c('team', 'position'))

# Selecting the columns that matter

Access_data <- Access_data |>
  select(EventID, LocationID, Species, ModNo, CovCode)

end2end <- end2end |>
  select(EventID, LocationID, Species, ModNo, CovCode)

# testing for PK - unique no-nulls

Access_data |>
  count(EventID, LocationID, Species, ModNo, CovCode) |>
  filter(n > 1)

end2end |>
  count(EventID, LocationID, Species, ModNo, CovCode) |>
  filter(n > 1)

# need to delete duplicates from database and reexport <<<<<<<<<<<<<<<<<<<
# manually delete duplicates from Access_data and retest

# compare record counts

Access_data |>
  count(EventID, LocationID, Species, ModNo, CovCode)

end2end |>
  count(EventID, LocationID, Species, ModNo, CovCode)

#view(end2end)












