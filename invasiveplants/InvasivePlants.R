
################################################################################
#
#  InvasivePlants.R
#
#  Gareth Rowell, 12/3/2024
#
#  Creates PeriodID and renames columns for import into tbl_FieldData
# 
#
#
################################################################################


library(tidyverse)

#################
#
# Step 1 - load spreadsheet csv file
#
#################


# load the Survey123 data

setwd("C:/Users/growell/HTLN-data-capture/invasiveplants")

getwd()


Access_data <- read_csv("CUVA_INP_2024_survey123_output_merged.csv")


#################
#
# Step 2 - select and rename columns, convert date to yyyy-mm-dd
#
#################


Access_data <- Access_data |> 
  select(MyDate, Site, Code, Cover, Notes) |>
  mutate( 
	LocationID = Site,
	Species = Code,
	CoverClass = Cover,
	Comments = Notes,
    MyDate = (MyDate <- as.Date(MyDate, format = "%m/%d/%Y"))
  )


#################
#
# Step 3 - Generate PeriodID from MyDate
#
#################

Access_data <- Access_data |>
  mutate(PeriodID = str_c( 'CUVAInvPla', MyDate)) |>
  mutate(PeriodID = str_replace_all(PeriodID, "-", "")) |>
  mutate(NumMonth = str_sub(PeriodID, start = 15L, end = -3L)) 

Months_LUT <- read_csv("Months_LUT.csv")

Access_data <- Access_data |>
  left_join(Months_LUT, join_by(NumMonth))

Access_data <- Access_data |>
  mutate(PeriodID_left = str_sub(PeriodID, start = 1L, end = -5)) |>
  mutate(PeriodID_right = str_sub(PeriodID, start = 17, end = -1)) 

Access_data <- Access_data |>
  mutate(PeriodID = str_c(PeriodID_left, TxtMonth, PeriodID_right))


Access_data <- Access_data |> 
  select(LocationID, PeriodID, Species, CoverClass, Comments)
  

##########
#
# Step 4 - Write to file
#
##########

writexl::write_xlsx(Access_data, "Load_InvPla_2024.xlsx")


