
library(tidyverse)

# Example from RS4DS-2e Section 6.3 Lengthening data

# Your data are in columns
# The sample data are


billboard

glimpse(billboard)

# To pivot longer

billboard |> 
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank"
  )

billboard |> 
  pivot_longer(
    cols = starts_with("wk"), 
    names_to = "week", 
    values_to = "rank",
    values_drop_na = TRUE
  )

