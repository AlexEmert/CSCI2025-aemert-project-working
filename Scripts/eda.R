library(tidyverse)
library(scryr)

ltr_commanders <- scry_cards("is:commander set:ltr") |>
  unnest(prices)
  

