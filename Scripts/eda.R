library(tidyverse)
library(scryr)

ltr_commanders <- scry_cards("is:commander set:ltr") |>
  unnest(prices)
  
ltr_commmanders_df <- as.data.frame(ltr_commanders) |>
  select(usd, cmc)
 
  ggplot(ltr_commmanders_df, aes(x=cmc, y=usd)) +
  geom_point()
