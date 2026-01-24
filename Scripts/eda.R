library(tidyverse)
library(scryr)

card_price_data <- read_csv("Data/raw/AllPrintingsCSVFiles/cardPrices.csv")
cards_data <- read_csv("Data/raw/AllPrintingsCSVFiles/cards.csv")
set_data <- read_csv("Data/raw/AllPrintingsCSVFiles/sets.csv")
set_booster_data <- read_csv("Data/raw/AllPrintingsCSVFiles/setBoosterContents.csv")

glimpse(set_booster_data)

llorwyn <- scry_cards("set:ECL") |>
  unnest(prices)


plot_data <- llorwyn |>
  select(cmc, usd, edhrec_rank, cmc) |>
  mutate(across(everything(), as.numeric)) |>
  filter(!is.na(cmc))

  ggplot(plot_data, aes(x=cmc, y = usd)) +
  geom_point()

