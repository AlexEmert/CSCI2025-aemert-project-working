library(tidyverse)
library(scryr)

card_price_data <- read_csv("Data/raw/AllPrintingsCSVFiles/cardPrices.csv")
cards_data <- read_csv("Data/raw/AllPrintingsCSVFiles/cards.csv")
set_data <- read_csv("Data/raw/AllPrintingsCSVFiles/sets.csv")
set_booster_data <- read_csv("Data/raw/AllPrintingsCSVFiles/setBoosterContents.csv")

glimpse(set_booster_data)


#scryr package exploration
llorwyn <- scry_cards("set:ECL") |>
  unnest(prices)

plot_data <- llorwyn |>
  select(cmc, usd, edhrec_rank, cmc) |>
  mutate(across(everything(), as.numeric)) |>
  filter(!is.na(cmc))

plot_data |>
  filter(usd>=5)|>
  ggplot(aes(x=cmc, y = usd)) +
  geom_point()

#new approach
scryfall_cards <- scry_cards("is:commander") |>
  glimpse()

scryfall_cards <- scryfall_cards |>
  unnest(prices)

scryfall_cards_sets <- scryfall_cards |>
  select(set)

  ggplot(scryfall_cards_sets, aes(x=set)) +
  geom_bar()
