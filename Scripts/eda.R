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
scryfall_cards <- scry_cards("type:goblin c:green") |>
  glimpse()
  ggplot(scryfall_cards, aes(x=set)) +
  geom_bar()

full_data <- cards_data |>
  left_join(card_price_data) |>
  filter(price < 200000) 

full_data <- full_data |>
  select(-asciiName, -attractionLights, -boosterTypes, -cardParts, -duelDeck, -faceConvertedManaCost, 
  -faceFlavorName, -faceManaValue, -faceName, -facePrintedName, -frameEffects, -hand, -hasAlternativeDeckLimit, 
  -isAlternative, -isStorySpotlight, -isTimeshifted, -otherFaceIds, -printedName, -printedType, -printedText, 
  -rebalancedPrintings, -relatedCards, -securityStamp, -side, -originalText)

full_data |>
  write_csv("data/processed/cleaned_data.csv")


full_data |>
  filter(edhrecRank <=100) |>
ggplot(aes(x=edhrecRank, y = price)) +
  geom_point()
  
full_data <- full_data |>
  mutate(color_type = if_else(str_detect(colors, ","), "Multicolored", colors))

full_data |>
  filter(edhrecSaltiness >=1) |>
  ggplot(aes(x=edhrecRank, y =edhrecSaltiness, color = color_type)) +
  geom_point() +
  scale_color_manual(values = c("B" = "black", "G" = "green", "U" = "blue", "R" = "red", "W" = "white", "Multicolored" = "salmon")) +
  geom_smooth(se=FALSE, method = "lm") 

library(plotly)

fig1 <- plot_ly(full_data, z=~volcano) |>
  add_surface()
fig1


#most common first names in artists
full_data |>
  select(artist) |>
  mutate(firstName = str_extract(artist, "^\\S+")) |>
  count(firstName) |>
  arrange(desc(n)) |>
  head()

