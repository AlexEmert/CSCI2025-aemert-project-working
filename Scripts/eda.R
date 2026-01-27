library(tidyverse)
library(scryr)
library(plotly)


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

clean_data <- read_csv("Data/processed/cleaned_data.csv")

clean_data |>
  filter(edhrecRank <=100) |>
ggplot(aes(x=edhrecRank, y = price)) +
  geom_point()
  
clean_data|>
  mutate(color_type = if_else(str_detect(colors, ","), "Multicolored", colors)) |>
  filter(edhrecSaltiness >=1) |>
  ggplot(aes(x=edhrecRank, y =edhrecSaltiness, color = color_type)) +
  geom_point() +
  scale_color_manual(values = c("B" = "black", "G" = "green", "U" = "blue", "R" = "red", "W" = "white", "Multicolored" = "salmon")) +
  geom_smooth(se=FALSE, method = "lm") 

fig1 <- plot_ly(clean_data, z=~volcano) |>
  add_surface()
fig1


#most common first names in artists
clean_data |>
  mutate(color_type = if_else(str_detect(colors, ","), "Multicolored", colors)) |>
  select(artist, color_type) |>
  mutate(firstName = str_extract(artist, "^\\S+")) |>
  count(firstName)

clean_data |>
  mutate(firstName = str_extract(artist, "^\\S+")) |>
  group_by(firstName) |>
  summarise(meanPrice = mean(price, na.rm = TRUE),
            card_count= n()) |>
  filter(card_count >=10) |>
  arrange(desc(meanPrice)) |>
  head() |>
  ggplot(aes(x=firstName, y = meanPrice)) +
  geom_boxplot()

clean_data <- clean_data |>
  mutate(firstName = str_extract(artist, "^\\S+")) |>
  group_by(firstName) |>
  mutate(meanPrice = mean(price, na.rm = TRUE))

clean_data |>
  group_by(firstName) |>
  filter(meanPrice >=200) |>
  ggplot(aes(x=firstName, y = meanPrice)) +
  geom_violin()





#probably going in app
mtg_data_unique <- clean_data |>
  distinct(name, setCode, .keep_all = TRUE)

simulate_booster <- function(mtg_data_unique, set_code_input) {

  set_pool <- mtg_data_unique |>
      filter(set_code_input == setCode)

  commons <- set_pool  |>
    filter(rarity == "common")
  uncommons <- set_pool |>
    filter(rarity =="uncommon")
  rares <- set_pool |> 
    filter(rarity == "rare")
  mythics <- set_pool |> 
    filter(rarity == "mythic")

  rare_slot_type <- sample(c("rare", "mythic"), size = 1, prob = c(7/8, 1/8))

  if (rare_slot_type == "rare") {
      my_rare <- rares |>
        sample_n(1)
    } else {
      my_rare <- mythics |>
        sample_n(1)
    }

  my_commons <- commons |>
    slice_sample(n=8)
  my_uncommons <- uncommons |>
    slice_sample(n=3)

  pack <- bind_rows(my_commons, my_uncommons, my_rare)
  
  return(pack)
}

my_pack <- simulate_booster(mtg_data_unique, "LTR")
my_pack |>
  select(name, rarity) |>
  print()

sim_results <- replicate(1000, {
  avg_pack <- simulate_booster(mtg_data_unique, "MH3")
  sum(avg_pack$price, na.rm = TRUE)
}, simplify = TRUE)

results_df <- tibble(total_value = sim_results)

ggplot(results_df, aes(x = total_value)) +
  geom_histogram(fill = "steelblue", color = "white") +
  geom_vline(xintercept = mean(sim_results), color = "red", linetype = "dashed") +
  labs(title = "Distribution of Booster Pack Values",
       x = "Total Pack Value ($)",
       y = "Frequency")
