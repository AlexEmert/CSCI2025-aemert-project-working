library(shiny)
library(bslib)
library(tidyverse)
library(here)

clean_data <- read_csv(here("Data/processed", "cleaned_data.csv"))


mtg_data_unique <- clean_data |>
  distinct(name, setCode, .keep_all = TRUE)


simulate_booster <- function(mtg_data_unique, set_code_input) {

  set_pool <- mtg_data_unique |>
      filter(setCode == set_code_input)

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


ui <- fluidPage(

  titlePanel("Simulate some Booster Packs!"),
  
  sidebarLayout(

    sidebarPanel(

      selectInput("setCodeChosen", label = "Select Set", 
                  choices = unique(mtg_data_unique$setCode)),

      actionButton("simulate_btn", "Simulate!", class = "btn-primary")
    ),

  mainPanel(
    tableOutput("simTable"),
    plotOutput("simPlot")
    )
  )
)

server <- function(input, output, session) {

  simulated_data <- eventReactive(input$simulate_btn, {
    # browser()
    selected_val <- input$setCodeChosen

    sim_results <- replicate(1000, {
      avg_pack <- simulate_booster(mtg_data_unique, selected_val)
      sum(avg_pack$price, na.rm = TRUE)
    }, simplify = TRUE)

    results_df <- tibble(sim_results)
    
    return(results_df)

  })

  "output$simTable <- renderTable ({
    head(simulated_data())
  })
"
  output$simPlot <- renderPlot ({
    ggplot(simulated_data(), aes(x = sim_results$avg_pack)) +
    geom_histogram(fill = "steelblue", color = "white") +
    geom_vline(xintercept = mean(total_value), color = "red", linetype = "dashed") +
    labs(title = "Distribution of Booster Pack Values",
         x = "Total Pack Value ($)",
         y = "Frequency")
  })

}

shinyApp(ui, server)