# Load libraries
library(shiny)
library(tidyverse)
library(kableExtra)

# Define UI
ui <- fluidPage(
  titlePanel("Alternate Advantage/Disadvantage Rolling"),
  
  # All content in main panel style
  h2("Background"),
  p("The fifth edition of Dungeons & Dragons introduced a system of 'advantage' and 'disadvantage.' 
     When you roll a die with advantage, you roll twice and keep the higher result. 
     Rolling with disadvantage is similar, except you keep the lower result. 
     When both advantage and disadvantage apply, they cancel out, and you roll a single die."),
  
  p("There are two other, more mathematically interesting ways that advantage and disadvantage could be combined:"),
  tags$ul(
    tags$li("Advantage of Disadvantage: roll twice with disadvantage, then keep the higher result"),
    tags$li("Disadvantage of Advantage: roll twice with advantage, then keep the lower result")
  ),
  
  p("With a fair 20-sided die, which situation produces the highest expected roll: 
     advantage of disadvantage, disadvantage of advantage, or rolling a single die?"),
  
  p("Extra Credit: Instead of maximizing your expected roll, suppose you need to roll N or better with your 20-sided die. 
     For each value of N, is it better to use advantage of disadvantage, disadvantage of advantage, or rolling a single die?"),
  
  hr(),
  
  h3("Simulation and Results"),
  p("We simulate rolls based on the number of simulations selected with the slider. 
     For each roll, we compute the outcomes of both advantage-on-disadvantage and disadvantage-on-advantage rules."),
  
  h4("Summary of Advantage/Disadvantage Rolls"),
  tableOutput("summaryTable"),
  
  h4("Histogram of Roll Outcomes (Side-by-Side)"),
  plotOutput("histPlot"),
  
  # Slider placed under the histogram
  sliderInput(
    "N",
    "Number of Simulations (N):",
    min = 10,
    max = 500000,
    value = 2000,
    step = 10
  ),
  
  br(),
  h4("Dominant Rule by Roll Value"),
  tableOutput("dominantTable"),
  
  p("Winner: Disadvantage-on-Advantage. This may seem counterintuitive, but the simulations consistently show this outcome."),
  
  p("TODO: You can explore the divergence of distributions further by adjusting the number of simulations with the slider above.")
)

# Define server
server <- function(input, output, session) {
  
  # Reactive expression: simulate rolls based on slider N
  outcomes_table <- reactive({
    N <- input$N
    
    # Simulate rolls
    roll_table <- tibble(
      die_1 = sample(1:20, 2 * N, replace = TRUE),
      die_2 = sample(1:20, 2 * N, replace = TRUE),
      advantage = pmax(die_1, die_2),
      disadvantage = pmin(die_1, die_2),
      roll = rep(1:N, each = 2)
    )
    
    # Aggregate per roll
    roll_table %>%
      group_by(roll) %>%
      summarize(
        adv_on_disadv = max(disadvantage),
        disadv_on_adv = min(advantage),
        .groups = "drop"
      )
  })
  
  # Summary table: means
  output$summaryTable <- renderTable({
    outcomes_table() %>%
      summarize(
        adv_on_disadv_mean = mean(adv_on_disadv),
        disadv_on_adv_mean = mean(disadv_on_adv)
      )
  })
  
  # Histogram: side-by-side bars without black borders
  output$histPlot <- renderPlot({
    outcomes_table_long <- outcomes_table() %>%
      pivot_longer(
        cols = c(adv_on_disadv, disadv_on_adv),
        names_to = "advantage_rule"
      )
    
    ggplot(outcomes_table_long, aes(x = value, fill = advantage_rule)) +
      geom_histogram(position = "dodge", binwidth = 1) +  # No black border
      scale_fill_manual(
        values = c("adv_on_disadv" = "skyblue", "disadv_on_adv" = "salmon"),
        labels = c("Adv. on Disadv.", "Disadv. on Adv.")
      ) +
      labs(x = "Roll Value", y = "Count", fill = "Rule") +
      theme_minimal()
  })
  
  # Dominant rule table
  output$dominantTable <- renderTable({
    outcomes_table_long <- outcomes_table() %>%
      pivot_longer(
        cols = c(adv_on_disadv, disadv_on_adv),
        names_to = "advantage_rule"
      )
    
    outcomes_table_long %>%
      group_by(value) %>%
      summarize(
        `% A on D` = mean(advantage_rule == "adv_on_disadv"),
        `% D on A` = mean(advantage_rule == "disadv_on_adv"),
        `Dominant Rule` = if_else(`% A on D` > `% D on A`, "Adv. on Disadv.", "Disadv. on Adv."),
        .groups = "drop"
      ) %>%
      rename(`Required Roll Value` = value)
  }, digits = 3)
}

# Run the Shiny app
shinyApp(ui, server)
