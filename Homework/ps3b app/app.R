library(fivethirtyeight)
library(shinythemes)
library(tidyverse)

data("mad_men")
summary(mad_men)
show_choice <- as.list(mad_men$show)

ui <- fluidPage(
  h1("Actors of your favorite TV shows"),
  #Create drop down list for choosing shows
  selectInput(inputId = "show_choice"
              , label = "Choose a show of interest"
              , choices = show_choice
              , selected = "Game of Thrones"
              ),
  #Create action button to confirm interested show
  actionButton(inputId = "button"
               , label = "Ready"),
  
  
  tableOutput(outputId = "table")
)

server <- function(input, output) {
  use_data <- reactive({
    data <- filter(mad_men, show == input$show_choice)
  })
  
  output$table <- renderTable({
    dplyr::select(use_data(), input$show_choice, performer, show_start, show_end)
  })
  
}

shinyApp(ui = ui, server = server)