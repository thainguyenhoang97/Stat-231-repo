library(fivethirtyeight)
library(shinythemes)
library(tidyverse)


data("mad_men")
summary(mad_men)

  
show_choice <- as.list(mad_men$show)
perf <- as.list(mad_men$performer)

#namecheck <- mad_men %>%
 # filter(str_detect(performer, "\\") )

#mad_men_new <- mad_men %>%
 # mutate(name_new = stringi::stri_enc_toutf8(performer))

ui <- fluidPage(
  h1("Actors of your favorite TV shows"),
  #Create drop down list for choosing shows
  selectInput(inputId = "x"
              , label = "Choose a show of interest"
              , choices = show_choice
              ),
  
  h4("Press confirm when you are sure with this choice of show"),
  
  actionButton(inputId = "button", label = "Confirm"),
  
  conditionalPanel(condition = "input.button > 0"
                   , selectInput(inputId = "y"
                                 , label = "Choose a performer of interest"
                                 , choices = perf_choice)
  ),
  
  tableOutput(outputId = "table1"),
  tableOutput(outputId = "table2")
)

server <- function(input, output) {
  use_data1 <- reactive({
    data <- filter(mad_men, show == input$x)
  })
  
  
  use_data2 <- reactive({
    data <- filter(mad_men2, performer == input$y)
  })
  
  mad_men2 <- mad_men %>%
    filter(show == input$x)
  
  perf_choice <- as.list(mad_men2$performer)
  
  output$table1 <- renderTable({
    dplyr::select(use_data1(), performer, show_start, show_end)
  })
  
  output$table2 <- renderTable({
    dplyr::select(use_data2(), performer, num_lead, num_support, num_shows)
  })
  
}

shinyApp(ui = ui, server = server)