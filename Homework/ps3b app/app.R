library(fivethirtyeight)
library(shinythemes)
library(tidyverse)



# fix wierd encodings
mad_men2 <- fivethirtyeight::mad_men %>%
  rename(performer_old = performer) %>%
  mutate(performer = case_when(str_detect(performer_old, "Freddy Rodr") ~ "Freddy Rodriguez"
                               , str_detect(performer_old, "Laura Cer") ~ "Laura Cernn"
                               , str_detect(performer_old, "Sevigny") ~ "Chloe Sevigny"
                               , str_detect(performer_old, "Lauren V") ~ "Lauren Valez"
                               , str_detect(performer_old, "Alexander Skarsg") ~ "Alexander Skarsgaard"
                               , TRUE ~ performer_old))

# Make actors/actresses status uppercase
mad_men2$status = tolower(mad_men2$status)


# these can be vectors
# (only need to make as a named list *if* want the user to see something different)
show_choice <- unique(mad_men2$show)
perf_choice <- unique(mad_men2$performer)

ui <- fluidPage(
  h1("How are the actors/actresses of your favorite TV shows doing?"),
  sidebarLayout(
    sidebarPanel(
 
      selectInput(inputId = "x"
               , label = "Choose a show of interest"
               , choices = show_choice
                 ),
      
      conditionalPanel(condition = "input.x != 0"
                       , selectInput(inputId = "y"
                                     , label = "Choose a performer of interest"
                                     , choices = perf_choice
                                     , selected = NULL)
      ),
      
      h4("Click confirm if you want to learn about this actor/actress"),
      
      actionButton(inputId = "button", label = "Confirm")
    ),
    
  mainPanel(
    fluidRow(
      splitLayout(cellWidths = c("20%", "80%")
                  , tableOutput("table1")
                  , conditionalPanel(condition = "input.button > 0"
                                     , textOutput(outputId = "text")
                                     , textOutput(outputId = "text1")
                                     )
      ),
    ),
    fluidRow()
    )
  )
)

server <- function(input, output, session) {
  use_data1 <- reactive({
    filter(mad_men2, show == input$x)
  })
  
  use_data2 <- reactive({
    filter(mad_men2, show == input$x & performer == input$y)
  })
 
   observe({
     updateSelectInput(session, inputId = "y"
                       , label = paste("Choose a performer from", input$x)
                       , choices = mad_men2[mad_men2$show==input$x, "performer"]
                       )
   })
   
   observeEvent(input$x, {input$button == 0})
   
  output$table1 <- renderTable({
    use_data1() %>%
      select(performer) %>%
      rename("Performers list" = performer)
                               }
    )
  
  output$text <- renderText({
    paste("Since", input$y, use_data2()$status, "his/her role in", 
          input$x, use_data2()$years_since, "years ago, he/she has appeared as a lead in",
          use_data2()$num_lead, "show(s),")
  })
  
  output$text1 <- renderText({
    paste("as support in", use_data2()$num_support,
          "show(s), and played an integral part in", use_data2()$num_shows, "show(s).")
  }
    
  )
}

shinyApp(ui = ui, server = server)