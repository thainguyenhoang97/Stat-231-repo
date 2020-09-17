library(fivethirtyeight)
library(shinythemes)
library(tidyverse)

#namecheck <- fivethirtyeight::mad_men %>%
#  mutate(performer2 = case_when(performer == "Alexander Skarsg\u008crd" ~ "Alexander Skarsgaard"
#                                , str_detect(performer, "Freddy Rodr") ~ "WOO HOO!"
#                                , TRUE ~ performer)
#         , performer3 = str_replace(performer, "[:xdigit:]", "WOOHOO")
#         , performer4 = str_replace(performer, "\\\\", "WOOHOO")
#         , performer5 = str_replace(performer, "[:digit:]", "WOOHOO")
#         , performer6 = stringi::stri_enc_toutf8(performer)
#         ) %>%
#  select(starts_with("performer"))

# not ideal: but can hard code using str_detect
# checking to make sure no others will be erroneously recorded 
# i.e., that there is only one match per str_detect
#length(which(str_detect(mad_men$performer, "Freddy Rodr")==TRUE))
#length(which(str_detect(mad_men$performer, "Laura Cer")==TRUE))
#length(which(str_detect(mad_men$performer, "Sevigny")==TRUE))
#length(which(str_detect(mad_men$performer, "Alexander Skarsg")==TRUE))

# fix wierd encodings
mad_men2 <- fivethirtyeight::mad_men %>%
  rename(performer_old = performer) %>%
  mutate(performer = case_when(str_detect(performer_old, "Freddy Rodr") ~ "Freddy Rodriguez"
                               , str_detect(performer_old, "Laura Cer") ~ "Cernn"
                               , str_detect(performer_old, "Sevigny") ~ "Chloe Sevigny"
                               , str_detect(performer_old, "Lauren V") ~ "Lauren Valez"
                               , str_detect(performer_old, "Alexander Skarsg") ~ "Alexander Skarsgaard"
                               , TRUE ~ performer_old))


# these can be vectors
# (only need to make as a named list *if* want the user to see something different)
show_choice <- unique(mad_men2$show)
perf_choice <- unique(mad_men2$performer)

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
                       , choices = mad_men2[mad_men2$show==input$x, "performer"])
   })
   
  output$table1 <- renderTable({
    dplyr::select(use_data1(), performer, show_start, show_end)
  })
  
  output$table2 <- renderTable({
    use_data2() %>%
      select(performer, num_lead, num_support, num_shows)
  })
  
}

shinyApp(ui = ui, server = server)