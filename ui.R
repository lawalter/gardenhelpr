# libraries ---------------------------------------------------------------

library(tidyverse)
library(shinyStore)

# ui ----------------------------------------------------------------------

# Read data 
clean_data <- read_csv("clean_data/plant_relationships.csv")

# Define UI for application:
ui <- 
  
  fluidPage(
    
    # CSS:
    includeCSS('www/main-css.css'),
    
    # Application title:
    fluidRow(
      column(
        width = 10, 
        tags$h1("Garden Helpr"))),
    
    # Sidebar with input options:
    sidebarLayout(
      sidebarPanel(
        
        # Initialize shinyStore:
        initStore("store", "shinyStore-mac-app"),
        
        # Colors checkbox input:
        checkboxGroupInput("plantVector",
                           "Fruits and vegetables:",
                           choices = NULL,
                           selected = NULL),
        
        actionButton("go", "Save choices", 
                     icon("crow", lib = "font-awesome"))),
      
      # Main panel:
      
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Friends & Foes", 
            br(),
            h4("Friends:"),
            fluidRow(
              column(width = 2, tableOutput("friend_list"))),
            br(),
            h4("Foes:"),
            fluidRow(
              column(width = 2, tableOutput("foe_list"))
            ),
            #br(),
            textOutput("combo_count"),
            br()
            #downloadButton("downloadData", "Download full .csv")
            ),
          tabPanel("pH", verbatimTextOutput("pH")),
          tabPanel("Sun", verbatimTextOutput("sun")),
          tabPanel("Water", verbatimTextOutput("water")),
          tabPanel("Dates", verbatimTextOutput("dates"))
          )
        )
    )
  )
  

# server ------------------------------------------------------------------

# Define functions

# Friends of plants chosen:
get_friends <-
  function(x, plants) {
    if(length(plants) > 0){
    x %>% 
      filter(plant %in% plants) %>% 
      filter(relationship == "Companions") %>% 
      select(-relationship) %>% 
      mutate(second_plant = str_flatten(second_plant, collapse = ", ")) %>% 
      distinct()}
    else{NULL}
  }

# Foes of plants chosen:
get_foes <-
  function(x, plants) {
    if(length(plants) > 0){
    x %>% 
      filter(plant %in% plants) %>% 
      filter(relationship == "Antagonists") %>% 
        select(-relationship) %>% 
        mutate(second_plant = str_flatten(second_plant, collapse = ", ")) %>%
        distinct()}
    else{NULL}
  }

# Define server logic:

server <- function(input, output, session) {
  
  updateCheckboxGroupInput(session, 'plantVector',
                       choices = unique(clean_data$plant))
  
  # Reactive:
  dataFriends <- reactive({
    get_friends(clean_data, input$plantVector)
  })
  
  dataFoes <- reactive({
    get_foes(clean_data, input$plantVector)
  })
  
  # Vector of plants chosen:
  output$plants <- renderText(input$plantVector)
  
  # Friend list:
  output$friend_list <-
    renderTable({dataFriends()},
        bordered = FALSE,
        striped = FALSE,
        hover = FALSE,
        colnames = FALSE)
  
  # Foe list:
  output$foe_list <-
    renderTable({dataFoes()},
                bordered = FALSE,
                striped = FALSE,
                hover = FALSE,
                colnames = FALSE)
  
  # # All random combinations in a download-friendly format:
  # combo_list_csv <- reactive({dataInput()})
  # 
  # # Download handler:
  # output$downloadData <- downloadHandler(
  #   filename = paste0("colorband_list-", Sys.Date(), ".csv"),
  #   content = function(file){write.csv(combo_list_csv(), file)})
  
  # Memory using shinyStore for color checkbox input:
  observe({
    if (input$go <= 0){
      # On initialization, set the values of the checkbox group
      # to the saved values.
      updateCheckboxInput(session, "plantVector", 
                          value = isolate(input$store)$plantVector)    
      return()
    }
    updateStore(session, "plantVector", isolate(input$plantVector))
  })
  
  # pH
  output$pH <- renderPrint({"pH"})
  
  # Sun
  output$sun <- renderPrint({"sun"})
  
  # Water
  output$water <- renderPrint({"water"})
  
  # Dates
  output$dates <- renderPrint({"Coming soon!"})
  
}

# run ---------------------------------------------------------------------

# Run the application:

shinyApp(ui = ui, server = server)
