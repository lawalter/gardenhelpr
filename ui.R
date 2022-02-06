# libraries ---------------------------------------------------------------

library(tidyverse)
library(shinyStore)

# ui ----------------------------------------------------------------------

# Read data 
clean_data <- read_csv("clean_data/plant_relationships.csv")  

# Note
# simplify beans?

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
            fluidRow(plotOutput("ffPlot")),
            h4("Friends (comprehensive list):"),
            fluidRow(
              column(width = 10, tableOutput("friend_list"))),
            br(),
            h4("Foes (comprehensive list):"),
            fluidRow(
              column(width = 10, tableOutput("foe_list"))
            ),
            br()
            #downloadButton("downloadData", "Download full .csv")
            ),
          tabPanel(
            "pH", 
            h4("pH:"),
            br(),
            verbatimTextOutput("pH")),
          tabPanel(
            "Sun", 
            h4("Sun:"),
            br(),
            verbatimTextOutput("sun")),
          tabPanel(
            "Water", 
            h4("Water:"),
            br(),
            verbatimTextOutput("water")),
          tabPanel(
            "About", 
            h4("About:"),
            br(),
            h5("American varieties were chosen over others when possible (e.g., pennyroyal)."),
            br(),
            verbatimTextOutput("about"))
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
        arrange(second_plant) %>% 
        group_by(plant) %>% 
        mutate(second_plant = str_flatten(second_plant, collapse = ", ")) %>%
        ungroup() %>% 
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
        arrange(second_plant) %>% 
        group_by(plant) %>% 
        mutate(second_plant = str_flatten(second_plant, collapse = ", ")) %>%
        ungroup() %>% 
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
  
  # Plot of friends and foes:
  output$ffPlot <- 
    renderPlot({
      clean_data %>% 
        filter(plant %in% input$plantVector) %>% 
        filter(second_plant %in% plant) %>% 
        ggplot(aes(x = second_plant, y = plant, fill = relationship)) +
        geom_tile() +
        coord_equal(expand = T) +
        labs(x = NULL, y = NULL) +
        theme_minimal() +
        theme(
          panel.grid = element_blank(),
          axis.text.x = element_text(family = "mono"),
          axis.text.y = element_text(family = "mono"),
          axis.title.y = element_text(family = "mono"),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.direction = "vertical",
          plot.margin = unit(c(0, 0, 0, 0), "cm")) +
        scale_fill_manual(
          labels = c("Companions", "Antagonists"),
          values = c("#7AD151", "#FDE725"))
    })
  
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
  
  # About
  output$about <- renderPrint({"Coming soon!"})
  
}

# run ---------------------------------------------------------------------

# Run the application:

shinyApp(ui = ui, server = server)
