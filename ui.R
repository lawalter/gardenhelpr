# libraries ---------------------------------------------------------------

library(shinyStore)
library(fresh)

# ui ----------------------------------------------------------------------

# Read data 
clean_data <- readr::read_csv("clean_data/plant_relationships.csv")  

# Define UI for application:
ui <- 
  
  fluidPage(
    # CSS:
    includeCSS('www/freshtheme.css'),
    includeCSS('www/main-css.css'),
    
    # Application title:
    fluidRow(
      column(
        width = 10, 
        tags$h1("Gardenhelpr"))),
    
    # Sidebar with input options:
    sidebarLayout(
      sidebarPanel(
        
        # Initialize shinyStore:
        initStore("store", "shinyStore-mac-app"),
        
        # Plants checkbox input:
        list(h3("Choose your plants"),
             tags$div(align = 'left',
                      class = 'multicol',
                      checkboxGroupInput(inputId  = 'plantVector',
                                         label    = NULL,
                                         choices  = NULL,
                                         selected = NULL,
                                         inline   = FALSE))),
        
        actionButton("go", "Save choices", 
                     icon("carrot", lib = "font-awesome")),
        
        width = 5),
      
      # Main panel:
      
      mainPanel(
        h3("Friends & Foes"), 
          br(),
          fluidRow(plotOutput("ffPlot")),
        h4("Friends (comprehensive list):"),
          fluidRow(column(width = 10, tableOutput("friend_list"))),
          br(),
        h4("Foes (comprehensive list):"),
          fluidRow(column(width = 10, tableOutput("foe_list"))),
          br(),
          downloadButton("downloadData", "Download full list as .csv"),
          br(),
        width = 5
        )
      ),
      
    # Footer
    hr(),
    HTML(
      "<b>Created by:</b> Abby Walter
      <br><b>Repository:</b> <a href = 'https://github.com/lawalter/gardenhelpr'>https://github.com/lawalter/gardenhelpr</a></br></br>
      <p><i>Note:</i> American varieties were chosen over others when possible (e.g., pennyroyal).</p>"
      )
    
  )
  

# server ------------------------------------------------------------------

# Define functions

# Get friends or foes of plants chosen:
get_type <-
  function(x, plants, type) {
    if(length(plants) > 0){
      x |> 
        dplyr::filter(plant %in% plants) |> 
        dplyr::filter(relationship == type) |> 
        dplyr::select(-relationship) |> 
        dplyr::arrange(second_plant) |> 
        dplyr::group_by(plant) |> 
        dplyr::mutate(
          second_plant = 
            stringr::str_flatten(second_plant, collapse = ", ")) |>
        dplyr::ungroup() |> 
        dplyr::distinct()}
    else{ NULL }
  }

# Define server logic:

server <- function(input, output, session) {
  
  updateCheckboxGroupInput(session, 'plantVector',
                       choices = unique(clean_data$plant))
  
  # Reactive:
  dataFriends <- reactive({
    get_type(clean_data, input$plantVector, "Companions")
  })
  
  dataFoes <- reactive({
    get_type(clean_data, input$plantVector, "Antagonists")
  })
  
  # Vector of plants chosen:
  output$plants <- renderText(input$plantVector)
  
  # Plot of friends and foes:
  output$ffPlot <- 
    renderPlot({
      clean_data |> 
        dplyr::filter(plant %in% input$plantVector) |> 
        dplyr::filter(second_plant %in% plant) |> 
        ggplot2::ggplot(
          ggplot2::aes(x = second_plant, y = plant, fill = relationship)) +
        ggplot2::geom_tile() +
        ggplot2::coord_equal(expand = T) +
        ggplot2::labs(x = NULL, y = NULL) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          panel.grid = ggplot2::element_blank(),
          axis.text.x = 
            ggplot2::element_text(
              size = 15, angle = 45, hjust = 0.95),
          axis.text.y = 
            ggplot2::element_text(size = 15),
          axis.title.y = 
            ggplot2::element_text(size = 15),
          legend.position = "bottom",
          legend.title = ggplot2::element_blank(),
          legend.text = 
            ggplot2::element_text(size = 15),
          legend.direction = "vertical",
          plot.margin = 
            ggplot2::unit(c(0, 0, 0, 0), "cm")) +
        ggplot2::scale_fill_manual(
          values = c("Antagonists" = "#FDE725", "Companions" = "#7AD151"))
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
  
  # All plants & their friends/does in a download-friendly format:
  combo_list_csv <- 
    reactive({
      dplyr::bind_rows(
        get_type(clean_data, input$plantVector, "Companions") |> 
          dplyr::mutate(type = "friends"),
        get_type(clean_data, input$plantVector, "Antagonists") |> 
          dplyr::mutate(type = "foes")) |> 
        dplyr::rename(list = second_plant) |> 
        dplyr::relocate(type, .before = "list") |> 
        dplyr::arrange(plant)})

  # Download handler:
  output$downloadData <- 
    downloadHandler(
      filename = 
        function(){paste0("plant_list-", Sys.Date(), ".csv")},
      content = 
        function(file){write.csv(combo_list_csv(), file, row.names = F)})
  
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
  
}

# run ---------------------------------------------------------------------

# Run the application:

shinyApp(
  ui = ui, 
  server = server, 
  options = list(height = 1080))
