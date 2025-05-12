library(shiny)
library(bslib)
library(tidyverse)
library(DT)
library(jsonlite)
library(httpuv)
source("getData.R")

# Define UI
ui <- page_fluid(
  title = "Boat Ramp Information Dashboard",
  
  # Header
  card(
    full_screen = TRUE,
    card_header("Boat Ramp Elevation Dashboard"),
    
    # Main content
    layout_sidebar(
      sidebar = sidebar(
        title = "Select Lake",
        selectInput("lakeSelect", "Choose a Lake:", 
                    choices = unique(myData$name), 
                    selected = unique(myData$name)[1])
      ),
      
      # Main panel with tabs
      navset_card_tab(
        nav_panel(
          "Lake Data Table",
          div(style = "margin-top: 15px;"),
          DTOutput("tableData")
        ),
        nav_panel(
          "Lake Visualization",
          div(style = "margin-top: 15px;"),
          fluidRow(
            column(
              width = 6,
              card(
                card_header("Lake Image"),
                imageOutput("lakeImage")
              )
            ),
            column(
              width = 6,
              card(
                card_header("Ramp Status"),
                plotOutput("rampPlot", height = "500px")
              )
            )
          )
        ),
        nav_panel(
          "API Information",
          div(style = "margin-top: 15px;"),
          card(
            card_header("API Endpoints"),
            tags$ul(
              tags$li(tags$strong("Elevation Data (JSON):"), 
                      tags$code("api/elevation/<wbCode>")),
              tags$li(tags$strong("Ramp Plot Image (PNG):"), 
                      tags$code("api/plot/<wbCode>"))
            ),
            textOutput("apiBaseUrl")
          )
        )
      )
    )
  )
)

# Custom HTTP handler for API endpoints
httpHandler <- function(req) {
  path <- req$PATH_INFO
  browser()
  # Handle elevation data API
  if (grepl("^/api/elevation/", path)) {
    wbCode <- sub("^/api/elevation/", "", path)
    
    # Filter data for the requested wbCode
    lake_data <- myData %>%
      filter(wbCode == wbCode) %>%
      select(name, elevation, updatedDate, r.cp, r.name, r.bottom, r.top)
    
    if (nrow(lake_data) > 0) {
      json_data <- toJSON(lake_data, auto_unbox = TRUE, pretty = TRUE)
      return(list(
        status = 200,
        headers = list("Content-Type" = "application/json"),
        body = json_data
      ))
    } else {
      return(list(
        status = 404,
        headers = list("Content-Type" = "text/plain"),
        body = "Lake not found"
      ))
    }
  }
  
  # Handle plot image API
  if (grepl("^/api/plot/", path)) {
    wbCode <- sub("^/api/plot/", "", path)
    
    # Filter data for the requested wbCode
    lake_data <- myData %>%
      filter(wbCode == wbCode)
    
    if (nrow(lake_data) > 0) {
      # Create a temporary file for the plot
      temp_file <- tempfile(fileext = ".png")
      
      # Save the plot to the temp file
      png(temp_file, width = 800, height = 600)
      print(plotRamp(lake_data))
      dev.off()
      
      # Read the file content
      plot_content <- readBin(temp_file, "raw", file.info(temp_file)$size)
      
      # Remove the temp file
      unlink(temp_file)
      
      return(list(
        status = 200,
        headers = list("Content-Type" = "image/png"),
        body = plot_content
      ))
    } else {
      return(list(
        status = 404,
        headers = list("Content-Type" = "text/plain"),
        body = "Lake not found"
      ))
    }
  }
  
  # If not an API call, continue with normal Shiny processing
  return(NULL)
}

# Define server
server <- function(input, output, session) {
  
  # Display the base URL for API usage
  output$apiBaseUrl <- renderText({
    paste0("Base URL: ", session$clientData$url_protocol, "//", 
           session$clientData$url_hostname, ":", 
           session$clientData$url_port)
  })
  
  # Create a summary table of lake data
  lake_summary <- reactive({
    myData %>%
      group_by(name) %>%
      summarize(
        `Conservation Pool` = first(r.cp),
        `Current Elevation` = first(elevation),
        `Data Source` = first(Source),
        `Updated Date` = first(as.character(updatedDate))
      ) %>%
      ungroup()
  })
  
  # Render the data table
  output$tableData <- renderDT({
    datatable(lake_summary(), 
              options = list(pageLength = 10),
              rownames = FALSE)
  })
  
  # Get filtered data for the selected lake
  selected_lake_data <- reactive({
    myData %>%
      filter(name == input$lakeSelect)
  })
  
  # Render the lake image
  output$lakeImage <- renderImage({
    lake_data <- selected_lake_data()
    if(nrow(lake_data) > 0 && !is.na(lake_data$pic[1])) {
      image_path <- file.path("./pics", lake_data$pic[1])
      # Check if image exists, if not use a placeholder
      if(!file.exists(image_path)) {
        image_path <- "www/no_image.jpg"
      }
    } else {
      image_path <- "www/no_image.jpg"
    }
    
    list(src = image_path,
         contentType = "image/jpeg",
         width = "100%",
         alt = paste("Image of", input$lakeSelect))
  }, deleteFile = FALSE)
  
  # Render the ramp plot
  output$rampPlot <- renderPlot({
    lake_data <- selected_lake_data()
    if(nrow(lake_data) > 0) {
      plotRamp(lake_data)
    }
  })
}

# Create Shiny app with custom HTTP handler
shinyApp(
  ui = ui, 
  server = server,
  options = list(port = 3838),
  onStart = function() {
    cat("Custom handler enabled for API endpoints\n")
    addResourcePath("api", ".")
  }#,
  #httpHandler = httpHandler
)