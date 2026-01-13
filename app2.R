library(shiny)
library(shinydashboard)
library("dplyr")
library("tidyr")
library(readxl)
library(lubridate)
library(ggplot2)
library(base64enc)
library(stringr)

# Source your getData.R file which builds myData, plotRamp, etc.
source("getData.R")  # This file should load 'wb', 'ramps', 'myData', 'plotRamp', etc.

# Build a table of current elevations per waterbody
elevationsTable <- myData %>%
  group_by(name) %>%
  summarize(
    "Conservation Pool" = first(r.cp),
    "Current Elevation" = first(elevation),
    "Source" = first(Source),
    wbCode = first(wbCode),
    "Last Updated" = first(updatedDate)
  ) %>%
  ungroup()

ui <- dashboardPage(
  dashboardHeader(
    title = "Boat Ramp Conditions",
    # Add a custom link to the header for the Data Table
    tags$li(class = "dropdown",
            actionLink("showData", "Data Table", icon = icon("table"))
    )
  ),
  dashboardSidebar(disable = TRUE), # Disable the sidebar
  dashboardBody(
    # Include the Panzoom JS library
    tags$head(
      tags$script(src = "https://unpkg.com/@panzoom/panzoom/dist/panzoom.min.js")
    ),
    # Custom CSS
    tags$style(HTML("
      /* Panzoom Container Styles - Fixed Height */
      .panzoom-parent {
        width: 100%;
        height: 600px; /* Equal height */
        overflow: hidden;
        border: 1px solid #ddd;
        background-color: #f0f0f0;
        position: relative;
      }
      .panzoom-content {
        width: 100%; 
        height: 100%;
      }
      .panzoom-content img {
        width: 100%;
        height: 100%;
        object-fit: contain;
        display: block;
      }
      
      /* Ramp Plot Container - fixed height, scrollable, background */
      .ramp-scroll-container {
        height: 600px; /* Equal height */
        overflow-y: auto;
        border: 1px solid #a0a0a0; /* Slightly darker border for visibility */
        padding: 10px;
        background-color: #f9f9f9; /* Light gray background */
      }
      .ramp-plot-card {
        margin-bottom: 40px; 
        border: 1px solid #ddd;
        padding: 5px;
        border-radius: 5px;
        background-color: #fff;
        box-shadow: 0 1px 3px rgba(0,0,0,0.1); /* Subtle shadow */
      }
      
      /* General Styles */
      .box-header { font-weight: bold; }
      .content-wrapper { background-color: #ecf0f5; }
      
      /* Top Card Flex Alignment */
      .top-card-flex {
        display: flex;
        align-items: center; /* Vertically align items */
        flex-wrap: wrap;
        gap: 20px;
      }
      
      /* Fix Shiny Input Margin in Flex container */
      .top-card-flex .form-group {
        margin-bottom: 0px !important;
      }
      
      .stats-container {
        display: flex;
        flex-wrap: wrap;
        gap: 20px;
        align-items: center;
        font-size: 16px;
      }
      .stat-item {
        /* No extra padding needed with gap */
      }
      
      /* Responsive Helpers - Ensure columns have padding when stacked/grid */
      .col-lg-8, .col-lg-4 {
        padding-left: 15px;
        padding-right: 15px;
      }
    ")),
    
    # --- Stacked Layout ---
    
    # 1. Top Card: Lake (Full Width)
    fluidRow(
      column(width = 12,
             box(
               title = "Lake",
               status = "primary",
               solidHeader = TRUE,
               width = NULL,
               div(class = "top-card-flex",
                   div(style = "min-width: 250px;",
                       selectInput("selectedLake", label = NULL, choices = sort(unique(myData$name)))
                   ),
                   uiOutput("topLakeStats")
               )
             )
      )
    ),
    
    # 2. Main Visuals Row: Map (2/3) + Ramps (1/3) on Large; Stack on Medium
    fluidRow(
      # Map Card - Info Status (Light Blue)
      div(class = "col-lg-8",
          box(
            title = "Ramps",
            status = "info", 
            solidHeader = TRUE,
            width = NULL,
            div(class = "panzoom-parent", id = "panzoom-parent",
                div(class = "panzoom-content", id = "panzoom-element",
                    imageOutput("lakePic", height = "100%")
                )
            ),
            helpText("Scroll to zoom, drag to pan. Double-click to reset.")
          )
      ),
      
      # Ramp Plots Card - Info Status (Light Blue)
      div(class = "col-lg-4",
          box(
            title = "Ramp Status",
            status = "info",
            solidHeader = TRUE,
            width = NULL,
            div(class = "ramp-scroll-container",
                uiOutput("rampPlotsList")
            )
          )
      )
    ),
    
    # Custom JavaScript for Panzoom
    tags$script(HTML("
      var panzoomInstance;
      
      function initPanzoom() {
        var elem = document.getElementById('panzoom-element');
        var parent = document.getElementById('panzoom-parent');
        
        if (elem && parent) {
          if (panzoomInstance) { panzoomInstance.destroy(); }
          
          panzoomInstance = Panzoom(elem, {
            maxScale: 10,
            contain: 'outside',
            startTransform: 'scale(1) translate(0px, 0px)'
          });
          
          parent.addEventListener('wheel', panzoomInstance.zoomWithWheel);
          
          parent.addEventListener('dblclick', function() {
           panzoomInstance.reset();
          });
        }
      }

      $(document).on('shiny:connected', function() {
          setTimeout(initPanzoom, 500); 
      });
      
      Shiny.addCustomMessageHandler('reinitPanzoom', function(message) {
         setTimeout(initPanzoom, 500);
      });
    "))
  )
)

server <- function(input, output, session) {
  
  # Reactive Data
  selectedLakeData <- reactive({
    req(input$selectedLake)
    myData %>% filter(name == input$selectedLake)
  })
  
  # --- Top Card Outputs ---
  
  output$topLakeStats <- renderUI({
    req(input$selectedLake)
    stats <- elevationsTable %>% filter(name == input$selectedLake)
    
    div(class = "stats-container",
        div(class = "stat-item", strong("Elevation: "), stats$`Current Elevation`),
        div(class = "stat-item", strong("Pool: "), stats$`Conservation Pool`),
        div(class = "stat-item", strong("Last Updated: "), format(stats$`Last Updated`, "%Y-%m-%d")),
        div(class = "stat-item", em(stats$Source))
    )
  })
  
  # --- Map Outputs ---
  
  output$lakePic <- renderImage({
    req(input$selectedLake)
    picName <- selectedLakeData() %>% pull(pic) %>% unique()
    
    picPath <- file.path("pics", picName)
    
    if (!length(picName) || is.na(picName) || !file.exists(picPath)) {
      picPath <- file.path("www", "placeholder.png")
    }
    
    list(src = picPath,
         contentType = "image/png",
         width = "100%",
         height = "100%", 
         alt = paste("Picture of", input$selectedLake))
  }, deleteFile = FALSE)
  
  observeEvent(input$selectedLake, {
    session$sendCustomMessage("reinitPanzoom", list())
  })
  
  # --- Ramp Plots Outputs ---
  output$rampPlotsList <- renderUI({
    req(input$selectedLake)
    rampNames <- unique(selectedLakeData()$r.name)
    
    if (length(rampNames) == 0) {
      return(p("No ramps found for this lake."))
    }
    
    plot_output_list <- lapply(rampNames, function(rn) {
      clean_rn <- gsub("[^a-zA-Z0-9]", "_", rn)
      plotname <- paste0("plot_", clean_rn)
      
      div(
        class = "ramp-plot-card", 
        plotOutput(plotname, height = "400px")
      )
    })
    
    do.call(tagList, plot_output_list)
  })
  
  # Generate Plots - With Data Validation
  observe({
    req(input$selectedLake)
    rampNames <- unique(selectedLakeData()$r.name)
    
    for (rn in rampNames) {
      local({
        my_rn <- rn
        clean_rn <- gsub("[^a-zA-Z0-9]", "_", my_rn)
        plotname <- paste0("plot_", clean_rn)
        
        output[[plotname]] <- renderPlot({
          # Retrieve data for this ramp
          filteredData <- selectedLakeData() %>% filter(r.name == my_rn)
          
          # VALIDATION: Check if valid data exists before plotting
          # This prevents "no non-missing arguments to min" errors
          if (nrow(filteredData) == 0) return(NULL)
          if (all(is.na(filteredData$r.bottom)) || all(is.na(filteredData$r.top))) {
            # Return an empty plot with a message or NULL
             plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
             text(x = 0.5, y = 0.5, paste("Insufficient data for ramp:", my_rn), 
                  cex = 1.6, col = "red")
             return()
          }
          
          plotRamp(filteredData)
        })
      })
    }
  })
  
  # --- Data Table Modal Logic ---
  
  observeEvent(input$showData, {
    showModal(modalDialog(
      title = "Lake Elevations Data",
      size = "l", 
      downloadButton("downloadCsv", "Download CSV"),
      br(), br(),
      div(style = "overflow-x: auto;", tableOutput("elevTable")),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  output$elevTable <- renderTable({
    elevationsTable %>%
      select(`Name`=name, `Conservation Pool`, `Current Elevation`, `Last Updated`, Source) %>%
      mutate_if(is.Date, ~format(., "%m-%d-%Y"))
  })
  
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste("lake_elevations_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(elevationsTable, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
