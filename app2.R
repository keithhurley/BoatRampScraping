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
  dashboardHeader(title = "Boat Ramp Usability"),
  dashboardSidebar(
    collapsed = TRUE,
    width = 500,
    # Constrain the table output within a div
    div(style = "overflow-x: auto;", tableOutput("elevTable"))
  ),
  dashboardBody(
    # Include the Panzoom JS library and custom JS code
    tags$head(
      tags$script(src = "https://unpkg.com/@panzoom/panzoom/dist/panzoom.min.js")
    ),
    # Custom CSS to constrain the image container and control image display
    tags$style(HTML("
      #panzoomContainer {
        width: 100%;
        height: 300px;
        overflow: hidden;
        position: relative;
      }
      #panzoomContainer img {
        width: 100%;
        height: 100%;
        object-fit: contain; /* Scale the image to show the entire content */
        display: block;
        margin: 0 auto;
      }
      
      .selectize-input {
        padding-right:40px;
      }
      
      /* Increase font sizes for UI elements */
      body, .content-wrapper, .content, .box, .box-header, .box-title, 
      .control-label, .selectize-input, .shiny-input-container {
        font-size: 14px;
      }
      
      .carousel-indicators li.active {
      background-color:red;
      }
    ")),
    
    # Top row: lake selector aligned to the right
    fluidRow(
      column(width = 12,
             div(style = "display: flex; align-items: center; justify-content: flex-end; padding: 10px;",
                 tags$h3("Choose a lake:", style = "margin: 0 5px 0 0;"),
                 selectInput("selectedLake", label = NULL, choices = sort(unique(myData$name)))
             )
      )
    ),
    # Main content row: left = lake picture; right = ramp plots carousel
    fluidRow(
      column(width = 6,
             box(
               title = uiOutput("lakeNameHeader"),
               status = "primary",
               solidHeader = TRUE,
               width = NULL,
               div(id = "panzoomContainer",
                   imageOutput("lakePic", height = "300px")
               )
             )
      ),
      column(width = 6,
             box(
               title = "Ramp Plots",
               status = "primary",
               solidHeader = TRUE,
               width = NULL,
               # Render a UI output that holds a carousel.
               uiOutput("rampCarousel")
             )
      )
    ),
    # Custom JavaScript to initialize and reinitialize Panzoom for the lake picture
    tags$script(HTML("
      function initPanzoom() {
        const elem = document.getElementById('panzoomContainer');
        if (elem) {
          const panzoomInstance = Panzoom(elem, {
            maxScale: 10,
            contain: 'outside',
            startTransform: 'scale(1) translate(0px, 0px)'
          });
          elem.addEventListener('wheel', panzoomInstance.zoomWithWheel);
        }
      }
      
      document.addEventListener('DOMContentLoaded', function() {
        initPanzoom();
      });
      
      Shiny.addCustomMessageHandler('reinitPanzoom', function(message) {
        initPanzoom();
      });
    "))
  )
)

server <- function(input, output, session) {
  
  # Render the elevation table in the sidebar
  output$elevTable <- renderTable({
    elevationsTable %>%
      select(`Name`=name, `Conservation Pool`, `Current Elevation`, `Last Updated`, Source) %>%
      mutate_if(is.Date, ~format(., "%m-%d-%Y"))
  })
  
  # Dynamic header for the picture box showing the currently selected lake
  output$lakeNameHeader <- renderUI({
    req(input$selectedLake)
    paste(input$selectedLake)
  })
  
  # Render the lake picture for the selected lake
  output$lakePic <- renderImage({
    req(input$selectedLake)
    # Get the picture filename for the selected lake (assumes one picture per lake)
    picName <- myData %>%
      filter(name == input$selectedLake) %>%
      pull(pic) %>%
      unique()
    picPath <- file.path("pics", picName)
    # Use a placeholder if the file doesn't exist
    if (!file.exists(picPath)) {
      picPath <- file.path("www", "placeholder.png")
    }
    list(src = picPath,
         contentType = "image/png",
         alt = paste("Picture of", input$selectedLake))
  }, deleteFile = FALSE)
  
  # Reactive expression to generate individual ramp plots as base64 images
  rampPlots <- reactive({
    req(input$selectedLake)
    # Get unique ramp names for the selected lake
    rampNames <- unique(myData$r.name[myData$name == input$selectedLake])

    lapply(rampNames, function(rn) {
      filteredData <- myData %>%
        filter(name == input$selectedLake, r.name == rn)
      tmp <- tempfile(fileext = ".png")
      png(tmp, width = 600, height = 400)
      print(plotRamp(filteredData))
      dev.off()
      # Encode the image in base64 for embedding
      encoded <- dataURI(file = tmp, mime = "image/png")
      list(src = encoded, alt = rn)
    })
  })
  
  # Render the ramp plots carousel UI using Bootstrap 3 markup.
  output$rampCarousel <- renderUI({
    imgs <- rampPlots()
    if (length(imgs) == 0) return(NULL)
    
    # If there's only one ramp plot, just show the image
    if (length(imgs) == 1) {
      return(
        tags$div(
          tags$img(class = "img-responsive",
                   src = imgs[[1]]$src,
                   alt = imgs[[1]]$alt,
                   style = "max-height:400px; object-fit: contain;")
        )
      )
    }
    
    # Otherwise, build the carousel with indicators and controls
    # Create carousel indicators
    indicators <- tags$ol(class = "carousel-indicators",
                          lapply(seq_along(imgs) - 1, function(i) {
                            tags$li(`data-target` = "#carouselExample", `data-slide-to` = i,
                                    class = if(i == 0) "active" else NULL)
                          })
    )
    
    # Create carousel items (each slide)
    items <- tags$div(class = "carousel-inner", role = "listbox",
                      lapply(seq_along(imgs), function(i) {
                        tags$div(class = paste("item", if(i == 1) "active" else ""),
                                 tags$img(class = "img-responsive",
                                          src = imgs[[i]]$src,
                                          alt = imgs[[i]]$alt,
                                          style = "max-height:400px; object-fit: contain;")
                        )
                      })
    )
    
    # Carousel controls (previous/next)
    controls <- tagList(
      tags$a(class = "left carousel-control", href = "#carouselExample", role = "button", `data-slide` = "prev",
             tags$span(class = "glyphicon glyphicon-chevron-left", `aria-hidden` = "true"),
             tags$span(class = "sr-only", "Previous")
      ),
      tags$a(class = "right carousel-control", href = "#carouselExample", role = "button", `data-slide` = "next",
             tags$span(class = "glyphicon glyphicon-chevron-right", `aria-hidden` = "true"),
             tags$span(class = "sr-only", "Next")
      )
    )
    
    # Wrap everything in the main carousel container.
    tags$div(id = "carouselExample", class = "carousel slide", `data-ride` = "carousel",
             indicators,
             items,
             controls
    )
  })
  
  
  # Reinitialize Panzoom each time the selected lake changes.
  observeEvent(input$selectedLake, {
    session$sendCustomMessage("reinitPanzoom", list())
  })
}

shinyApp(ui, server)
