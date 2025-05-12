# Plumber API for Boat Ramp Information
library(plumber)
library(tidyverse)
library(jsonlite)
source("getData.R")

#* @apiTitle Boat Ramp Information API
#* @apiDescription API for accessing boat ramp elevation data and visualizations

#* Get lake elevation data by wbCode
#* @param wbCode Water body code to look up
#* @get /elevation/<wbCode:char>
#* @response 200 Returns lake elevation data
#* @response 404 Lake not found
function(wbCode) {
  # Filter data for the requested wbCode
  lake_data <- myData %>%
    filter(wbCode == wbCode) %>%
    select(name, elevation, updatedDate, r.cp, r.name, r.bottom, r.top)
  
  if(nrow(lake_data) > 0) {
    return(lake_data)
  } else {
    # Return 404 error
    res <- list(error = "Lake not found", status = 404)
    attr(res, "status") <- 404
    return(res)
  }
}

#* Get ramp plot image by wbCode
#* @param wbCode Water body code to look up
#* @get /plot/<wbCode:char>
#* @serializer png
function(wbCode, res) {
  # Filter data for the requested wbCode
  lake_data <- myData %>%
    filter(wbCode == wbCode)
  
  if(nrow(lake_data) > 0) {
    # Create a temporary file for the plot
    temp_file <- tempfile(fileext = ".png")
    
    # Save the plot to the temp file
    png(temp_file, width = 800, height = 600)
    tryCatch({
      plotRamp(lake_data)
    }, error = function(e) {
      dev.off()
      unlink(temp_file)
      res$status <- 500
      return(NULL)
    })
    dev.off()
    
    # Read and return the binary content
    bin <- readBin(temp_file, "raw", file.info(temp_file)$size)
    unlink(temp_file)
    return(bin)
  } else {
    res$status <- 404
    return(NULL)
  }
}

#* List all available lakes
#* @get /lakes
function() {
  lake_list <- myData %>%
    distinct(wbCode, name) %>%
    arrange(name)
  
  return(lake_list)
}

# Start the Plumber API
pr() %>%
  pr_set_docs(use_path = TRUE) %>%
  pr_run(host = "0.0.0.0", port = 8000)