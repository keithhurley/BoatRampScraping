library(plumber)
library(dplyr)
library(tidyr)
library(jsonlite)
library(ggplot2)
library(base64enc)
library(stringr)

# Source your getData.R file which loads 'myData', 'plotRamp', etc.
source("getData.R")  


#* Get elevation data for a lake by wbCode
#* @param myWbCode The unique waterbody code.
#* @get /elevation
function(myWbCode = "") {
  if(myWbCode == ""){
    res <- list(error = "Please supply a valid wbCode")
  } else {
    res <- myData %>%
      filter(myWbCode == wbCode) %>%
      select("Waterbody Name" = name, "Conservation_Pool"=r.cp, "Elevation" = elevation, "Last_Updated" = updatedDate, Source) %>%
      distinct()
    if(nrow(res) == 0) res <- list(error = "No data found for this wbCode")
  }
  res
}

#* Get elevation data for a lake's ramps by wbCode
#* @param myWbCode The unique waterbody code.
#* @get /ramps
function(myWbCode = "") {
  if(myWbCode == ""){
    res <- list(error = "Please supply a valid wbCode")
  } else {
    res <- myData %>%
      filter(myWbCode == wbCode) %>%
      select("Waterbody Name" = name, 
             "Conservation_Pool"=r.cp, 
             "Lake_Elevation" = elevation, 
             "Last_Updated" = updatedDate,
             Source,
             "Ramp_Name"=r.name,
             "Top_Of_Ramp"= r.top,
             "Bottom_Of_Ramp" = r.bottom,
             "Latitude" = r.lat,
             "Longitude" = r.long,
             "WaterbodyCode" = wbCode,
             "OutOfService_Flag" = oos)
    if(nrow(res) == 0) res <- list(error = "No data found for this wbCode")
  }
  res
}

#* Return the ramp plot for the ramp specified by its index in myData.
#* @param index The row number (as a string or numeric) in myData for the ramp.
#* @get /rampPlot
#* @serializer contentType list(type="image/png")
function(index = NA) {
  if(is.na(index) || !nzchar(index)) {
    stop("Please supply a valid index parameter.")
  }
  index <- as.numeric(index)
  if(index < 1 || index > nrow(myData)) {
    stop("Index out of bounds.")
  }
  rampData <- myData[index, ]
  p <- plotRamp(rampData)
  
  # Save the plot to a temporary PNG file.
  tmp <- tempfile(fileext = ".png")
  png(tmp, width = 600, height = 400)
  print(p)
  dev.off()
  
  # Return the binary contents of the PNG.
  readBin(tmp, "raw", n = file.info(tmp)$size)
}

#* List available ramps.
#* Returns a JSON list of available ramps (r.name) along with an index number.
#* @get /rampList
#* @serializer json
function() {
  # Assume that each row in myData that has a non-missing r.name represents an available ramp.
  available <- myData %>%
    filter(!is.na(r.name)) %>%
    mutate(index = row_number()) %>%
    select(index, r.name) %>%
    distinct()
  
  return(available)
}

#* Return ramp plots for all ramps on a waterbody.
#* @param wbCode The waterbody code.
#* @get /waterbodyRampPlots
#* @serializer json
function(wbCode = "") {
  if(wbCode == "") {
    stop("Please supply a waterbody code (wbCode).")
  }
  
  dataFiltered <- myData %>% filter(wbCode == wbCode)
  if(nrow(dataFiltered) == 0) {
    stop("No data found for this waterbody code.")
  }
  
  # Get the unique ramp names for the specified waterbody.
  rampNames <- unique(dataFiltered$r.name)
  
  # For each ramp, generate a plot and encode it in base64.
  rampImages <- lapply(rampNames, function(rn) {
    rampData <- dataFiltered %>% filter(r.name == rn)
    tmp <- tempfile(fileext = ".png")
    png(tmp, width = 600, height = 400)
    print(plotRamp(rampData))
    dev.off()
    
    encoded <- dataURI(file = tmp, mime = "image/png")
    list(ramp = rn, image = encoded)
  })
  
  return(rampImages)
}

