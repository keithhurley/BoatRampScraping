library(plumber)

# Run the API on port 8000 with Swagger docs enabled
r <- plumb("api2.R")
r$run(port = 8000, docs = TRUE)
