# Base Image with Tidyverse and Shiny/Devtools
FROM rocker/shiny-verse:latest

# Install System Dependencies
# nginx: Reverse Proxy
# supervisor: Process Manager
RUN apt-get update && apt-get install -y \
    nginx \
    supervisor \
    && rm -rf /var/lib/apt/lists/*

# Install R Packages not included in rocker/shiny-verse
# (shiny, dplyr, tidyr, stringr, ggplot2, lubridate, readxl, rvest are likely needed)
RUN R -e "install.packages(c('plumber', 'shinydashboard', 'base64enc', 'readxl', 'rvest', 'lubridate'))"

# Setup Application Directory
WORKDIR /app

# Copy all project files into /app
COPY . /app

# Ensure directories exist for supervisor logs
RUN mkdir -p /var/log/supervisor

# Expose the ShinyProxy port
EXPOSE 3838

# Start Supervisor (which starts Nginx, Shiny, and Plumber)
CMD ["/usr/bin/supervisord", "-c", "/app/supervisord.conf"]
