# Base Image: rocker/r-ver (Versioned R, Ubuntu-based)
# This is lighter than shiny-verse and allows perfect reproducibility
FROM rocker/r-ver:4.3.2

# Install System Dependencies
# libcurl4-openssl-dev, libssl-dev, libxml2-dev: Required for R packages
# nginx: Reverse Proxy
# supervisor: Process Manager
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    nginx \
    supervisor \
    && rm -rf /var/lib/apt/lists/*

# Install R Packages from POSIT PUBLIC MANAGER (Binary Installation)
# This prevents compilation from source, saving huge amounts of space and time.
# Repository: Ubuntu Jammy (22.04) binaries
RUN R -e "install.packages(c('shiny', 'plumber', 'shinydashboard', 'dplyr', 'tidyr', 'stringr', 'lubridate', 'ggplot2', 'readxl', 'rvest', 'base64enc', 'jsonlite'), repos='https://packagemanager.posit.co/cran/__linux__/jammy/latest')"

# Setup Application Directory
WORKDIR /app

# Copy all project files into /app
# (Note: .dockerignore handles excluding large files like zips/git)
COPY . /app

# Ensure directories exist for supervisor logs
RUN mkdir -p /var/log/supervisor

# Expose the ShinyProxy port
EXPOSE 3838

# Start Supervisor (which starts Nginx, Shiny, and Plumber)
CMD ["/usr/bin/supervisord", "-c", "/app/supervisord.conf"]
