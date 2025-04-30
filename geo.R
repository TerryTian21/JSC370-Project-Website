# Load required libraries
library(dplyr)
library(leaflet)

# Function to load location data
load_location_data <- function(year = 2023) {
  # Determine file path based on year
  if (year == 2021) {
    file_path <- "data/location_2021.rds"
  } else if (year == 2023) {
    file_path <- "data/location_2023.rds"
  } else {
    stop("Only years 2021 and 2023 are supported")
  }
  
  # Load the data
  location_data <- readRDS(file_path)
  
  # Return the data with year attribute
  attr(location_data, "year") <- year
  return(location_data)
}

# Function to create an interactive leaflet map
create_interactive_map <- function(location_data, title = NULL) {
  # If title is not provided, use the year from data attributes
  if (is.null(title)) {
    year <- attr(location_data, "year")
    title <- paste("Interactive State Map -", year)
  }
  
  # Create popup content
  location_data$popup_content <- paste0(
    "<strong>", location_data$state, "</strong><br>",
    "Count: ", location_data$Count
  )
  
  # Create a color palette based on the count values
  pal <- colorNumeric(
    palette = "viridis",
    domain = location_data$Count
  )
  
  # Create the leaflet map
  leaflet(location_data) %>%
    addTiles() %>%
    addCircleMarkers(
      lng = ~lon,
      lat = ~lat,
      radius = ~sqrt(Count)/2,
      color = ~pal(Count),
      fillOpacity = 0.7,
      popup = ~popup_content,
      label = ~state
    ) %>%
    addLegend(
      position = "bottomright",
      title = "Count",
      pal = pal,
      values = ~Count,
      opacity = 0.7
    )
}
