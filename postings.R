# Load required libraries
library(dplyr)
library(plotly)
library(data.table)
library(DT)
library(htmlwidgets)

# Function to load postings data
load_postings_data <- function(year = 2023) {
  # Determine file path based on year
  if (year == 2021) {
    file_path <- "data/2021_postings.rds"
  } else if (year == 2023) {
    file_path <- "data/2023_postings.rds"
  } else {
    stop("Only years 2021 and 2023 are supported")
  }
  
  # Load the data
  postings_data <- readRDS(file_path)
  
  # Return the data with year attribute
  attr(postings_data, "year") <- year
  return(postings_data)
}

# Function to create a bar chart of job postings by seniority
plot_seniority_distribution <- function(postings_data, title = NULL) {
  # If title is not provided, use the year from data attributes
  if (is.null(title)) {
    year <- attr(postings_data, "year")
    title <- paste("Job Postings by Seniority -", year)
  }
  
  # Prepare data for visualization
  seniority_counts <- postings_data %>%
    mutate(Seniority = ifelse(is.na(Seniority) | Seniority == "None Specified", "Not Specified", Seniority)) %>%
    count(Seniority) %>%
    arrange(desc(n))
  
  # Create the interactive bar chart
  plot_ly(seniority_counts, x = ~Seniority, y = ~n, type = "bar",
          marker = list(color = "rgba(50, 171, 96, 0.7)"),
          hoverinfo = "text",
          text = ~paste("Seniority: ", Seniority, "<br>Count: ", n, "<br>Percentage: ", 
                        round(n/sum(n)*100, 1), "%"),
          textposition = "none",
          showlegend = FALSE) %>%
    layout(title = title,
           xaxis = list(title = "Seniority Level"),
           yaxis = list(title = "Number of Job Postings"),
           hovermode = "closest")
}

# Function to create a pie chart of job postings by role
plot_role_distribution <- function(postings_data, title = NULL) {
  # If title is not provided, use the year from data attributes
  if (is.null(title)) {
    year <- attr(postings_data, "year")
    title <- paste("Job Postings by Role -", year)
  }
  
  # Prepare data for visualization - get top 10 roles
  role_counts <- postings_data %>%
    count(Role) %>%
    arrange(desc(n)) %>%
    slice_head(n = 10)
  
  # Calculate percentage
  role_counts$percentage <- round(role_counts$n / sum(role_counts$n) * 100, 1)
  
  # Create the interactive pie chart
  plot_ly(role_counts, labels = ~Role, values = ~n, type = "pie",
          textposition = "inside",
          textinfo = "label+percent",
          insidetextfont = list(color = "#FFFFFF"),
          hoverinfo = "text",
          text = ~paste("Role: ", Role, "<br>Count: ", n, "<br>Percentage: ", percentage, "%"),
          marker = list(colors = colorRamp(c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b"))(seq(0, 1, length.out = nrow(role_counts))))) %>%
    layout(title = title,
           showlegend = TRUE,
           legend = list(orientation = "h", y = -0.1))
}

# Function to create a stacked bar chart comparing seniority across years
plot_seniority_comparison <- function() {
  # Load data for both years
  data_2021 <- load_postings_data(2021)
  data_2023 <- load_postings_data(2023)
  
  # Prepare data for 2021
  seniority_2021 <- data_2021 %>%
    mutate(Seniority = ifelse(is.na(Seniority) | Seniority == "None Specified", "Not Specified", Seniority)) %>%
    count(Seniority) %>%
    mutate(Year = "2021", Percentage = n / sum(n) * 100)
  
  # Prepare data for 2023
  seniority_2023 <- data_2023 %>%
    mutate(Seniority = ifelse(is.na(Seniority) | Seniority == "None Specified", "Not Specified", Seniority)) %>%
    count(Seniority) %>%
    mutate(Year = "2023", Percentage = n / sum(n) * 100)
  
  # Combine data
  combined_data <- rbind(seniority_2021, seniority_2023)
  
  # Create the interactive grouped bar chart
  plot_ly(combined_data, x = ~Seniority, y = ~Percentage, color = ~Year, type = "bar",
          colors = c("#1f77b4", "#ff7f0e"),
          hoverinfo = "text",
          text = ~paste("Year: ", Year, "<br>Seniority: ", Seniority, "<br>Count: ", n, 
                        "<br>Percentage: ", round(Percentage, 1), "%")) %>%
    layout(title = "Seniority Level Comparison: 2021 vs 2023",
           xaxis = list(title = "Seniority Level"),
           yaxis = list(title = "Percentage of Job Postings"),
           barmode = "group",
           hovermode = "closest")
}
