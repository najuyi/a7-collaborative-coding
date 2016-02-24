# This function will make a visualization out of any dataset.

# Requires the library dplyr and plotly.
library(dplyr)
library(plotly)

# Read in the "intro survey data" dataset
data <- read.csv("data/intro_survey_data.csv")

# Uses the Summary_Function script to help summarize the datatable.
source("scripts/Summary_Function.r")

# Creates a visualization that compares how many of the people in each class standing are planning on applying
# to the Informatics major to the number of people in each class standing.
make_visualization <- function(requested_data) {
  summary <- sum_func(requested_data)
  chart <- plot_ly( 
    # Creates the bottom part of the stacked bar graph that talks about the people planning on applying to Informatics.
    data = summary, 
    x = summary$'Class Standing', 
    y = summary$'Number Applying to Info',
    # Reports the total number of people planning to apply.
    text = paste('Applying: ', summary$'Number Applying to Info'), 
    name = 'Applying to Informatics',
    type = "bar"
  ) %>% 
    # Creates the top part of the stacked bar graph that talks about the people who aren't planning on applyign to Informatics.
    add_trace( 
      x = summary$'Class Standing', 
      # Uses the difference to show the actual population of the class.
      y = summary$'Students in Class' - summary$'Number Applying to Info', 
      # Reports the number of people in the class instead of just the difference.
      text = paste('Population: ', summary$'Students in Class'), 
      name = 'People in Class'
    ) %>% 
    layout(
      title = "Students Applying to Informatics",
      barmode = "stack",
      xaxis = list(title = "Class Standing"),
      yaxis = list(title = "Population of Class")
    )
  return(chart)
}