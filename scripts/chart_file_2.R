# This function will make a visualization out of any dataset.

# Requires the library dplyr and plotly.
library(dplyr)
library(plotly)

# Read in the "intro survey data" dataset
data <- read.csv("data/intro_survey_data.csv")

# Uses the Summary_Function script to help summarize the datatable.
source("scripts/Summary_Function.r")


build_chart <- function(data_set) {
  
  #creates the labels for the x axis
  animals <- c("Cat Person", "Dog Person", "Both!", "Neither")
  
  # gets the desired data to manipulate
    summary <- data_set %>% group_by(Do.you.consider.yourself.) %>% 
    summarise(
      mac = sum(What.operating.system.do.you.typically.use. == "Mac"), 
      windows = sum(What.operating.system.do.you.typically.use. == "Windows"), 
      other = sum(What.operating.system.do.you.typically.use. == "Linux") +  sum(What.operating.system.do.you.typically.use. == "Other: Windows at home, Linux (Ubuntu) at work"))   

  # Create new plotly graph.
  new_graphic <- plot_ly(
    x = animals,
    y = summary$mac,
    name = "Mac",
    type = "bar",
    marker = list(color = toRGB("skyblue2"))
  ) %>%
    add_trace(
      x = animals,
      y = summary$windows,
      name = "Windows",
      type = "bar",
      marker = list(color = toRGB("darkorange3"))
    ) %>%
    add_trace(
      x = animals,
      y = summary$other,
      name = "Other",
      type = "bar",
      marker = list(color = toRGB("green"))
    ) %>%
    layout(
      barmode = "stack", 
      xaxis = list(title = "Your Favorite Animal?"), 
      yaxis = list(title = "OS"), 
      legend = list(traceorder = "grouped")
    )
  return(new_graphic)
  
}
