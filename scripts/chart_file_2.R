library(plotly)
library(dplyr)

build_chart <- function(data_set) {
  PC  <- data_set %>% filter(What.operating.system.do.you.typically.use. == "PC")
  Mac <- data_set %>% filter(What.operating.system.do.you.typically.use. == "Mac")
  
  new_chart <- plot_ly(y = freshman$How.many.countries.have.you.visited.in.your.life., 
                       type = "box", 
                       name = "PC") %>%
    add_trace(y = sophomore$How.many.countries.have.you.visited.in.your.life., 
              type = "box", 
              name = "Mac") %>%
    layout(yaxis = list(title = "Countries Visited")
    )
  
  return(new_chart)
}
