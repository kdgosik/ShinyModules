library(ggplot2)
library(plotly)

# MODULE UI
TrendDisplayUI <- function(id) {
  ns <- NS(id)
  
  div(plotlyOutput(ns("plot1")))
}



# MODULE Server
TrendDisplayServer <- function(input, output, session, data, x_var, y_var) {
  
  # plotly
  output$plot1 <- renderPlotly({
    
    p <- ggplot(data(), aes_string(x = x_var, y = y_var)) + 
      geom_point() + 
      geom_line()
    
    ggplotly(p)
    
  })
  
  
}