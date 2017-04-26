library(ggplot2)
library(plotly)

# MODULE UI
TrendDisplayUI <- function(id) {
  ns <- NS(id)
  
  fillCol(
    div(
      selectInput(ns("xvar"), "Select X Variable", choices = NULL),
      selectInput(ns("yvar"), "Select Y Variable", choices = NULL),
      plotlyOutput(ns("plot1"))
      )
  )
}



# MODULE Server
TrendDisplayServer <- function(input, output, session, data) {
  
  ns <- session$ns
  
  observe({
    updateSelectInput(session, "xvar", "Select X Variable", choices = colnames(data()))
    updateSelectInput(session, "yvar", "Select Y Variable", choices = colnames(data()))
  })
  
  x_var <- reactive({input$xvar})
  y_var <- reactive({input$yvar})
  
    # plotly
  output$plot1 <- renderPlotly({
    
    p <- ggplot(data(), aes_string(x = x_var(), y = y_var())) + 
      geom_point() + 
      geom_line()
    
    ggplotly(p)
    
  })
  
  
}