library(ggplot2)
library(plotly)

# MODULE UI
scatterUI <- function(id) {
  ns <- NS(id)
  
  fillCol(
    div(
    sliderInput(ns("slider1"), label = "Limit points", min = 5, max = 32, value = 10),
    style="display: inline-block; height:220px;", plotlyOutput(ns("plot1"))
    )
  )
}



# MODULE Server
scatterServer <- function(input, output, session, data, var1, var2, ptshape = 1, col1 = 1) {
  
  resultdata <- reactive({
    #inpt_slider <- input$slider1
    
    data()[1 : input$slider1, ]
    
  })
  
  output$plot1 <- renderPlotly({
    
    p1 <- ggplot(resultdata(), aes_string(var1, var2)) + 
      geom_point(color = col1, shape = ptshape, size = 3) +
      ggtitle(paste("Using the", paste(quote(data())), "data.frame"))
    
    ggplotly(p1)
    
  })
  
}