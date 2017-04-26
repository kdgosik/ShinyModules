library(ggplot2)
library(plotly)

# MODULE UI
PlotlyHeatmapScatterUI <- function(id) {
  ns <- NS(id)
  
  fillCol(
    div(
      plotlyOutput(ns("heat")),
      verbatimTextOutput(ns("selection")),
      plotlyOutput(ns("scatterplot"))
      )
  )
}



# MODULE Server
PlotlyHeatmapScatterServer <- function(input, output, session, data) {
  
  resultdata <- reactive({
    
    correlation <- round(cor(data()), 3)
    # cor_order <- hclust(dist(data(), method = "binary"))$order
    # correlation <- correlation[cor_order, cor_order]
    nms <- names(data())
    
    list(names = nms,
         data = correlation)
    
  })
  
  
  output$heat <- renderPlotly({
    
    plot_ly(x = resultdata()[["names"]], 
            y = resultdata()[["names"]], 
            z = resultdata()[["data"]], 
            key = resultdata()[["data"]], type = "heatmap") %>%
      layout(xaxis = list(title = ""), 
             yaxis = list(title = ""))
    
  })
  
  output$selection <- renderPrint({
    
    s <- event_data("plotly_click")
    if (length(s) == 0) {
      "Click on a cell in the heatmap to display a scatterplot"
    } else {
      cat("You selected: \n\n")
      as.list(s)
    }
    
  })
  
  output$scatterplot <- renderPlotly({
    
    s <- event_data("plotly_click")
    if (length(s)) {
      vars <- c(s[["x"]], s[["y"]])
      d <- setNames(mtcars[vars], c("x", "y"))
      yhat <- fitted(lm(y ~ x, data = d))
      plot_ly(d, x = ~x) %>%
        add_markers(y = ~y) %>%
        add_lines(y = ~yhat) %>%
        layout(xaxis = list(title = s[["x"]]), 
               yaxis = list(title = s[["y"]]), 
               showlegend = FALSE)
    } else {
      plotly_empty()
    }
    
  })
  
}