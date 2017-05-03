## require()  for required libraries for module
require(ggplot2)
require(plotly)

# MODULE UI
OneVarPlotUI <- function(id) {
  ns <- NS(id)
  
  ## Ui Outputs Here from server below
  
  ## for mutliple Output use fillCol/fillRow(), or flowLayout() wrapped around Outputes
  fillCol(
    div(
    selectInput(ns("plot_type"), "Select Plot Type", choices = c("Histogram", "Density", "Dot Plot", "Bar")),
    checkboxInput(ns("group_check"), "Include Comparison Group"),
    
    uiOutput(ns("group_var")), 
    
    selectInput(ns("xvar"), "Select Variable", choices = NULL),
    actionButton(ns("update_chart"), "Update"),
    plotlyOutput(ns("plot_out"))
  ))
  
}



# MODULE Server
OneVarPlotServer <- function(input, output, session, data) {
  
  ## Place server code here to be called by callModule
    ## place whatever inputs needed in function call
  ns <- session$ns
  
  observe({
    updateSelectInput(session, "xvar", "Select Variable", choices = colnames(data()))
  })
  
  output$group_var <- renderUI({
    
    if(input$group_check){
      selectInput("group_var", "Select Comparison", choices = colnames(data()))
    }
    
  })
  
  plot_data <- eventReactive(input$update_chart, {
    
    p <- ggplot(data(), aes_string(x = input$xvar))
    
      if(input$group_check){
        p <- ggplot(data(), aes_string(x = input$xvar, group = input$group_var, fill = input$group_var))
      }
    
    p <- switch(input$plot_type, 
                `Histogram` = p + geom_histogram(),
                `Density` = p + geom_density(),
                `Dot Plot` = p + geom_dotplot(),
                `Bar` = p + geom_bar())
    
  })
  ## use reactive and observe elements
    ## render elements
  output$plot_out <- renderPlotly({
    
    ggplotly(plot_data())
    
  })

}