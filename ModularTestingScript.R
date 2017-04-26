library(shiny)
rm(list=ls()); gc(reset = TRUE)
source("ModularControlChart.R")


ui <- fluidPage(
  
  ControlChartUI("spc_id")
  
)


server <- function(input, output, session) {

  Data <- reactive({mtcars})
  
  callModule(ControlChartServer, "spc_id", data = Data)

}

shinyApp(ui, server)