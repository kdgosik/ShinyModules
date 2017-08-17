library(shiny)
rm(list=ls()); gc(reset = TRUE)
source("ModularRbokehMultipleScatter.R")
source("ModularDataView.R")

ui <- fluidPage(
  
 RbokehMultipleScatterUI("scatter"),
 DataViewUI("view")
 
)


server <- function(input, output, session) {

  Data <- reactive({iris})
  
  callModule(DataViewServer, "view", data = Data, data_out_name = "tele")
  callModule(RbokehMultipleScatterServer, "scatter", data = Data)

}

shinyApp(ui, server)