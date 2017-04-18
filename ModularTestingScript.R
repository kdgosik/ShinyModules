library(shiny)
rm(list=ls()); gc(reset = TRUE)
source("ModularTimelineVisualizer.R")
source("ModularCSVFileInput.R")
source("ModularScatterPlot.R")

ui <- fluidPage(

  sidebarLayout(
    
    sidebarPanel(
      csvFileInputUI("input_data")
    ),
    
    mainPanel(
      TimelineUI("timelineplot"),
      scatterUI("scatter_id")
    )
    
  )
  
)


server <- function(input, output, session) {

  Data <- callModule(csvFileInputServer, "input_data", stringsAsFactors = FALSE)
  
  callModule(TimelineServer, "timelineplot", data = Data)
  callModule(scatterServer, "scatter_id", data = reactive({mtcars}), var1 = "mpg", var2 = "disp", ptshape = 1, col1 = 1)

}

shinyApp(ui, server)