library(shiny)
rm(list=ls()); gc(reset = TRUE)
source("ModularOneVarPlot.R")
source("ModularTeletrackingTimeline.R")
source("ModularDataView.R")

ui <- fluidPage(
  
 TeletrackingTimelineUI("tele"),
 DataViewUI("view")
 
)


server <- function(input, output, session) {

  Data <- callModule(TeletrackingTimelineServer, "tele", date_range = c("2017-01-01", "2017-01-07"))
    # reactive({mtcars})
  
  callModule(DataViewServer, "view", data = Data, data_out_name = "tele")

}

shinyApp(ui, server)