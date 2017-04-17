library(shiny)
rm(list=ls()); gc(reset = TRUE)
source("ModularTimelineVisualizer.R")

ui <- fluidPage(

  TimelineUI("timelineplot")
  
)


server <- function(input, output, session) {

  Data <- reactive({
    data.frame(id = 1:10,
               content = paste0("item", 1:10),
               start = seq(as.Date("2016-01-01"), as.Date("2016-10-01"), "month"),
               end = seq(as.Date("2016-02-01"), as.Date("2016-11-01"), "month"),
               group = c(rep("group1", 4), rep("group2", 6)),
               stringsAsFactors = FALSE
    )
  })
  
  callModule(TimelineServer, "timelineplot", data = Data)

}

shinyApp(ui, server)