library(timevis)
library(dplyr)
library(lubridate)


## Begin Modular ################


# MODULE UI
TimelineUI <- function(id) {
  ns <- NS(id)
  
  fillCol(
    div(
      actionButton(ns("show_timeline"), "Show Timeline"),
      selectizeInput(ns("content"), "Select Content Column", choices = NULL),
      selectizeInput(ns("start"), "Select Content Column", choices = NULL),
      selectizeInput(ns("end"), "Select Content Column", multiple = TRUE, choices = NULL),
      selectizeInput(ns("group"), "Select Content Column", multiple = TRUE, choices = NULL),
      timevisOutput(ns("timeline"))
      )
    )
  
}



# MODULE Server
TimelineServer <- function(input, output, session, data) {
  
  ns <- session$ns
  
  observe({
    
    updateSelectizeInput(session, "content", "Select Content Column", choices = colnames(data()))
    updateSelectizeInput(session, "start", "Select Start Date Column", choices = colnames(data()))
    updateSelectizeInput(session, "end", "Select End Date Column", choices = colnames(data()))
    updateSelectizeInput(session, "group", "Select Group Column", choices = colnames(data()))
  
  })

  
  resultsdata <- eventReactive(input$show_timeline, {

    id <- 1:NROW(data())
    content <- data()[[input$content]]
    start <- data()[[input$start]]
    end <- tryCatch({data()[[input$end]]}, error = function(e) NULL)
    group <- tryCatch({data()[[input$group]]}, error = function(e) NULL)
    if( is.null(end) ) end <- NA
    if( is.null(group) ) group <- NA

    vis_data <- data.frame(id = id,
                           content = content,
                           start = start, 
                           end = end,
                           group = group)
    group_data <- data.frame(id = unique(group), 
                             content = unique(group))
    
    list(data = vis_data, 
         group = group_data)

  })
  
  
  output$timeline <- renderTimevis({ 
    
    if( is.null(input$group) ){

      timevis(data = resultsdata()[["data"]],
              options = list(editable = TRUE,
                             multiselect = TRUE
              )
      )

    }else{

      timevis(data = resultsdata()[["data"]],
              groups = resultsdata()[["group"]],
              options = list(editable = TRUE,
                             multiselect = TRUE
              )
      )


    }

    })

}
