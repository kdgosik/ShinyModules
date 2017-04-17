library(timevis)
library(dplyr)
library(lubridate)


## Begin Modular ################


# MODULE UI
TimelineUI <- function(id) {
  ns <- NS(id)
  
  list(
    div(actionButton(ns("show_timeline"), "Show Timeline")),
    div(selectizeInput(ns("content"), "Select Content Column", choices = NULL)),
    div(selectizeInput(ns("start"), "Select Content Column", choices = NULL)),
    div(selectizeInput(ns("end"), "Select Content Column", multiple = TRUE, choices = NULL)),
    div(selectizeInput(ns("group"), "Select Content Column", multiple = TRUE, choices = NULL)),
    div(timevisOutput(ns("timeline"))),
    div(dataTableOutput(ns("table1")))
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
    end <- ifelse(is.null(input$end), NA, data()[[input$end]])
    group <- ifelse(is.null(input$group), NA,  data()[[input$group]])

    vis_data <- data.frame(id = id,
                           content = content,
                           start = start, 
                           end = end)
    group_data <- data.frame(id = unique(group), 
                             content = unique(group))
    
    list(data = vis_data, 
         group = group_data)

  })
  
  
  output$timeline <- renderTimevis({
    
    # timevis(data = resultsdata()[["data"]],
    #         group = resultsdata()[["group"]])
    

    
    if( is.null(input$group) ){

      timevis(data = resultsdata()[["data"]],
              options = list(editable = TRUE,
                             multiselect = TRUE
              )
      )

    }else{

      timevis(data = resultsdata()[["data"]],
              group = resultsdata()[["group"]],
              options = list(editable = TRUE,
                             multiselect = TRUE
              )
      )


    }

    })
  
  output$table1 <- renderDataTable({ resultsdata() })
  
}