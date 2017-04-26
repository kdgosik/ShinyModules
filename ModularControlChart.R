library(ggplot2)
library(plotly)
library(qcc)
library(dplyr)
library(lubridate)


## Helper Function #####

qcc_convert_df <- function( data, 
                            data_col, 
                            type, 
                            size_col = NULL, 
                            group_col = NULL, 
                            time_col = NULL ) {
  
  # if( type == "xbar" ) {
  #   
  #   data_grp <- qcc.groups(data[, data_col], data[, group_col])
  #   qcc_output <- qcc(data_grp, type = "xbar", plot = FALSE)
  #   
  # } else {
    
    qcc_output <- qcc(data = data[, data_col], type = type, sizes = data[, size_col], plot = FALSE)
    
  # }
  
  date <- data[, time_col]
    
  
  out <- data.frame(#Date = date,
                    Points = qcc_output$statistics,
                    Center = qcc_output$center,
                    LCL = qcc_output$limits[, "LCL"],
                    UCL = qcc_output$limits[, "UCL"],
                    Violations = "black",
                    #call = deparse(qcc_output$call, nlines = 1),
                    #type = qcc_output$type,
                    #data_col = data_col,
                    #size_col = size_col,
                    #group_col = ifelse(is.null(group_col), NA, group_col),
                    #time_col = ifelse(is.null(time_col), NA, time_col),
                    #sd = qcc_output$std.dev,
                    #nsigmas = qcc_output$nsigmas,
                    stringsAsFactors = FALSE)
  
  out$Violations[qcc_output$violations$beyond.limits] <- "darkred"
  out$Violations[qcc_output$violations$violating.runs] <- "goldenrod" 
  
  out <- list(Date = data,
              plot_output = data.frame(Date = date, out)
  )
  
  out
  
}

## Practice Data ###############

# mort <- read.csv("Data/11354_Quality_and_Safety_Dashboard_Mortality_OE.csv", stringsAsFactors = F)
# 
# serv_cross_walk_idx <- which(mort$KPI.Name == "Division Description")
# month_idx <- which(mort$KPI.Name == "Calendar Year-Month Formatted")
# 
# cfg.service.unit <- mort[(serv_cross_walk_idx + 1) : (month_idx - 1), ]
# colnames(cfg.service.unit) <- mort[serv_cross_walk_idx, ]
# cfg.service.unit <- cfg.service.unit[, 1 : 3]
# 
# cfg.month <- mort[(month_idx + 1) : nrow(mort), ]
# colnames(cfg.month) <- mort[month_idx, ]
# cfg.month <- cfg.month[,1, drop = F]
# 
# mort_summary <- mort[1 : (serv_cross_walk_idx - 1), ] %>%
#   mutate(Date = ymd_hms(KPI.Date, tz = "EST"), 
#          Date = floor_date(Date, unit = "month"), 
#          KPI.Numerator.SUM = as.numeric(KPI.Numerator.SUM)) %>%
#   group_by(Date) %>%
#   summarise(Numerator = sum(KPI.Numerator.SUM), Denominator = sum(KPI.Denominator.SUM))
# 
# 
# 
# 
# 
#   
# qcc_out <- qcc_convert_df(data = as.data.frame(mort_summary),
#                  data_col = "Numerator",
#                  type = "p",
#                  size_col = "Denominator",
#                  time_col = "Date")
# 
# 
# p <- ggplot(qcc_out$plot_output, aes( x = Date, y = Points)) + 
#   geom_point(aes(fill = Violations)) +
#   geom_line(aes(y = LCL), color = "red") + 
#   geom_line(aes(y = UCL), color = "red") + 
#   geom_line(aes(y = Center), color = "darkgreen")
# 
# ggplotly(p)





## Begin Modular ################


# MODULE UI
ControlChartUI <- function(id) {
  ns <- NS(id)
  
  list(
    div(checkboxInput(ns("changepoint"), "Find Change Point", value = FALSE)),
    div(plotlyOutput(ns("plot1")))
  )
}



# MODULE Server
ControlChartServer <- function(input, output, session, data, var1, var2) {
  
  resultdata <- reactive({
    
    inpt_changepoint <- input$changepoint
    
    # dat <- dat %>%
    #   filter(Date >= inpt_range[1] & Date <= inpt_range[2])
    
    
    qcc_out <- qcc_convert_df(data = as.data.frame(data()),
                              data_col = var1,
                              type = "p",
                              size_col = var2,
                              time_col = "Date")
    
    qcc_out$plot_output
    
  })
  
  output$plot1 <- renderPlotly({
    
    p1 <- ggplot(resultdata(), aes( x = Date, y = Points)) + 
      geom_point(aes(fill = Violations)) +
      geom_line(aes(y = LCL), color = "red") + 
      geom_line(aes(y = UCL), color = "red") + 
      geom_line(aes(y = Center), color = "darkgreen")
    
    # if( inpt_changepoint ){
    #   p1 <- p1 + geom_vline()
    # }
    
    ggplotly(p1)
    
  })
  
}