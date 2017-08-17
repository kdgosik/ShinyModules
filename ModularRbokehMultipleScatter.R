## require()  for required libraries for module
require(rbokeh)

# MODULE UI
RbokehMultipleScatterUI <- function(id) {
  ns <- NS(id)
  
  ## Ui Outputs Here from server below
  rbokeh::rbokehOutput(ns("plot1"))
  
  ## for mutliple Output use fillCol/fillRow(), or flowLayout() wrapped around Outputes
  
}



# MODULE Server
RbokehMultipleScatterServer <- function(input, output, session, data, ...) {
  
  ## Place server code here to be called by callModule
    ## place whatever inputs needed in function call
  
  output$plot1 <- rbokeh::renderRbokeh({
    tools <- c("pan", "wheel_zoom", "box_zoom", "box_select", "reset")
    nms <- names(grep("numeric", sapply(data(), class), value = TRUE))
    grp <- names(grep("factor|character", sapply(data(), class), value = TRUE))[1]
    p <- length(nms)
    
    splom_list <- lapply(nms[2 : p], function(nam) {
      figure(width = 200, height = 200, tools = tools,
             xlab = "Petal.Width", ylab = nam) %>%
        ly_points(nms[1], nam, data = data(),
                  color = "black", size = 5, legend = FALSE, 
                  hover = lapply(names(data()), paste))
    })
    
    grid_plot(splom_list, ncol = 4, same_axes = FALSE, link_data = TRUE)
    
  })
  
  ## use reactive and observe elements
    ## render elements
}