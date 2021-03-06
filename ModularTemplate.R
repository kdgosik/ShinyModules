## require()  for required libraries for module
require(rbokeh)

# MODULE UI
ModularRbokehMultipleScatterUI <- function(id) {
  ns <- NS(id)
  
  ## Ui Outputs Here from server below
  robokeh::rbokehOutput(ns("p1"))
  
  ## for mutliple Output use fillCol/fillRow(), or flowLayout() wrapped around Outputes
  
}



# MODULE Server
ModularRbokehMultipleScatterServer <- function(input, output, session, data, ...) {
  
  ## Place server code here to be called by callModule
    ## place whatever inputs needed in function call
  
  output$p1 <- rbokeh::renderRbokeh({
    tools <- c("pan", "wheel_zoom", "box_zoom", "box_select", "reset")
    nms <- colnames(data())
    p <- length(nms)
    
    splom_list <- lapply(nms[2 : p], function(nam) {
      figure(width = 200, height = 200, tools = tools,
             xlab = "Petal.Width", ylab = nam) %>%
        ly_points(nms[1], nam, data = data(),
                  color = Species, size = 5, legend = FALSE, 
                  hover = list(names(data())))
    })
    
    grid_plot(splom_list, ncol = 4, same_axes = FALSE, link_data = TRUE)
    
  })
  
  ## use reactive and observe elements
    ## render elements
}