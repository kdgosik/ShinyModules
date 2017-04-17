## require()  for required libraries for module

# MODULE UI
**YourModularName**UI <- function(id) {
  ns <- NS(id)
  
  ## Ui Outputs Here from server below
  
  ## for mutliple Output use fillCol/fillRow(), or flowLayout() wrapped around Outputes
  
}



# MODULE Server
**YourModularName**Server <- function(input, output, session, data, ...) {
  
  ## Place server code here to be called by callModule
    ## place whatever inputs needed in function call
  
  ## use reactive and observe elements
    ## render elements
  

  
}