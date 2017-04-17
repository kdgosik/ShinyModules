require(DT)

# MODULE UI
DataViewUI <- function(id) {
  ns <- NS(id)
  
  dataTableOutput(ns("table1"), width = "100%")
  
}



# MODULE Server
DataViewServer <- function(input, output, session, data, data_out_name) {
  
  output$table1 <- renderDataTable({
    
    DT::datatable( data()
                 , rownames = FALSE
                 , style = 'bootstrap'
                 , class = paste( c('compact', 'cell-border' , 'hover' , 'stripe') , collapse = " ")
                 , filter = 'top'
                 , extensions = c( 'Buttons' , 'KeyTable' , 'ColReorder' , 'FixedColumns' , 'FixedHeader')
                 , options = list(
                   dom = 'Bfrtip'
                   , autoWidth = TRUE
                   , columnDefs = list( list( width = '200px', targets = 1 ) )
                   , colReorder = TRUE
                   , paging = F
                   , keys = T
                   , scrollX = TRUE
                   , scrollY = TRUE
                   , fixedHeader = TRUE
                   , buttons = list(
                     'colvis'
                     , 'copy'
                     , 'print'
                     , list( extend = 'collection', buttons = list(list(extend='csv', filename = data_out_name)
                                                                   , list(extend='excel', filename = data_out_name)
                                                                   , list(extend='pdf', filename= data_out_name) )
                             , text = 'Download'
                     ) ) ) )
    
  })
  

  
}