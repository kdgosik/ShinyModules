## require()  for required libraries for module
library(D3TableFilter)

# MODULE UI
EditDataTableUI <- function(id) {
  ns <- NS(id)
  
  ## Ui Outputs Here from server below
  
  ## for mutliple Output use fillCol/fillRow(), or flowLayout() wrapped around Outputes
  fillCol(
  d3tfOutput('mtcars')
  )
  
}



# MODULE Server
EditDataTableServer <- function(input, output, session) {
  
  ns <- session$ns
  
  ## Place server code here to be called by callModule
    ## place whatever inputs needed in function call
  
  enableEdit(session, "mtcars", c("col_1", "col_2"))
  # For a output object "mtcars" D3TableFilter generates an input "mtcars_edit".
  #
  # This observer does a simple input validation and sends a confirm or reject message
  # after each edit.
  observe({
    if(is.null(input$mtcars_edit)) return(NULL);
    edit <- input$mtcars_edit;
    
    isolate({
      # need isolate, otherwise this observer would run twice
      # for each edit
      id <- edit$id;
      row <- as.integer(edit$row);
      col <- as.integer(edit$col);
      val <- edit$val;
      
      # validate input 
      if(col == 0) {
        # rownames
        oldval <- rownames(revals$mtcars)[row];
        # rownames can not start with a digit
        if(grepl('^\\d', val)) {
          rejectEdit(session, tbl = "mtcars", row = row, col = col,  id = id, value = oldval);
          return(NULL);
        }
      } else if (col %in% c(1, 2, 3)){
        # numeric columns
        if(is.na(suppressWarnings(as.numeric(val)))) {
          oldval <- revals$mtcars[row, col];
          # reset to the old value
          # input will turn red briefly, than fade to previous color while
          # text returns to previous value
          rejectEdit(session, tbl = "mtcars", row = row, col = col, id = id, value = oldval);
          return(NULL);
        } 
      }
      
      # accept edits
      if(col == 0) {
        rownames(revals$mtcars)[row] <- val;
      } else if (col %in% c(1, 2, 3)) {
        revals$mtcars[row, col] <- as.numeric(val);
        val = round(as.numeric(val), 1)
      }
      # confirm edits
      confirmEdit(session, tbl = "mtcars", row = row, col = col, id = id, value = val);
    })
  })
  
  
  ## use reactive and observe elements
    ## render elements
  output$mtcars <- renderD3tf({
    
    # Define table properties. See http://tablefilter.free.fr/doc.php
    # for a complete reference
    tableProps <- list(
      btn_reset = TRUE,
      # alphabetic sorting for the row names column, numeric for all other columns
      col_types = c("string", rep("number", ncol(mtcars)))
    );
    
    d3tf(mtcars,
         tableProps = tableProps,
         extensions = list(
           list(name = "sort")
         ),
         showRowNames = TRUE,
         tableStyle = "table table-bordered");
  })
  

  
}