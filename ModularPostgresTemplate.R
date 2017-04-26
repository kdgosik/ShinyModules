## require()  for required libraries for module
library(RPostgreSQL)
library(magrittr)
library(dplyr)

pgsrc <-
  read.csv( '/projects/creds.csv', stringsAsFactors = FALSE) %$%
  src_postgres(
    host = 'postgres'
    , dbname = 'dev'
    , port = 5432
    , user = username
    , password = password
    , options="-c search_path=public"
  )

## postgres
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- read.csv( '/projects/creds.csv', stringsAsFactors = FALSE ) %$%
  dbConnect(drv, dbname = "dev",
            host = "postgres", port = 5432,
            user = username, password = password)



# MODULE UI
PostgresTemplateUI <- function(id) {
  ns <- NS(id)

  ## Ui Outputs Here from server below

  ## for mutliple Output use fillCol/fillRow(), or flowLayout() wrapped around Outputs
    
    ## Internal Inputs
      # selectInput()
      # selectizeInput()
      # numericInput() ...

}



# MODULE Server
PostgresTemplateServer <- function(input, output, session, outside_inputs...) {
  
  out <- reactive({
    
    dat <- pgsrc %>%
      tbl("*Postgres Table Name*") %>%
      filter( ) %>%
      collect %>%
      mutate( ) %>%
      filter( ) %>%
      select( ) %>%

    
    dbDisconnect(con)
    dat
    
  })
  
  return(out)
  
}