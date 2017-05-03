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
TeletrackingTimelineUI <- function(id) {
  ns <- NS(id)

  ## Ui Outputs Here from server below
  units <- pgsrc %>%
    tbl("teletrackingtransfers") %>%
    select(OccupiedLocationNurseStationADTID) %>%
    collect(n = Inf) %>% 
    unique

  ## for mutliple Output use fillCol/fillRow(), or flowLayout() wrapped around Outputes
  selectizeInput(ns("unit_id"), "Select Units", choices = units, multiple = TRUE)
}



# MODULE Server
TeletrackingTimelineServer <- function(input, output, session, date_range) {
  
  out <- reactive({

      dat <- pgsrc %>%
        tbl("teletrackingtransfers") %>%
        filter(PatientADTDischargeDateTimeUTC > date_range[1] & 
               PatientADTDischargeDateTimeUTC < date_range[2] &
               !is.na(PatientAdmitDateTimeUTC)) %>%
        collect(n = Inf) %>%
        arrange(PatientDetailPatientVisitNumber, OccupyDateTimeUTC) %>%
        group_by(PatientDetailPatientVisitNumber) %>%
        mutate(OccupyDateTimeLag = lead(OccupyDateTimeUTC, 1)) %>%
        mutate(OccupyDateTimeLag = ifelse(is.na(OccupyDateTimeLag), PatientADTDischargeDateTimeUTC, OccupyDateTimeLag)) %>%
        mutate(OccupyDateTimeLag = as.POSIXct(OccupyDateTimeLag, origin = "1970-01-01")) %>% 
        filter(!is.na(OccupyDateTimeUTC)) %>%
        ungroup
        
        if( !is.null(input$unit_id) ) dat %<>% filter(OccupiedLocationNurseStationADTID %in% input$unit_id)
        
        dat %<>% 
          select(content = OccupiedLocationRoomADTID,
                 start = OccupyDateTimeUTC,
                 end = OccupyDateTimeLag,
                 group = OccupiedLocationNurseStationADTID) %>%
        arrange(group, start) %>%
        mutate(id = 1 : n()) %>%
        select(id, everything())
    
      dbDisconnect(con)
      
      dat
    
  })
  
  return(out)
  
}