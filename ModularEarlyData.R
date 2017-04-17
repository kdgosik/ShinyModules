
# MODULE Server
EarlyDataServer <- function(input, 
                            output, 
                            session, 
                            pgsrc,
                            table,
                            inpt_date, 
                            inpt_group,
                            inpt_unit,
                            inpt_mins,
                            inpt_interval,
                            cfg.costcenters) {
  
  EarlyWorkingData <- reactive({
    
    dat <- dplyr::tbl(pgsrc, table) %>%
      dplyr::filter(PunchDate_UTC > inpt_date[1] &
                      PunchDate_UTC < inpt_date[2] &
                      MinutesEarly > inpt_mins[1] &
                      MinutesEarly < inpt_mins[2] &
                      CostCenter %in%  inpt_group) %>%
      select(-row.names)
    
    if( inpt_unit != "All Units" ) {
      dat <- dat %>%
        filter(CostCenter == cfg.costcenters[cfg.costcenters$UnitName == inpt_unit, "CostCenter"])
    }
    
    collect(dat)
    
  })
  
  
  EarlyTrendData <- reactive({

    if( inpt_interval != "payperiod" ) {

      working_data <- EarlyWorkingData() %>%
        dplyr::mutate(Date = floor_date(PunchDate_UTC, unit = inpt_interval)) %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(Minutes = sum(MinutesEarly)) %>%
        dplyr::mutate(Hours = round(Minutes/60, 2), Cost = 30 * Hours)

    }else{

      working_data <-EarlyWorkingData() %>%
        dplyr::mutate(Date = ppend(PunchDate_UTC)) %>%
        dplyr::group_by(Date) %>%
        dplyr::summarise(Minutes = sum(MinutesEarly)) %>%
        dplyr::mutate(Hours = round(Minutes/60, 2), Cost = 30 * Hours)
    }

    working_data
  })


  EarlyUnitData <- reactive({

    EarlyWorkingData() %>%
      inner_join(cfg.costcenters, "CostCenter") %>%
      mutate(Date = floor_date(ymd_hms(PunchDate_UTC), unit = "month")) %>%
      arrange(Date) %>%
      group_by(Date, UnitName) %>%
      summarise(Minutes = sum(MinutesEarly)) %>%
      spread(UnitName, Minutes)

  })

  list(Raw = EarlyWorkingData,
       Trend = EarlyTrendData,
       Unit = EarlyUnitData)

}