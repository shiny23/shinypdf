

server <- function(input, output,session) {
 
  #leaflet map is created here, because it is used in both the app and in the report
  lf <- reactive({
    leaflet() %>% 
      addTiles((urlTemplate = "https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png") ) %>%
      fitBounds(176.4991,-39.7270,177.0382,-39.4487) %>% 
      addMapPane(name = "raster", zIndex = 410) %>% 
      addMapPane(name = "wells", zIndex = 420)
  })
  

  output$map <- renderLeaflet({

    lf()
  })
  #observe map click and update input vith coordinates
  observe({
    if(input$pump_in_type == "single point"){
      #input$map_clic
      click <- input$map_shape_click
      if(is.null(click))
        return()
      click_xy <- data.frame (lat = as.numeric(click$lat),
                              lon = as.numeric(click$lng)) 
      #transfrm coordinates to nztm
      click_xy <- st_as_sf(click_xy,coords = c("lon", "lat"),crs = 4326)
      click_xy <- st_transform(click_xy,2193)
      click_xy$E <- st_coordinates(click_xy)[,1]
      click_xy$N <- st_coordinates(click_xy)[,2]
      click_xy <- st_set_geometry(click_xy, NULL)
      click_xy_txt <- paste(click_xy[1,1],click_xy[1,2])
      E <- click_xy[1,1]
      N <- click_xy[1,2]
      updateNumericInput(session,
                         inputId = "E",
                         label = "Easting:",
                         value = E
      )
      updateNumericInput(session,
                         inputId = "N",
                         label = "Northing:",
                         value = N
      )  
    }
    
  })
  
  wells_csv <- reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read_csv(inFile$datapath)
    
    
  })
  
  well_csv_row <- reactive({
    NROW(wells_csv())
  })
  
  
  
  well_csv_ready <- reactive({
    #if(exists ("wells_csv()" )==T){
    if( well_csv_row()>0){
      T
      # }
    }else{F}
  })

  wells <- reactive({
    #input
    if (well_csv_ready() == T & input$pump_in_type == 'upload csv') {
      wells <-  wells_csv() %>% 
        mutate(type = NA) %>% 
        select(x,y,Q,L,bore,type)
      
    } else if (input$pump_in_type == "historical data" &  length(input$abstr_type >0) ){
      load("data/wells/wells_hist.rdata")
      wells <- wells_hist %>% 
        filter(type %in%  input$abstr_type) %>% 
        mutate(Q=-Q) %>% 
        select(x,y,Q,L,bore,type,date)
      
      if (input$period_type == "dry summer 2012-2013"){
        dates_range <- parse_date_time (c("1/11/2012","1/03/2013"),"dmy")
      }
      else if (input$period_type =="date range"){
        dates_range <- as_datetime(as_date(input$date_range_hist,origin = "1970-01-01"))
        
      }
      wells <- wells %>% 
        filter(between(date,dates_range[1],dates_range[2]))  %>%  
        filter(type %in%  input$abstr_type) %>%
        group_by(x,y,L,bore,type) %>%
        summarise(Q = mean(Q))
      
      
    }
    else{

      wells <- data.frame(x = input$E,
                          y= input$N,
                          Q= input$Q,
                          L=1,
                          bore = NA,
                          type= NA) 
    }
    
    wells
  })
  
  wells_total <- reactive({
    round(sum(wells()$Q),digits = 1)
  })
  

  wells1 <- reactive({

    wells1 <- st_as_sf(wells(),coords = c("x", "y"),crs = 2193)
    wells1 <- st_transform(wells1,4326)
    wells1
  })
  

  wells_sp <- reactive({

    
    wells_b <- bind_rows(wells(),wells_dummy)
    
    wells1_1 <- wells_b %>% 
      filter(L==1) %>% 
      st_as_sf(coords = c("x", "y"),crs = 2193,remove = F)
    wells_sp1 <- as(wells1_1, 'Spatial')
    
    wells1_2 <- wells_b %>% 
      filter(L==2) %>% 
      st_as_sf(coords = c("x", "y"),crs = 2193,remove = F)
    wells_sp2 <- as(wells1_2, 'Spatial')
    
    wells_sp <-list(wells_sp1,wells_sp2)
    wells_sp
  })
  
  wells_list <- reactive( {
    wells1_1 <- wells() %>% 
      filter(L==1)
    wells1_2 <- wells() %>% 
      filter(L==2)
    wells_sp1 <- wells_sp()[[1]]
    wells_sp2 <- wells_sp()[[2]]
    wells_list <- list(wells_sp1,wells_sp2,wells1_1,wells1_2)
    wells_list
  })
  

  
  vals <- c("PWS","IND","irr")
  pal <- colorFactor(
    palette = 'Spectral',
    domain = vals)
  
  #generate a map with selected wells
  map_point <- observe({
    #add a marker to the map
    proxy <- leafletProxy("map")
    proxy %>% 
      clearGroup(group = "markers") %>% 

      addCircleMarkers(data = wells1(),  
                       radius = ~Q/5,#4
                       fillColor = ~pal(type),
                       stroke = T,
                       color = "black",
                       weight = 1,
                       fillOpacity = .7,
                       group = "markers",
                       options = leafletOptions(pane = "wells")
                       
      )%>% 
      addLegend(layerId = "legend2",
                title = "abstraction type",
                pal = pal,
                values= vals ) 
  })
  
  time <- reactive({
    time <- input$time
    time <- as.integer(time)
    df <- data.frame(comm_time=c(time)) %>% 
      left_join(times) %>% 
      select(id)
    time <- unlist(df)
    
  })
  
  time2 <- reactive({
    time2 <- times %>% 
      filter(id == time()) %>% 
      select(comm_time) 
    time2 <- unlist(time2)
    time2 <- unname(time2)
    time2
  })
  output$time2 <- renderText(time2())
  
  zone <- reactive({
    input$zone

  })
  
  
  zone2 <- reactive({
    zone2 <- rivers %>% 
      filter(river == zone()) %>% 
      select(descr) 
    zone2 <- unlist(zone2)
    zone2 <- unname(zone2)
    zone2
  })
  
  output$zone2 <- renderText(zone2())
  

  perc <- "Q50"
  RF_poly0 <- reactive({
    #function to load raster
    RF_comb <- SDZ_imp(zone(),time(),perc)
    #output
    RF_L1 <- RF_comb[[1]]
    RF_L2 <- RF_comb[[2]]
    

    if(as.numeric(input$Layer) == 1){
      RF <- RF_L1
    }  
    else{
      RF <- RF_L2
    }
    crs(RF) <- "+proj=tmerc +lat_0=0.0 +lon_0=173.0 +k=0.9996 +x_0=1600000.0 +y_0=10000000.0 +datum=WGS84 +units=m"
    names(RF) <- "SDR"
    RF

  })
  
  RF_poly1 <- reactive({
    RF_poly <- rasterToPolygons(RF_poly0(),na.rm=T) %>% 
      st_as_sf() %>% 
      st_set_crs(value= 2193) %>% 
      st_transform(crs = 4326)
    names(RF_poly) <- c("SD","geometry")
    RF_poly
  })
  
  
  
  labels1 <- reactive({
    labels <- as.character(paste("SD",round(RF_poly1()$SD*100,2),"%",sep=" "))
    labels
  })
  

  map_raster <- observe({
    
    
    proxy <- leafletProxy("map")
    proxy %>% 
      clearGroup(group = "poly") %>% 
      addPolygons(group = "poly",
                  data=RF_poly1(),
                  opacity = 1,
                  fillOpacity = 0.7,
                  stroke = F,
                  fillColor = ~cb(SD*100),
                  label= labels1(),
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto",
                    clickable = F),
                  options = leafletOptions(pane = "raster")
      ) %>% 
      addLegend(layerId = "legend",
                pal = cb,
                values= at ) 
    
  })
  

  tot_effect_zones_df <- reactive({
    
    for (z in 1: NROW(rivers)){
      
      zone_z <- unlist(rivers$river[z])
      river_z <- unlist(rivers$descr[z])
      
      for (p in 1:3){
        perc_p <- perc_s[p] 
        res <- data.frame(SD_tot =tot_eff2_sum(tot_eff2(wells_list(),zone_z,time(),perc_p)))%>% 
          mutate(zone = zone_z,
                 river = river_z,
                 perc = perc_p )
        if(z==1 & p==1) {
          res1 <- res
        } else {
          res1 <- bind_rows(res1,res) 
        }
      }
    }
    res1 <- res1 %>% 
      spread(perc,SD_tot) %>% 
      mutate(river1 = str_wrap(river,width = 10))
    res1
  })
  
  tot_effect_zones_df_print <- reactive({
    df <- tot_effect_zones_df() %>% 
      select(-c(river1,river) ) %>% 
      mutate_at(vars(Q10,Q50,Q90),funs(round(.,2)))
  })
  

  tot_effect_times_df <- reactive({

    zone <- zone()

    for (t in 1: NROW(times)){
      
      time_t <- unlist(times[t,1])
      time_t2 <- unlist(times[t,3])
      for (p in 1:3){
        perc_p <- perc_s[p] 
        res <- data.frame(SD_tot =tot_eff2_sum(tot_eff2(wells_list(),zone,time_t,perc_p))) %>% 
          mutate(time = time_t,
                 perc = perc_p,
                 time2 = time_t2)
        if(t==1 & p==1) {
          res1 <- res
        } else {
          res1 <- bind_rows(res1,res) 
        }
      }
    }
    
    res2 <- res1 %>% 
      spread(perc,SD_tot)
    res2
  })
  
  tot_effect_times_df_print <-  reactive({
    df <- tot_effect_times_df() %>% 
      dplyr::select(-time) %>% 
      rename(time = time2) %>% 
      mutate_all(funs(round(.,2)))
    
    df
  })
  
  
  
  
  SDZ_perc <- reactive({
    SDZ_perc <- round(sd_extract(wells1(),zone(),time(),as.numeric(input$Layer))*100,2)
  })
  
  tot_effect_df <- reactive({
    perc = "Q50"
    tot_effect_df <- tot_eff2(wells_list(),zone(),time(),perc) %>% 
      mutate(perc = "Q50")
    tot_effect_df
  })
  tot_effect10_df <- reactive({
    perc = "Q10"
    tot_effect_df <- tot_eff2(wells_list(),zone(),time(),perc) %>% 
      mutate(perc = "Q10")
    tot_effect_df
  })
  tot_effect90_df <- reactive({
    perc = "Q90"
    tot_effect_df <- tot_eff2(wells_list(),zone(),time(),perc)%>% 
      mutate(perc = "Q90")
    tot_effect_df
  })
  
  
  tot_effect_df_print <- reactive({
    df <- bind_rows(tot_effect_df(),tot_effect10_df(),tot_effect90_df()) %>% 
      rename(SDR = SD,
             SD = SD_tot,
             quantile = perc) %>% 
      mutate_at(vars(SD,SDR),funs(round(.,2))) %>% 
      select(x,y,L,bore,type,Q,SDR,SD,quantile)
    
  })
  
  
  
  tot_effect <- reactive({
    perc = "Q50"
    df <- tot_eff2_sum(tot_effect_df())
    df <- round(df,1)
    df
  })
  
  
  tot_effect_10 <- reactive({
    perc = "Q10"
    df <- tot_eff2_sum(tot_eff2(wells_list(),zone(),time(),perc))
    df <- round(df,1)
    df
  })
  
  tot_effect_90 <- reactive({
    perc = "Q90"
    df <- tot_eff2_sum(tot_eff2(wells_list(),zone(),time(),perc))
    df <- round(df,1)
    df
  })
  
  tot_effect_all <- reactive({data.frame(`SD` = c(tot_effect(),tot_effect_10(),tot_effect_90()),
                               row.names = c("mean Q50","minimum Q10","maximum Q90"))})
  SDR_all <- reactive({data.frame(`SDR` = SDZ_perc(),
                        row.names = c("mean Q50","minimum Q10","maximum Q90"))})
  
  results_df <- reactive({if(input$pump_in_type == "single point"){ 
    results_df <- bind_cols(tot_effect_all(),SDR_all())
    results_df
  }else{
    results_df <-  tot_effect_all()
    results_df
  }
    row.names(results_df) <- c("most likely mean: (Q50)","minimum (Q10)","maximum (Q90)")
    results_df
    })
  
  dt_col_names <- reactive({
    if(input$pump_in_type == "single point"){ 
      c("SD (L/s)","SDR (%)")
      }else{
      c("SD (L/s)")
      }  
    }) 
  
  output$results_table <- DT::renderDataTable(datatable(results_df(),
                                                        rownames = c("Q_50","Q_10","Q_90"),
                                                        colnames = dt_col_names()))
  

  chart_bar1 <- reactive({
    chart <- tot_effect_zones_df() %>% 
      ggplot(aes(zone,Q50,fill = river))+
      geom_bar(stat = "identity")+
      geom_errorbar(aes(ymin = Q10, ymax = Q90),
                    width = .2)+
      theme(legend.position = "none",axis.text.x = element_text(angle = 90, hjust = 1))+
      ylab("Effect L/s")+
      xlab("Streams")
    
    chart
    #+ theme(legend.position = "none"))
  })
  
  
  
  output$total_Q <- (renderText(paste("Total rate:",wells_total(),"L/s")))
  
  
  
  
  output$bar_chart <- renderPlotly({
        ggplotly(chart_bar1())
  })
  
  
  chart_line1 <- reactive({
    chart <- ggplot(data =tot_effect_times_df(),aes(x=time2,y=Q50))+
      geom_ribbon(aes(ymin=Q10, ymax=Q90),fill = "gray70")+
      geom_line(col = "red")+
      theme(legend.position = "none",axis.text.x = element_text(angle = 90, hjust = 1))+
      ylab("Effect L/s")+
      xlab("Time days")
    chart
  })
  
  output$line_chart <- renderPlotly({

    ggplotly(chart_line1())

  })
  output$test1 <- renderText(map_raster())
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else round(d$y,1)
  })
  
  output$wells_tab <- renderDataTable(tot_effect_df_print())
  output$zones_tab <- renderDataTable(tot_effect_zones_df_print())
  output$times_tab <- renderDataTable(tot_effect_times_df_print())

  
  output$downloadReport <- downloadHandler(
    filename = "report.pdf",
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(chart_line = chart_line1(),
                     chart_bar = chart_bar1(),
                     map = lf(),
                     wells = wells1(),
                     raster = RF_poly0(),
                     labels = labels1(),
                     zone = zone2(),
                     time  = time2(),
                     pumping_type = input$pump_in_type,
                     abstr_type = input$abstr_type,
                     period_type = input$period_type,
                     date_range_hist = input$date_range_hist,
                     results_df = results_df(),
                     SD_per_bore = tot_effect_df_print(),
                     SD_per_river = tot_effect_zones_df_print(),
                     SD_per_times = tot_effect_times_df_print(),
                     include_wells = input$include_wells,
                     total_pump = wells_total()
                     )

      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())#,
      )
    }
  )
  
  output$downloadData_bore_template <- downloadHandler(
    
      filename = "bore_template.csv",
      content = function(file) {
          file.copy("data/wells/wells.csv", file)
    }
  )
  
  
  output$downloadData_bore <- downloadHandler(
    filename = "SD_per_bore.csv",
    content = function(file) {
      write.csv(tot_effect_df_print() , file = file, row.names = FALSE)
    }
  )
  
  output$downloadData_river <- downloadHandler(
    filename = "SD_per_river.csv",
    content = function(file) {
      write.csv(tot_effect_zones_df_print() , file = file, row.names = FALSE)
    }
  )
  
  output$downloadData_time <- downloadHandler(
    filename = "SD_per_times.csv",
    content = function(file) {
      write.csv(tot_effect_times_df_print() , file = file, row.names = FALSE)
    }
  )
}