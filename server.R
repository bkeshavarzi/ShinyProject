library(shiny)
library(googleVis)
library(leaflet)

shinyServer(function(input, output){
    # show map for AC using googleVis
    output$ACPavements <- renderGvis({
        gvisGeoChart(df_ac, "States", "Number",options=list(region="US", displayMode="regions", resolution="provinces",
        width="auto", height="auto",colorAxis="{values:[1, 179],colors:[\'blue',\'red']}"))
    })
    
    # show map for PC using googleVis
    output$PCPavements <- renderGvis({
        gvisGeoChart(df_pc,"States","Number",options=list(region="US", displayMode="regions", resolution="provinces",
        width="auto", height="auto",colorAxis="{values:[1, 100],colors:[\'blue',\'red']}"))
    })
    
    # show map for Average Temperature using googleVis
    output$AverageTemperature <- renderGvis({
        df_temperature_month_year=df_temperature %>% filter(YEAR==input$temperature_year_id & MONTH==input$temperature_month_id)
        gvisGeoChart(df_mera_temp_month,"LatLong",colorvar ='Average Teperature',sizevar='ELEVATION',
        options=list(region="US",colorAxis="{values:[6, 78],colors:[\'blue',\'red']}"))
    })
    
    # show map for Traffic using googleVis
    output$Traffic <- renderGvis({
        df_traffic_year=df_traffic %>% filter(YEAR==input$traffic_year)
        gvisGeoChart(df_traffic_year, "LatLong", colorvar='ESAL',sizevar='ELEVATION',
        options=list(region="US",resolution="provinces",colorAxis="{values:[0, 4000000],colors:[\'blue',\'red']}"))
    })
    #show SHRP for all states
    output$SHRP <- renderLeaflet({
        leaflet(df_gps) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BZH") %>% addMarkers(lng=df_gps$LONGITUDE,lat=df_gps$LATITUDE,popup = df_gps$tip)
    })
    
    # show histogram for distributation of layer thickness
    output$LayerDistributation <- renderPlot({
        df_structure=df_pavement_info %>% filter(STATE_CODE_EXP==input$state1,LAYER_TYPE==input$layer_type1) %>% group_by(SHRP_ID,LAYER_TYPE,CONSTRUCTION_NO) %>%
        summarise(Thickness=sum(REPR_THICKNESS,na.rm = TRUE))
        bw=max(df_structure$Thickness)/input$bin1
        df_structure$CONSTRUCTION_NO=as.character(df_structure$CONSTRUCTION_NO)
        ggplot(df_structure,aes(x=Thickness,fill=CONSTRUCTION_NO))+
        geom_histogram(binwidth=bw,position = "dodge")+labs(title = paste("Number of",input$layer_type1,"Layer",sep = " "))+xlim(seq(0,max(df_structure$Thickness),5))
    })
    #show triffc year growth as a function of year
    output$Traffic_year_growth <- renderPlot({
        
        df_traffic_state_shrp=df_traffic %>% filter(STATE_CODE_EXP==input$state2 & SHRP_ID==input$SHRP)
        df_traffic_state_shrp$CONSTRUCTION_NO=as.character(df_traffic_state_shrp$CONSTRUCTION_NO)
        ggplot(df_traffic_state_shrp,aes(x=YEAR,y=ANNUAL_ESAL_TREND))+geom_point(aes(fill=CONSTRUCTION_NO))+labs(title = "ESAL vs. Year")+xlab("Year")+ylab("ESAL")
        
    })
    # show scater plot for ac layer as a function of traffic
    output$Traffic_AC_Thickness <-renderPlot({
        
        df_traffic_shrp_=df_traffic %>% filter(STATE_CODE_EXP==input$state2 & SHRP_ID==input$SHRP) %>% group_by(CONSTRUCTION_NO) %>% summarise(min_ESAL=min(ANNUAL_ESAL_TREND),max_ESAL=max(ANNUAL_ESAL_TREND))
        df_pavement_state_layer_shrp=df_pavement_info %>% filter(STATE_CODE_EXP==input$state2 & SHRP_ID==input$SHRP  & LAYER_TYPE=="AC") %>% group_by(CONSTRUCTION_NO) %>% summarise(st=sum(REPR_THICKNESS))
        df_final=inner_join(df_pavement_state_layer_shrp,df_traffic_shrp_,by="CONSTRUCTION_NO")
        ggplot(df_final)+geom_point(aes(x=st,y=min_ESAL,color='blue'))+geom_point(aes(x=st,y=max_ESAL,color='Red'))+labs(title = "ESAL vs. Thickness for Aspahlt")+xlab("Thickness (in)")+ylab("ESAL")
    })
    # show scater plot for PC layer as a function of traffic
    output$Traffic_PC_Thickness <-renderPlot({
        
        df_traffic_shrp_=df_traffic %>% filter(STATE_CODE_EXP=="California" & SHRP_ID==7452) %>% group_by(CONSTRUCTION_NO) %>% summarise(min_ESAL=min(ANNUAL_ESAL_TREND),max_ESAL=max(ANNUAL_ESAL_TREND))
        df_pavement_state_layer_shrp=df_pavement_info %>% filter(STATE_CODE_EXP=="California" & SHRP_ID==7452  & LAYER_TYPE=="PC") %>% group_by(CONSTRUCTION_NO) %>% summarise(st=sum(REPR_THICKNESS))
        df_final=inner_join(df_pavement_state_layer_shrp,df_traffic_shrp_,by="CONSTRUCTION_NO")
        ggplot(df_final)+geom_point(aes(x=st,y=min_ESAL,color='blue'))+geom_point(aes(x=st,y=max_ESAL,color='Red'))+labs(title = "ESAL vs. Thickness for Concrete")+xlab("Thickness (in)")+ylab("ESAL")
        
    })
    
    
    
    
    
    
    
    
    # show data using DataTable
    output$table <- DT::renderDataTable({
        datatable(state_stat, rownames=FALSE) %>% 
            formatStyle(input$selected, background="skyblue", fontWeight='bold')
    })
    
    # show statistics using infoBox
    output$maxBox <- renderInfoBox({
        max_value <- max(state_stat[,input$selected])
        max_state <- 
            state_stat$state.name[state_stat[,input$selected] == max_value]
        infoBox(max_state, max_value, icon = icon("hand-o-up"))
    })
    output$minBox <- renderInfoBox({
        min_value <- min(state_stat[,input$selected])
        min_state <- 
            state_stat$state.name[state_stat[,input$selected] == min_value]
        infoBox(min_state, min_value, icon = icon("hand-o-down"))
    })
    output$avgBox <- renderInfoBox(
        infoBox(paste("AVG.", input$selected),
                mean(state_stat[,input$selected]), 
                icon = icon("calculator"), fill = TRUE))
})