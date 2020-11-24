library(shiny)
library(googleVis)
library(leaflet)

shinyServer(function(input, output,session){
    
    observe({
        
        month_option=unique(df_temperature %>% filter(YEAR==input$temperature_year_id) %>% select(MONTH))
        layer_option=unique(df_pavement_info %>% filter(STATE_CODE_EXP==input$state1) %>% select(LAYER_TYPE_EXP))
        
        shrp_option=unique(df_traffic %>% filter(STATE_CODE_EXP==input$state2) %>% select(SHRP_ID))
        updateSelectizeInput(session,"temperature_month_id",choices=month_option,selected = month_option[1])
        updateSelectizeInput(session,"layer_type1",choices=layer_option,selected = layer_option[1])
        updateSelectizeInput(session,"SHRP",choices=shrp_option,selected=shrp_option[1])

    })

     # show map for AC using googleVis
    output$ACPavements <- renderGvis({
        h=gvisGeoChart(df_ac, "States", "Number",options=list(region="US", displayMode="regions", resolution="provinces",
        width="auto", height="auto",colorAxis="{values:[1, 179],colors:[\'blue',\'red']}"))
    })
    
    # show map for PC using googleVis
    output$PCPavements <- renderGvis({
        gvisGeoChart(df_pc,"States","Number",options=list(region="US", displayMode="regions", resolution="provinces",
                                                          width="auto", height="auto",colorAxis="{values:[1, 100],colors:[\'blue',\'red']}"))
    })
    
    # show map for Average Temperature using googleVis
    output$AverageTemperature <- renderGvis({
        df_temperature_month_year=df_temperature %>% filter(YEAR==input$temperature_year_id & MONTH==input$temperature_month_id) %>% 
            mutate(size=1/Average_Temperature) %>% filter(!is.infinite(size))
        gvisGeoChart(df_temperature_month_year,"LatLong",colorvar ='Average_Temperature',sizevar = 'size',
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
        print(input$state1)
        print(input$layer_type1)
        df_structure=df_pavement_info %>% filter(STATE_CODE_EXP==input$state1,LAYER_TYPE_EXP==input$layer_type1) %>% select(!STATE_CODE_EXP & !LAYER_NO & !LAYER_TYPE_EXP) %>%  
            group_by(SHRP_ID,LAYER_TYPE,CONSTRUCTION_NO) %>% summarise(Thickness=sum(REPR_THICKNESS,na.rm = TRUE))
        bw=max(df_structure$Thickness)/input$bin1
        df_structure$CONSTRUCTION_NO=as.character(df_structure$CONSTRUCTION_NO)
        ggplot(df_structure,aes(x=Thickness,fill=CONSTRUCTION_NO))+
            geom_histogram(binwidth=bw)+ylab(paste("Number of",input$layer_type1,"Layer",sep = " "))+scale_x_continuous(limits=c(min(df_structure$Thickness),max(df_structure$Thickness)))+
            xlab("Thickness (in)")
    })
    
    #show triffc year growth as a function of year
    output$Traffic_year_growth <- renderPlot({
        
        df_traffic_state_shrp=df_traffic %>% filter(STATE_CODE_EXP==input$state2 & SHRP_ID==input$SHRP) %>% select(!STATE_CODE & !STATE_CODE_EXP & !SHRP_ID)
        df_traffic_state_shrp$CONSTRUCTION_NO=as.character(df_traffic_state_shrp$CONSTRUCTION_NO)
        ggplot(df_traffic_state_shrp,aes(x=YEAR,y=ESAL))+geom_point(aes(color=CONSTRUCTION_NO))+labs(title = "ESAL vs. Year")+xlab("Year")+ylab("ESAL")
        
    })
    
    # show scater plot for ac layer as a function of traffic
    output$Traffic_AC_Thickness <-renderPlot({
        df_traffic_shrp=df_traffic %>% filter(STATE_CODE_EXP==input$state2 & SHRP_ID==input$SHRP) %>% select(!STATE_CODE & !STATE_CODE_EXP & !SHRP_ID) %>% 
            group_by(CONSTRUCTION_NO) %>% summarise(min_ESAL=min(ESAL),max_ESAL=max(ESAL))
        df_pavement_state_layer_shrp=df_pavement_info %>% filter(STATE_CODE_EXP==input$state2 & SHRP_ID==input$SHRP  & LAYER_TYPE=="AC") %>% 
            select(!STATE_CODE_EXP & !SHRP_ID & !LAYER_TYPE & !LAYER_NO & !LAYER_TYPE & !LAYER_TYPE_EXP) %>% group_by(CONSTRUCTION_NO) %>% summarise(st=sum(REPR_THICKNESS))
        df_final=inner_join(df_pavement_state_layer_shrp,df_traffic_shrp,by="CONSTRUCTION_NO")
        ggplot(df_final)+geom_point(aes(x=st,y=min_ESAL))+geom_point(aes(x=st,y=max_ESAL))+labs(title = "ESAL vs. Thickness for Aspahlt")+xlab("Thickness (in)")+ylab("ESAL")
    })
    
})

    