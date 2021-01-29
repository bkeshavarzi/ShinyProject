library(shiny)
library(googleVis)
library(leaflet)

shinyServer(function(input, output,session){
    
    
    observe({
        
        layer_type_list_updated=unique(df_pavement_info %>% filter(STATE_CODE_EXP==input$state_section_data) %>% select(LAYER_TYPE))
        updateSelectizeInput(session,"layer_type_data",choices=layer_type_list_updated,selected = layer_type_list_updated[1])
          
    })
    
    # show histogram for thickness
    
    output$thickness_histogram <- renderPlot({
        
        df_state_duration=df_pavement_SHRP %>% filter(STATE_CODE_EXP==input$state_section_data)
        df_state_layer=df_pavement_info %>% filter(STATE_CODE_EXP==input$state_section_data&LAYER_TYPE==input$layer_type_data) %>% group_by(SHRP_ID,CN) %>% summarise(sum_=sum(REPR_THICKNESS))
        min_year=min(df_state_duration$syear)
        max_year=max(df_state_duration$eyear)
        min_thickness=min(df_state_layer$sum_)
        max_thickness=max(df_state_layer$sum_)
        vec=round(seq(seq(min_thickness, max_thickness, (max_thickness-min_thickness)/input$thickness_histogram_slider)),2)
        ggplot(df_state_layer,aes(x=sum_),fill=CN)+
        geom_histogram(bins=input$thickness_histogram_slider,position = "stack")+labs(title=paste(input$layer_type_data,'Thickness Distributation For',input$state_section_data,sep=' '),
        subtitle = paste(as.character(min_year),as.character(max_year),sep='-'),caption='Data from LTPP',tag='Figure 1',x=paste(input$layer_type_data,'Layer Thickness (in)'),y='Frequency',colour = "CN")+ 
        theme_bw()+scale_x_continuous(breaks =vec )
        
    })
    
    # show map for duration
    
    output$duration_histogram <- renderPlot({
        
        df_state_duration=df_pavement_SHRP %>% filter(STATE_CODE_EXP==input$state_section_data)
        min_year=min(df_state_duration$syear)
        max_year=max(df_state_duration$eyear)
        max_duration=max(df_state_duration$duration)
        min_duration=min(df_state_duration$duration)
        vec=round(seq(min_duration,max_duration,(max_duration-min_duration)/input$duration_histogram_slider),2)
        ggplot(df_state_duration,aes(x=duration))+geom_histogram(bins = input$duration_histogram_slider)+labs(title=paste('Section Life Distributation','in',input$state_section_data,sep=' '),
        subtitle = paste(as.character(min_year),as.character(max_year),sep='-'),caption='Data from LTPP',tag='Figure 2',x='Section Life (Year)',y='Frequency')+
        theme_bw()+scale_x_continuous(breaks = )
        
    })
    
    #show map for elevation
    
    output$elevation_histogram <- renderPlot({
        
        df_state_duration=df_pavement_SHRP %>% filter(STATE_CODE_EXP==input$state_section_data)
        df_gps_state=df_gps %>% filter(STATE_CODE_EXP==input$state_section_data)
        min_year=min(df_state_duration$syear)
        max_year=max(df_state_duration$eyear)
        min_elevation=min(df_gps_state$ELEVATION)
        max_elevation=max(df_gps_state$ELEVATION)
        vec=round(seq(min_elevation,max_elevation,(max_elevation-min_elevation)/input$elevation_histogram_slider),1)
        ggplot(df_gps_state,aes(ELEVATION))+geom_histogram(bins = input$elevation_histogram_slider)+labs(title = paste('Section Elevation in',input$state_section_data,sep = ' '),
        subtitle = paste(as.character(min_year),as.character(max_year),sep='-'),
        caption='LTPP Data',tag='Figure 3',x='Elevation (ft)',y='Frequency')+
        theme_bw()+scale_x_continuous(breaks = vec)
        
    })
    
    # show map for sections in the state using googleVis
    output$section_map <- renderLeaflet({
        df_gps_state=df_gps %>% filter(STATE_CODE_EXP==input$state_section_data)
        leaflet(df_gps_state) %>% addTiles() %>% addMarkers(lng=df_gps_state$LONGITUDE,lat=df_gps_state$LATITUDE) %>% addProviderTiles('Stamen.Terrain') %>% 
            addMeasure(position = 'bottomleft',primaryLengthUnit = 'feet',primaryAreaUnit = 'sqfeet')
    })
    
    output$thickness_table <- DT::renderDataTable(DT::datatable({
        df_state_layer=df_pavement_info %>% filter(STATE_CODE_EXP==input$state_section_data&LAYER_TYPE==input$layer_type_data) %>% group_by(SHRP_ID,CN) %>% summarise(sum_=sum(REPR_THICKNESS))
        df_state_layer$sum_=round(df_state_layer$sum_,2)
        colnames(df_state_layer)=c('SHRP','CN','Thickness (in)')
        caption=paste("Section Thickness for",input$state_section_data,'Layer type :',input$layer_type_data,sep = ' ')
        df_state_layer
    }))
    output$life_table <- DT::renderDataTable(DT::datatable({
        df_gps_state=df_gps %>% filter(STATE_CODE_EXP==input$state_section_data)
        df_gps_state=df_gps_state %>% select(SHRP_ID,LATITUDE,LONGITUDE,ELEVATION)
        df_gps_state$LATITUDE=round(df_gps_state$LATITUDE,4)
        df_gps_state$LONGITUDE=round(df_gps_state$LONGITUDE,4)
        colnames(df_gps_state)=c('SHRP','Latitude','Longitude','Elevation (ft)')
        caption=paste("Section Elevations for",input$state_section_data,sep = ' ')
        df_gps_state
    }))
    
    # show map for Traffic using googleVis
    #output$Traffic <- renderGvis({
        #df_traffic_year=df_traffic %>% filter(YEAR==input$traffic_year)
        #gvisGeoChart(df_traffic_year, "LatLong", colorvar='ESAL',sizevar='ELEVATION',
                     #options=list(region="US",resolution="provinces",colorAxis="{values:[0, 4000000],colors:[\'blue',\'red']}"))
    #})
    
    #show SHRP for all states
    #output$SHRP <- renderLeaflet({
        #leaflet(df_gps) %>% addTiles() %>% addProviderTiles("OpenStreetMap.BZH") %>% addMarkers(lng=df_gps$LONGITUDE,lat=df_gps$LATITUDE,popup = df_gps$tip)
    #})
    
    # show histogram for distributation of layer thickness
    #output$LayerDistributation <- renderPlot({
        #print(input$state1)
        #print(input$layer_type1)
        #df_structure=df_pavement_info %>% filter(STATE_CODE_EXP==input$state1,LAYER_TYPE_EXP==input$layer_type1) %>% select(!STATE_CODE_EXP & !LAYER_NO & !LAYER_TYPE_EXP) %>%  
            #group_by(SHRP_ID,LAYER_TYPE,CONSTRUCTION_NO) %>% summarise(Thickness=sum(REPR_THICKNESS,na.rm = TRUE))
        #bw=max(df_structure$Thickness)/input$bin1
        #df_structure$CONSTRUCTION_NO=as.character(df_structure$CONSTRUCTION_NO)
        #ggplot(df_structure,aes(x=Thickness,fill=CONSTRUCTION_NO))+
            #geom_histogram(binwidth=bw)+ylab(paste("Number of",input$layer_type1,"Layer",sep = " "))+scale_x_continuous(limits=c(min(df_structure$Thickness),max(df_structure$Thickness)))+
            #xlab("Thickness (in)")
    #})
    
    #show triffc year growth as a function of year
    #output$Traffic_year_growth <- renderPlot({
        
        #df_traffic_state_shrp=df_traffic %>% filter(STATE_CODE_EXP==input$state2 & SHRP_ID==input$SHRP) %>% select(!STATE_CODE & !STATE_CODE_EXP & !SHRP_ID)
        #df_traffic_state_shrp$CONSTRUCTION_NO=as.character(df_traffic_state_shrp$CONSTRUCTION_NO)
        #ggplot(df_traffic_state_shrp,aes(x=YEAR,y=ESAL))+geom_point(aes(color=CONSTRUCTION_NO))+labs(title = "ESAL vs. Year")+xlab("Year")+ylab("ESAL")
        
    #})
    
    # show scater plot for ac layer as a function of traffic
    #output$Traffic_AC_Thickness <-renderPlot({
        #df_traffic_shrp=df_traffic %>% filter(STATE_CODE_EXP==input$state2 & SHRP_ID==input$SHRP) %>% select(!STATE_CODE & !STATE_CODE_EXP & !SHRP_ID) %>% 
            #group_by(CONSTRUCTION_NO) %>% summarise(min_ESAL=min(ESAL),max_ESAL=max(ESAL))
        #df_pavement_state_layer_shrp=df_pavement_info %>% filter(STATE_CODE_EXP==input$state2 & SHRP_ID==input$SHRP  & LAYER_TYPE=="AC") %>% 
            #select(!STATE_CODE_EXP & !SHRP_ID & !LAYER_TYPE & !LAYER_NO & !LAYER_TYPE & !LAYER_TYPE_EXP) %>% group_by(CONSTRUCTION_NO) %>% summarise(st=sum(REPR_THICKNESS))
        #df_final=inner_join(df_pavement_state_layer_shrp,df_traffic_shrp,by="CONSTRUCTION_NO")
        #ggplot(df_final)+geom_point(aes(x=st,y=min_ESAL))+geom_point(aes(x=st,y=max_ESAL))+labs(title = "ESAL vs. Thickness for Aspahlt")+xlab("Thickness (in)")+ylab("ESAL")
    #})
    
})

    