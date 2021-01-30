library(shiny)
library(googleVis)
library(leaflet)

shinyServer(function(input, output,session){
    
    
    observe({
        
        layer_type_list_updated=unique(df_pavement_info %>% filter(STATE_CODE_EXP==input$state_section_data) %>% select(LAYER_TYPE))
        updateSelectizeInput(session,"layer_type_data",choices=layer_type_list_updated,selected = layer_type_list_updated[1])
        
        temperature_year_list_modified=sort(unique(df_temperature %>% filter(STATE_CODE_EXP==input$temperature_state) %>% select(YEAR)),decreasing = FALSE)
        updateSelectInput(session,"temperature_year",choices = temperature_year_list_modified,selected = temperature_year_list_modified[1])
        
        temperature_month_number_list_modified=sort(unique(df_temperature %>% filter(STATE_CODE_EXP==input$temperature_state&YEAR==input$temperature_year)),decreasing = FALSE)
        x=temperature_month_list[temperature_month_number_list_modified]
        updateSelectInput(session,"temperature_month",choices = x,selected = x[1])
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
        caption='Data from LTPP',tag='Figure 3',x='Elevation (ft)',y='Frequency')+
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
    
    # show ave temp
    # output$ave_temperature <- renderPlot({
    #     
    #     df_ave_temperature_year=df_temperature %>% filter(STATE_CODE_EXP==input$temperature_state) %>% group_by(YEAR) %>% summarise(temp_ave=mean(Tave_F))
    #     ggplot(df_ave_temperature_year,aes(x=YEAR,y=temp_ave))+geom_point(color='red',size=2)+
    #     labs(title=paste('Annual Average Temperature for',input$temperature_state,sep = ' '),
    #     caption=('Data from LTPP'),tag='Figure 1',x='Year',y='Temperature (F)')+theme_bw()
    #     
    # })
    # 
    # #show SHRP for all states
    # output$ave_temperature_year <- renderPlot({
    #     
    #     x=sort(df_temperature %>% filter(STATE_CODE_EXP==input$temperature_state,YEAR==input$temperature_year) %>% select(MONTH),decreasing = FALSE)
    #     x_name=temperature_month_list[x]
    #     
    #     df_ave_temperature_year_month=df_temperature %>% filter(STATE_CODE_EXP==input$temperature_state,YEAR==input$temperature_year) %>% group_by(MONTH,SHRP_ID) %>% 
    #     summarise(temp_ave=mean(Tave_F)) %>% arrange(ascend(MONTH))
    #     ggplot(df_ave_temperature_year_month,aes(x=MONTH,y=temp_ave))+geom_point(color='red',size=2)+
    #     labs(title=paste('Annual Average Temperature for',input$temperature_state,sep = ' '),
    #     caption=('Data from LTPP'),tag='Figure 2',x='Month',y='Temperature (F)')+scale_x_discrete(labels=x_name)+theme_bw()
    #     
    # })
    # 
    # output$ave_temp_shrp_month <- renderPlot({
    #     
    #   df=df_temperature %>% filter(STATE_CODE_EXP==input$temperature_state,YEAR==input$temperature_year)
    #   df_min=df %>% group_by(MONTH) %>% summarise(min_F=min(Tave_F))
    #   df_max=df %>% group_by(MONTH) %>% summarise(max_F=max(Tave_F))
    #   
    #   ggplot()+geom_smooth(aes(x=df$MONTH,y=df$Tave_F),color='black')+
    #   geom_smooth(aes(x=df_min$MONTH,y=df_min$min_F),color='blue',se=FALSE)+
    #   geom_smooth(aes(x=df_max$MONTH,y=df_max$max_F),color='red',se=FALSE)
    # })
    # 
    # # show map
    # output$ave_temperature_location <- renderLeaflet({
    #     
    #     df=df_temperature %>% filter(STATE_CODE_EXP==input$temperature_state&YEAR==input$temperature_year&MONTH=input$temperature_month) %>% 
    #     select(SHRP_ID,LATITUDE,LONGITUDE,ELEVATION,Tave_F) %>% group_by(SHRP_ID,LATITUDE,LONGITUDE,ELEVATION) %>% summarise(temp_ave=mean(Tave_F))
    #     max_temp=max(df$temp_ave)
    #     min_temp=min(df$temp_ave)
    #     temp_bin=seq(min_temp,max_temp,input$temperature_slider)
    #     qpal <- colorBin("YlOrRd", domain = df$temp_ave, bins = temp_bin)
    #     
    #     leaflet(df) %>% addTiles() %>% addCircles(lng=~LONGITUDE,lat=~LATITUDE,color= ~qpal(temp_ave)) %>% 
    #     addLegend("bottomright", pal = qpal, values = ~temp_ave)
    # })
    
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

    