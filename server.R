library(shiny)
library(googleVis)
library(leaflet)

shinyServer(function(input, output,session){
    
    
    observe({
        
        layer_type_list_updated=unique(df_pavement_info %>% filter(STATE_CODE_EXP==input$state_section_data) %>% select(LAYER_TYPE))
        updateSelectizeInput(session,"layer_type_data",choices=layer_type_list_updated,selected = layer_type_list_updated[1])
        
        # temperature_year_list_modified=sort(unique((df_temperature %>% filter(STATE_CODE_EXP==input$temperature_state) %>% select(YEAR))[,1]),decreasing = FALSE)
        # updateSelectInput(session,"temperature_year",choices = temperature_year_list_modified,selected = temperature_year_list_modified[1])
        # 
        # df=df_traffic %>% filter(STATE_CODE_EXP==input$ESAL_state)
        # x=sort(unique(df$YEAR))
        # updateSelectizeInput(session ,"ESAL_year",choices = x,selected = x[1])
        
        # ESAL_YEAR_SHRP_modified=sort(unique((df_traffic %>% filter(STATE_CODE_EXP==input$ESAL_state&YEAR==input$ESAL_year) %>% select(SHRP_ID))[,1]))
        # updateSelectInput(session,"ESAL_SHRP",choices = ESAL_YEAR_SHRP_modified,selected = ESAL_YEAR_SHRP_modified[1])
        
         # temperature_month_number_list_modified=sort(unique((df_temperature %>% filter(STATE_CODE_EXP==input$temperature_state&YEAR==input$temperature_year) %>% select(MONTH))[,1]),decreasing = FALSE)
         # x=temperature_month_list[temperature_month_number_list==temperature_month_number_list_modified]
         # updateSelectInput(session,"temperature_month",choices = x,selected = x[1])
    })
  
  observe({
    
    
     temperature_year_list_modified=sort(unique((df_temperature %>% filter(STATE_CODE_EXP==input$temperature_state) %>% select(YEAR))[,1]),decreasing = FALSE)
     updateSelectInput(session,"temperature_year",choices = temperature_year_list_modified,selected = temperature_year_list_modified[1])
     
  })
  
  observe({
    
    
    df=df_traffic %>% filter(STATE_CODE_EXP==input$ESAL_state)
    x=sort(unique(df$YEAR))
    updateSelectizeInput(session ,"ESAL_year",choices = x,selected = x[1])
  
  })
  
  observe({
    
    
    df=df_IRI %>% filter(STATE_CODE_EXP==input$IRI_state_1)
    x=sort(unique(df$SHRP_ID))
    updateSelectizeInput(session ,"IRI_shrp",choices = x,selected = x[1])
    
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
     output$ave_temperature <- renderPlot({
         
         df_ave_temperature_year=df_temperature %>% filter(STATE_CODE_EXP==input$temperature_state) %>% group_by(YEAR) %>% summarise(temp_ave=mean(Tave_F))
         
         min_temp_10=min(df_ave_temperature_year$temp_ave)
         max_temp_10=max(df_ave_temperature_year$temp_ave)
         
         ggplot(df_ave_temperature_year)+geom_point(aes(x=YEAR,y=temp_ave),color='red',size=5)+geom_line(aes(x=YEAR,y=temp_ave),color='blue')+
         labs(title=paste('Annual Average Temperature for',input$temperature_state,sep = ' '),
         caption=('Data from LTPP'),tag='Figure 1',x='Year',y='Temperature (F)')+theme_bw()+scale_x_continuous(breaks = seq(min(df_ave_temperature_year$YEAR),max(df_ave_temperature_year$YEAR),3))+
        
         theme(axis.text.x = element_text(face="bold", color="#993333",size=12))+theme(axis.text.y = element_text(face="bold", color="#993333",size=12))+
         theme(axis.title = element_text(size = 14,color='#121111',face='bold'))
         
     })
    # 
    # #show SHRP for all states
     output$ave_temperature_year <- renderPlot({
         
         x=sort(unique((df_temperature %>% filter(STATE_CODE_EXP==input$temperature_state,YEAR==input$temperature_year) %>% select(MONTH))[,1]),decreasing = FALSE)
         x_name=temperature_month_list[x]
         
         df_ave_temperature_year_month=df_temperature %>% filter(STATE_CODE_EXP==input$temperature_state,YEAR==input$temperature_year) %>% group_by(MONTH,SHRP_ID) %>% 
         summarise(temp_ave=mean(Tave_F)) %>% arrange(MONTH)
         df_ave=df_ave_temperature_year_month %>% group_by(MONTH) %>% summarise(T_ave=mean(temp_ave))
         ggplot(df_ave_temperature_year_month)+geom_boxplot(aes(x=MONTH,y=temp_ave,group=MONTH),fill='#2616b8',color='black')+geom_line(data=df_ave,aes(x=MONTH,y=T_ave),color='red',linetype='dashed')+
         labs(title=paste('Annual Average Temperature for',input$temperature_state,'Year :',input$temperature_year,sep = ' '),
         caption=('Data from LTPP'),tag='Figure 2',x='Month',y='Temperature (F)')+scale_x_discrete(limits=temperature_month_list)+theme_bw()+
         theme(axis.text.x = element_text(face="bold", color="#993333",size=12))+theme(axis.text.y = element_text(face="bold", color="#993333",size=12))+
         theme(axis.title = element_text(size = 14,color='#121111',face='bold')) 
         
     })
    
     output$ave_temp_shrp_month <- renderPlot({
       
       selected_month=temperature_month_number_list[temperature_month_list==input$temperature_month]  
       df=df_temperature %>% filter(STATE_CODE_EXP==input$temperature_state&YEAR==input$temperature_year&MONTH==selected_month)
       y_seq=seq(min(df$ELEVATION),max(df$ELEVATION),by=15)
       
       ggplot(data=df,aes(x=df$ELEVATION,y=df$Tave_F))+geom_point(color='red',size=5)+geom_smooth(color='blue',se=TRUE,linetype='dashed',method = 'lm')+
       labs(title=paste('Temperature versus Elevation for State:',input$temperature_state,'Year:',input$temperature_year,'Month:',input$temperature_month,sep=' '),
       caption=('Data from LTPP'),tag='Figure 3',x='Elevation (ft)',y='Temperature (F)')+theme_bw()+
           theme(axis.text.x = element_text(face="bold", color="#993333",size=12))+theme(axis.text.y = element_text(face="bold", color="#993333",size=12))+
           theme(axis.title = element_text(size = 14,color='#121111',face='bold'))
     })
    
    # # show map
     
      output$ave_temperature_location <- renderLeaflet({
          
          selected_month=temperature_month_number_list[temperature_month_list==input$temperature_month]  
          df=df_temperature %>% filter(STATE_CODE_EXP==input$temperature_state&YEAR==input$temperature_year&MONTH==selected_month)
          
          leaflet(df) %>% addTiles() %>% addMarkers(lng=df$LONGITUDE,lat=df$LATITUDE) %>% addProviderTiles('OpenTopoMap') %>% 
              addMeasure(position = 'bottomleft',primaryLengthUnit = 'feet',primaryAreaUnit = 'sqfeet')
          
      })
      
      output$temperature_table1 <- DT::renderDataTable(DT::datatable({
        df_ave_temperature_year=df_temperature %>% filter(STATE_CODE_EXP==input$temperature_state) %>% group_by(YEAR) %>% summarise(Average_Temperature=round(mean(Tave_F),1))
        df_ave_temperature_year
      }))
      
      output$temperature_table2 <- DT::renderDataTable(DT::datatable({
        x=sort(unique((df_temperature %>% filter(STATE_CODE_EXP==input$temperature_state,YEAR==input$temperature_year) %>% select(MONTH))[,1]),decreasing = FALSE)
        x_name=temperature_month_list[x]
        
        df_ave_temperature_year_month=df_temperature %>% filter(STATE_CODE_EXP==input$temperature_state,YEAR==input$temperature_year) %>% group_by(MONTH,SHRP_ID,ELEVATION) %>% 
        summarise(temp_ave=mean(Tave_F)) %>% arrange(MONTH)
        df_ave=df_ave_temperature_year_month %>% group_by(MONTH,SHRP_ID,ELEVATION) %>% summarise(Average_Temperature=round(mean(temp_ave),1))
        df_ave
        
      }))
    
    #show traffic year growth as a function of year
      
    output$ESAL_year_plot <- renderPlot({
        
        df=df_traffic %>% filter(STATE_CODE_EXP==input$ESAL_state) %>% group_by(YEAR) %>% summarise(ave_ESAL=mean(ESAL))
        ggplot(df)+geom_point(aes(x=YEAR,y=ave_ESAL),color='red',size=5)+geom_line(aes(x=YEAR,y=ave_ESAL),color='blue',linetype='dashed')+
          labs(title = paste('Annual Average Traffic for State:',input$ESAL_state,sep=' '),x='Year',y='ESAL',caption = 'Data from LTPP',tag='Figure 1')+
          theme_bw()+theme(axis.text.x = element_text(face="bold", color="#993333",size=12))+theme(axis.text.y = element_text(face="bold", color="#993333",size=12))+
          theme(axis.title = element_text(size = 14,color='#121111',face='bold'))
        
    })
    
    output$ESAL_year_SHRP <- renderPlot({
      
      df=df_traffic %>% filter(STATE_CODE_EXP==input$ESAL_state&YEAR==input$ESAL_year)
      df_ave=df_traffic %>% filter(STATE_CODE_EXP==input$ESAL_state&YEAR==input$ESAL_year) %>% group_by(SHRP_ID) %>% summarise(ave_ESAL=mean(ESAL)) 
      ggplot(df)+geom_boxplot(aes(x=YEAR,y=ESAL,group=YEAR),fill='#2616b8',color='black')+labs(title = paste('ESAL data for State :',input$ESAL_state,'Year:',input$ESAL_year),x='Year',
      y='ESAL',caption='Data from LTPP',tag='Figure 2')+
      theme_bw()+theme(axis.text.x = element_text(face="bold", color="#993333",size=12))+theme(axis.text.y = element_text(face="bold", color="#993333",size=12))+
      theme(axis.title = element_text(size = 14,color='#121111',face='bold'))
      #+scale_x_discrete(limits=input$ESAL_year)
      
    })
    
    output$ESAL_map <- renderLeaflet({
      
      df=df_traffic %>% filter(STATE_CODE_EXP==input$ESAL_state&YEAR==input$ESAL_year)
      leaflet(df) %>% addTiles() %>% addMarkers(lng=df$LONGITUDE,lat=df$LATITUDE) %>% addProviderTiles('Stamen.Terrain') %>% 
        addMeasure(position = 'bottomleft',primaryLengthUnit = 'feet',primaryAreaUnit = 'sqfeet')
      
    })
    
    output$ESAL_table <- DT::renderDataTable(DT::datatable({
      
      df=df_traffic %>% filter(STATE_CODE_EXP==input$ESAL_state&YEAR==input$ESAL_year) %>% select(SHRP_ID,LATITUDE,LONGITUDE,ELEVATION,ESAL)
      df
      
    }))
    
    output$IRI_time_plot <-renderPlot({
      
      df=df_IRI %>% filter(STATE_CODE_EXP==input$IRI_state_1&SHRP_ID==input$IRI_shrp) %>% select(Date,CONSTRUCTION_NO,IRI)
      min_date=min(df$Date)
      df$day=as.integer(df$Date-min_date)
      
      ggplot(df)+geom_point(aes(x=day,y=IRI,color=factor(df$CONSTRUCTION_NO),size=5))+
      geom_line(aes(x=day,y=IRI),color='black',linetype='dashed')+
      labs(title=paste('IRI Variation for State',input$IRI_state_1, 'SHRP id :',input$IRI_shrp,sep = ' '),
      caption=('Data from LTPP'),tag='Figure 1',x='Day after first Construction',y='IRI',color='Construction No')+theme_bw()+
      scale_x_continuous(breaks = seq(0,max(df$day),length.out=10))+
      theme(axis.text.x = element_text(face="bold", color="#993333",size=12))+theme(axis.text.y = element_text(face="bold", color="#993333",size=12))+
      theme(axis.title = element_text(size = 14,color='#121111',face='bold'))
      
    })
    
    output$IRI_temp_plot <-renderPlot({
      
      df1=df_IRI %>% filter(STATE_CODE_EXP==input$IRI_state_2) %>% select(Date,CONSTRUCTION_NO,SHRP_ID,IRI) %>% filter(CONSTRUCTION_NO==1)
      
      df1$delta_year=0
      df1$delta_IRI=0
      
      for (ishrp in unique(df1$SHRP_ID)) {
        
        min_IRI=min(df1[df1$SHRP_ID==ishrp,'IRI']$IRI)
        min_date=as.integer(format(min(df1[df1$SHRP_ID==ishrp,'VISIT_DATE']$VISIT_DATE),'%Y'))
        survey_year=as.integer(format(df1[df1$SHRP_ID==ishrp,'VISIT_DATE']$VISIT_DATE,'%Y'))
    
        df1[df1$SHRP_ID==ishrp,'delta_year']$delta_year=as.integer(survey_year-min_date)
        df1[df1$SHRP_ID==ishrp,'delta_IRI']$delta_IRI=(df1[df1$SHRP_ID==ishrp,'IRI']$IRI-min_IRI)
        
      }
      
      df1$survey_year=as.integer(format(df1$VISIT_DATE,'%Y'))
      df1=df1 %>% group_by(SHRP_ID,survey_year) %>% summarise(dIRI=max(delta_IRI),dyear=min(delta_year))
      df2=df_temperature %>% filter(STATE_CODE_EXP==input$IRI_state_2) %>% group_by(SHRP_ID,YEAR) %>% summarise(Tave=mean(Tave_F))
      
      df3=inner_join(df1,df2,by=c('SHRP_ID'='SHRP_ID','survey_year'='YEAR'))
      
      ggplot(df3)+geom_boxplot(aes(x=dyear,y=dIRI,group=dyear))+
      geom_point(aes(x=dyear,y=dIRI,color=Tave),size=1)+scale_color_gradient(low="blue", high="red")+
      
      labs(title=paste('IRI Evolution for State',input$IRI_state_2,'as a function of Annual Average Temperature',sep = ' '),
      caption=('Data from LTPP'),tag='Figure 2',x='Temperature (F)',y='IRI')+theme_bw()+
      theme(axis.text.x = element_text(face="bold", color="#993333",size=12))+theme(axis.text.y = element_text(face="bold", color="#993333",size=12))+
      theme(axis.title = element_text(size = 14,color='#121111',face='bold'))
    
    })
    
    output$IRI_traffic_plot <- renderPlot({
      
      df1=df_IRI %>% filter(STATE_CODE_EXP==input$IRI_state_2) %>% select(Date,CONSTRUCTION_NO,SHRP_ID,IRI) %>% filter(CONSTRUCTION_NO==1)
      
      df1$delta_year=0
      df1$delta_IRI=0
      
      for (ishrp in unique(df1$SHRP_ID)) {
        
        min_IRI=min(df1[df1$SHRP_ID==ishrp,'IRI']$IRI)
        min_date=as.integer(format(min(df1[df1$SHRP_ID==ishrp,'VISIT_DATE']$VISIT_DATE),'%Y'))
        survey_year=as.integer(format(df1[df1$SHRP_ID==ishrp,'VISIT_DATE']$VISIT_DATE,'%Y'))
        
        df1[df1$SHRP_ID==ishrp,'delta_year']$delta_year=as.integer(survey_year-min_date)
        df1[df1$SHRP_ID==ishrp,'delta_IRI']$delta_IRI=(df1[df1$SHRP_ID==ishrp,'IRI']$IRI-min_IRI)
        
      }
      
      df1$survey_year=as.integer(format(df1$VISIT_DATE,'%Y'))
      df1=df1 %>% group_by(SHRP_ID,survey_year) %>% summarise(dIRI=max(delta_IRI),dyear=min(delta_year))
      df2=df_traffic %>% filter(STATE_CODE_EXP==input$IRI_state_2) %>% filter(CONSTRUCTION_NO==1) %>% select(SHRP_ID,YEAR,ESAL)
      df3=inner_join(df1,df2,by=c('SHRP_ID'='SHRP_ID','survey_year'='YEAR'))
      
      ggplot(df3,aes(x=dyear,y=dIRI))+geom_boxplot(aes(group=dyear))+geom_point(aes(color=ESAL))+
      scale_color_gradient(low="blue", high="red") +
      labs(title=paste('IRI Evolution for State',input$IRI_state_2,'as a function of ESAL',sep = ' '),
      caption=('Data from LTPP'),tag='Figure 1',x='ESAL',y='IRI')+theme_bw()+
      theme(axis.text.x = element_text(face="bold", color="#993333",size=12))+theme(axis.text.y = element_text(face="bold", color="#993333",size=12))+
      theme(axis.title = element_text(size = 14,color='#121111',face='bold'))
      
    })
    
    output$IRI_Ac_thickness_plot <- renderPlot({
      
      df1=df_IRI %>% filter(STATE_CODE_EXP==input$IRI_state_2) %>% select(Date,CONSTRUCTION_NO,SHRP_ID,IRI) %>% filter(CONSTRUCTION_NO==1)
        
      df1$delta_year=0
      df1$delta_IRI=0
      
      for (ishrp in unique(df1$SHRP_ID)) {
        
        min_IRI=min(df1[df1$SHRP_ID==ishrp,'IRI']$IRI)
        min_date=as.integer(format(min(df1[df1$SHRP_ID==ishrp,'VISIT_DATE']$VISIT_DATE),'%Y'))
        survey_year=as.integer(format(df1[df1$SHRP_ID==ishrp,'VISIT_DATE']$VISIT_DATE,'%Y'))
        
        df1[df1$SHRP_ID==ishrp,'delta_year']$delta_year=as.integer(survey_year-min_date)
        df1[df1$SHRP_ID==ishrp,'delta_IRI']$delta_IRI=(df1[df1$SHRP_ID==ishrp,'IRI']$IRI-min_IRI)
        
      }
      
      df1$survey_year=as.integer(format(df1$VISIT_DATE,'%Y'))
      df1=df1 %>% group_by(SHRP_ID,survey_year) %>% summarise(dIRI=max(delta_IRI),dyear=min(delta_year))
      
      df2=df_pavement_info %>% filter(STATE_CODE_EXP==input$IRI_state_2&CONSTRUCTION_NO==1&LAYER_TYPE=='AC') %>% 
        group_by(SHRP_ID) %>% summarise(ac_th=sum(REPR_THICKNESS))
      
      df3=inner_join(df1,df2,by='SHRP_ID')
      
      ggplot(df3,aes(x=dyear,y=IRI_ac_th))+geom_boxplot(aes(group=dyear))+geom_point(aes(color=ac_th))+
      scale_color_gradient(low="blue", high="red") +
      labs(title=paste('(IRI/AC Layer Thickness) Evolution for State',input$IRI_state_2,sep = ' '),
      caption=('Data from LTPP'),tag='Figure 2',x='ESAL',y='IRI')+theme_bw()+
      theme(axis.text.x = element_text(face="bold", color="#993333",size=12))+theme(axis.text.y = element_text(face="bold", color="#993333",size=12))+
      theme(axis.title = element_text(size = 14,color='#121111',face='bold'))
      
    })
    
    output$IRI_total_thickness_plot <- renderPlot({
      
      
      df1=df_IRI %>% filter(STATE_CODE_EXP==input$IRI_state_2) %>% select(Date,CONSTRUCTION_NO,SHRP_ID,IRI) %>% filter(CONSTRUCTION_NO==1)
      
      df1$delta_year=0
      df1$delta_IRI=0
      
      for (ishrp in unique(df1$SHRP_ID)) {
        
        min_IRI=min(df1[df1$SHRP_ID==ishrp,'IRI']$IRI)
        min_date=as.integer(format(min(df1[df1$SHRP_ID==ishrp,'VISIT_DATE']$VISIT_DATE),'%Y'))
        survey_year=as.integer(format(df1[df1$SHRP_ID==ishrp,'VISIT_DATE']$VISIT_DATE,'%Y'))
        
        df1[df1$SHRP_ID==ishrp,'delta_year']$delta_year=as.integer(survey_year-min_date)
        df1[df1$SHRP_ID==ishrp,'delta_IRI']$delta_IRI=(df1[df1$SHRP_ID==ishrp,'IRI']$IRI-min_IRI)
        
      }
      
      df1$survey_year=as.integer(format(df1$VISIT_DATE,'%Y'))
      df1=df1 %>% group_by(SHRP_ID,survey_year) %>% summarise(dIRI=max(delta_IRI),dyear=min(delta_year))
      
      df2=df_pavement_info %>% filter(STATE_CODE_EXP==input$IRI_state_2&CONSTRUCTION_NO==1) %>% 
      group_by(SHRP_ID) %>% summarise(t_th=sum(REPR_THICKNESS))
      
      df3=inner_join(df1,df2,by='SHRP_ID')
      
      ggplot(df3,aes(x=dyear,y=IRI_ac_th))+geom_boxplot(aes(group=dyear))+geom_point(aes(color=t_th))+
      scale_color_gradient(low="blue", high="red") +
      labs(title=paste('(IRI/total Layer Thickness) Evolution for State',input$IRI_state_2,sep = ' '),
      caption=('Data from LTPP'),tag='Figure 3',x='ESAL',y='IRI')+theme_bw()+
      theme(axis.text.x = element_text(face="bold", color="#993333",size=12))+theme(axis.text.y = element_text(face="bold", color="#993333",size=12))+
      theme(axis.title = element_text(size = 14,color='#121111',face='bold'))
      
    })
    
    output$IRI_Tave_plot <- renderPlot({
      
      df1=df_IRI %>% filter(STATE_CODE_EXP==input$IRI_state_2) %>% select(Date,CONSTRUCTION_NO,SHRP_ID,IRI) %>% filter(CONSTRUCTION_NO==1)
      
      df1$delta_year=0
      df1$delta_IRI=0
      
      for (ishrp in unique(df1$SHRP_ID)) {
        
        min_IRI=min(df1[df1$SHRP_ID==ishrp,'IRI']$IRI)
        min_date=as.integer(format(min(df1[df1$SHRP_ID==ishrp,'VISIT_DATE']$VISIT_DATE),'%Y'))
        survey_year=as.integer(format(df1[df1$SHRP_ID==ishrp,'VISIT_DATE']$VISIT_DATE,'%Y'))
        
        df1[df1$SHRP_ID==ishrp,'delta_year']$delta_year=as.integer(survey_year-min_date)
        df1[df1$SHRP_ID==ishrp,'delta_IRI']$delta_IRI=(df1[df1$SHRP_ID==ishrp,'IRI']$IRI-min_IRI)
        
      }
      
      df1$survey_year=as.integer(format(df1$VISIT_DATE,'%Y'))
      df1=df1 %>% group_by(SHRP_ID,survey_year) %>% summarise(dIRI=max(delta_IRI),dyear=min(delta_year))
      
      df2=df_temperature %>% filter(STATE_CODE_EXP==input$IRI_state_2) %>% group_by(SHRP_ID,YEAR) %>% summarise(Tave=mean(Tave_F))
      df3=inner_join(df1,df2,by=c('SHRP_ID'='SHRP_ID','survey_year'='YEAR'))
      
      ggplot(df3,aes(x=dyear,y=dIRI))+geom_boxplot(aes(group=dyear))+geom_point(aes(color=Tave))+
      scale_color_gradient(low="blue", high="red") +
      labs(title=paste('(IRI/Average Anuual Temperature) Evolution for State',input$IRI_state_2,sep = ' '),
      caption=('Data from LTPP'),tag='Figure 4',x='ESAL',y='IRI')+theme_bw()+
      theme(axis.text.x = element_text(face="bold", color="#993333",size=12))+theme(axis.text.y = element_text(face="bold", color="#993333",size=12))+
      theme(axis.title = element_text(size = 14,color='#121111',face='bold'))
      
      
    })
    
    
    output$IRI_Tave_plot <- renderPlot({
    
    
})

    