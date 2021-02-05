library(shiny)
library(googleVis)
library(leaflet)

shinyServer(function(input, output,session){
    
    
    observe({
        
        layer_type_list_updated=unique(df_pavement_info %>% filter(STATE_CODE_EXP==input$state_section_data) %>% select(LAYER_TYPE))
        updateSelectizeInput(session,"layer_type_data",choices=layer_type_list_updated,selected = layer_type_list_updated[1])
    })
  
  observe({


     temperature_year_list_modified=sort(unique((df_temperature %>% filter(STATE_CODE_EXP==input$temperature_state) %>% select(YEAR))[,1]),decreasing = FALSE)
     updateSelectInput(session,"temperature_year",choices = temperature_year_list_modified,selected = temperature_year_list_modified[1])

  })
  # 
  observe({


    df=df_traffic %>% filter(STATE_CODE_EXP==input$ESAL_state)
    x=sort(unique(df$YEAR))
    updateSelectizeInput(session ,"ESAL_year",choices = x,selected = x[1])

  })
  # 
  observe({


    df=df_IRI %>% filter(STATE_CODE_EXP==input$IRI_state_1)
    x=sort(unique(df$SHRP_ID))
    updateSelectizeInput(session ,"IRI_shrp",choices = x,selected = x[1])

  })
  
  observe({
    
    
    df=df_cracking %>% filter(STATE==input$crack_state)
    x=sort(unique(df$SHRP))
    updateSelectizeInput(session ,"crack_shrp",choices = x,selected = x[1])
    
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
        theme_bw()+theme(axis.text.x = element_text(face="bold", color="#0f0000",size=12))+theme(axis.text.y = element_text(face="bold", color="#0f0000",size=12))+
          theme(axis.title = element_text(size = 14,color='#121111',face='bold'))+
          scale_color_gradient(low="blue", high="red")

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
        theme_bw()+theme(axis.text.x = element_text(face="bold", color="#0f0000",size=12))+theme(axis.text.y = element_text(face="bold", color="#0f0000",size=12))+
          theme(axis.title = element_text(size = 14,color='#121111',face='bold'))+
          scale_color_gradient(low="blue", high="red")

    })
    # 
    # #show map for elevation
    # 
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
        theme_bw()+theme(axis.text.x = element_text(face="bold", color="#0f0000",size=12))+theme(axis.text.y = element_text(face="bold", color="#0f0000",size=12))+
          theme(axis.title = element_text(size = 14,color='#121111',face='bold'))+
          scale_color_gradient(low="blue", high="red")

    })
    # 
    # # show map for sections in the state using googleVis
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
    
    
    output$thickness_table_stat <-DT::renderDataTable(DT::datatable({

      dftemp=df_pavement_info %>% filter(STATE_CODE_EXP==input$state_section_data& LAYER_TYPE==input$layer_type_data)
      data1=round(quantile(dftemp$REPR_THICKNESS,probs=c(0.25,0.5,0.75)),2)
      data2=round(c(min(dftemp$REPR_THICKNESS),max(dftemp$REPR_THICKNESS)),2)
      dff=data.frame('data1'=dim(dftemp)[1],'data2'=data2[1],'data3'=data1[1],'data4'=data1[2],'data5'=data1[3],'data6'=data2[2])
      colnames(dff)[1]=paste(input$layer_type_data,'Layer Data points for',input$state_section_data)
      colnames(dff)[2:6]=c('Minimum','25th','50th','75th','Maximum')
      rownames(dff)='1'
      dff

    }))
    # 
    output$life_table_stat <-DT::renderDataTable(DT::datatable({

      df=df_pavement_SHRP %>% filter(STATE_CODE_EXP==input$state_section_data)
      data1=round(quantile(df$duration,probs=c(0.25,0.5,0.75)),2)
      data2=c(round(min(df$duration),2),round(max(df$duration),2))
      dff=data.frame('data1'=dim(df)[1],'data2'=data2[1],'data3'=data1[1],'data4'=data1[2],'data5'=data1[3],'data6'=data2[2])
      colnames(dff)[1]=paste('Life Data Points for',input$state_section_data,sep=' ')
      colnames(dff)[2:6]=c('Minimum','25th','50th','75th','Maximum')
      rownames(dff)='1'
      dff
    }))
    # 
    # 
    output$elevation_table_stat <-DT::renderDataTable(DT::datatable({

      df=df_gps %>% filter(STATE_CODE_EXP==input$state_section_data)
      data1=round(quantile(df$ELEVATION,probs=c(0.25,0.5,0.75)),2)
      data2=round(c(min(df$ELEVATION),max(df$ELEVATION)),2)
      dff=data.frame('data1'=dim(df)[1],'data2'=data2[1],'data3'=data1[1],'data4'=data1[2],'data5'=data1[3],'data6'=data2[2])
      colnames(dff)[1]=paste('Elevation Data Points for',input$state_section_data,sep=' ')
      colnames(dff)[2:6]=c('Minimum','25th','50th','75th','Maximum')
      rownames(dff)='1'
      dff

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
    # 
    # # show ave temp
     output$ave_temperature <- renderPlot({

         df_ave_temperature_year=df_temperature %>% filter(STATE_CODE_EXP==input$temperature_state) %>% group_by(YEAR) %>% summarise(temp_ave=mean(Tave_F))

         min_temp_10=min(df_ave_temperature_year$temp_ave)
         max_temp_10=max(df_ave_temperature_year$temp_ave)

         ggplot(df_ave_temperature_year)+geom_point(aes(x=YEAR,y=temp_ave),color='red',size=5)+geom_line(aes(x=YEAR,y=temp_ave),color='blue')+
         labs(title=paste('Annual Average Temperature for',input$temperature_state,sep = ' '),
         caption=('Data from LTPP'),tag='Figure 1',x='Year',y='Temperature (F)')+theme_bw()+scale_x_continuous(breaks = seq(min(df_ave_temperature_year$YEAR),max(df_ave_temperature_year$YEAR),3))+

           theme(axis.text.x = element_text(face="bold", color="#0f0000",size=12))+theme(axis.text.y = element_text(face="bold", color="#0f0000",size=12))+
           theme(axis.title = element_text(size = 14,color='#121111',face='bold'))+
           scale_color_gradient(low="blue", high="red")

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
           theme(axis.text.x = element_text(face="bold", color="#0f0000",size=12))+theme(axis.text.y = element_text(face="bold", color="#0f0000",size=12))+
           theme(axis.title = element_text(size = 14,color='#121111',face='bold'))+
           scale_color_gradient(low="blue", high="red")

     })
     # 
     output$ave_temp_shrp_month <- renderPlot({

       selected_month=temperature_month_number_list[temperature_month_list==input$temperature_month]
       df=df_temperature %>% filter(STATE_CODE_EXP==input$temperature_state&YEAR==input$temperature_year&MONTH==selected_month)
       y_seq=seq(min(df$ELEVATION),max(df$ELEVATION),by=15)

       ggplot(data=df,aes(x=ELEVATION,y=Tave_F))+geom_point(color='red',size=5)+geom_smooth(color='blue',se=TRUE,linetype='dashed',method = 'lm')+
       labs(title=paste('Temperature versus Elevation for State:',input$temperature_state,'Year:',input$temperature_year,'Month:',input$temperature_month,sep=' '),
       caption=('Data from LTPP'),tag='Figure 3',x='Elevation (ft)',y='Temperature (F)')+theme_bw()+
         theme(axis.text.x = element_text(face="bold", color="#0f0000",size=12))+theme(axis.text.y = element_text(face="bold", color="#0f0000",size=12))+
         theme(axis.title = element_text(size = 14,color='#121111',face='bold'))
     })
    
    # # show map
     
      output$ave_temperature_location <- renderLeaflet({

          selected_month=temperature_month_number_list[temperature_month_list==input$temperature_month]
          df=df_temperature %>% filter(STATE_CODE_EXP==input$temperature_state&YEAR==input$temperature_year&MONTH==selected_month)

          leaflet(df) %>% addTiles() %>% addMarkers(lng=df$LONGITUDE,lat=df$LATITUDE) %>% addProviderTiles('OpenTopoMap') %>%
              addMeasure(position = 'bottomleft',primaryLengthUnit = 'feet',primaryAreaUnit = 'sqfeet')

      })
      # 
      output$temperature_table1 <- DT::renderDataTable(DT::datatable({
        df_ave_temperature_year=df_temperature %>% filter(STATE_CODE_EXP==input$temperature_state) %>% group_by(YEAR) %>% summarise(AT=round(mean(Tave_F),1))
        df_ave_temperature_year$Temperature=round(df_ave_temperature_year$AT,2)
        
        df_ave_temperature_year=df_ave_temperature_year %>% select(YEAR,Temperature)
        df_ave_temperature_year
      }))
      # 
      output$temperature_table2 <- DT::renderDataTable(DT::datatable({
        
        x=sort(unique((df_temperature %>% filter(STATE_CODE_EXP==input$temperature_state,YEAR==input$temperature_year) %>% select(MONTH))[,1]),decreasing = FALSE)
        x_name=temperature_month_list[x]

        df_ave_temperature_year_month=df_temperature %>% filter(STATE_CODE_EXP==input$temperature_state,YEAR==input$temperature_year) %>% group_by(MONTH,SHRP_ID,ELEVATION) %>%
        summarise(temp_ave=mean(Tave_F)) %>% arrange(MONTH)
        df_ave=df_ave_temperature_year_month %>% group_by(MONTH,SHRP_ID,ELEVATION) %>% summarise(Temperature=round(mean(temp_ave),1))
        df_ave$ELEVATION=round(df_ave$ELEVATION,2)
        df_ave=df_ave %>% select(MONTH,SHRP=SHRP_ID,ELEVATION,Temperature)
        df_ave
        
      }))
      
      
      
      output$temperature_data_table <- DT::renderDataTable(DT::datatable({
        
        df=df_temperature %>% filter(STATE_CODE_EXP==input$temperature_state)
        #year_vector=sort(unique(df$YEAR))
        
        dff=df %>%  group_by(YEAR,MONTH) %>% summarise(min_temp=round(min(Tave_F),1),temp_25=round(quantile(Tave_F,0.25),1),
                temp_50=round(quantile(Tave_F,0.5),1),temp_75=round(quantile(Tave_F,0.75),1),max_temp=round(max(Tave_F),1))
        colnames(dff)=c('Year','Month','Min Temp(F)','25th','50th','75th','Maximum Temp (F)')
        
        dff
        
        
      }))
    
    #show traffic year growth as a function of year
      
    output$ESAL_year_plot <- renderPlot({

        df=df_traffic %>% filter(STATE_CODE_EXP==input$ESAL_state) %>% group_by(YEAR) %>% summarise(ave_ESAL=mean(ESAL))
        ggplot(df)+geom_point(aes(x=YEAR,y=ave_ESAL),color='red',size=5)+geom_line(aes(x=YEAR,y=ave_ESAL),color='blue',linetype='dashed')+
          labs(title = paste('Annual Average Traffic for State:',input$ESAL_state,sep=' '),x='Year',y='ESAL',caption = 'Data from LTPP',tag='Figure 1')+
          theme_bw()+
          theme(axis.text.x = element_text(face="bold", color="#0f0000",size=12))+theme(axis.text.y = element_text(face="bold", color="#0f0000",size=12))+
          theme(axis.title = element_text(size = 14,color='#121111',face='bold'))

    })
    # 
    output$ESAL_year_SHRP <- renderPlot({

      df=df_traffic %>% filter(STATE_CODE_EXP==input$ESAL_state&YEAR==input$ESAL_year)
      df_ave=df_traffic %>% filter(STATE_CODE_EXP==input$ESAL_state&YEAR==input$ESAL_year) %>% group_by(SHRP_ID) %>% summarise(ave_ESAL=mean(ESAL))
      ggplot(df)+geom_boxplot(aes(x=YEAR,y=ESAL,group=YEAR),fill='#2616b8',color='black')+labs(title = paste('ESAL data for State :',input$ESAL_state,'Year:',input$ESAL_year),x='Year',
      y='ESAL',caption='Data from LTPP',tag='Figure 2')+
      theme_bw()+theme(axis.text.x = element_text(face="bold", color="#0f0000",size=12))+theme(axis.text.y = element_text(face="bold", color="#0f0000",size=12))+
      theme(axis.title = element_text(size = 14,color='#121111',face='bold'))
      #+scale_x_discrete(limits=input$ESAL_year)

    })
    # 
    output$ESAL_map <- renderLeaflet({

      df=df_traffic %>% filter(STATE_CODE_EXP==input$ESAL_state&YEAR==input$ESAL_year)
      leaflet(df) %>% addTiles() %>% addMarkers(lng=df$LONGITUDE,lat=df$LATITUDE) %>% addProviderTiles('Stamen.Terrain') %>%
        addMeasure(position = 'bottomleft',primaryLengthUnit = 'feet',primaryAreaUnit = 'sqfeet')

    })
    # 
    output$ESAL_table <- DT::renderDataTable(DT::datatable({

      df=df_traffic %>% filter(STATE_CODE_EXP==input$ESAL_state&YEAR==input$ESAL_year) %>% select(SHRP_ID,LATITUDE,LONGITUDE,ELEVATION,ESAL)
      df$LATITUDE=round(df$LATITUDE,2)
      df$LONGITUDE=round(df$LONGITUDE,2)
      df$ELEVATION=round(df$ELEVATION,2)
      df=df %>% select(SHRP=SHRP_ID,LATITUDE,LONGITUDE,ELEVATION,ESAL)

    }))
    
    
    output$traffic_data_table <- DT::renderDataTable(DT::datatable({
      
      df=df=df_traffic %>% filter(STATE_CODE_EXP==input$ESAL_state) %>% group_by(YEAR) %>% summarise(min_temp=round(min(ESAL),0),temp_25=round(quantile(ESAL,0.25),0),
          temp_50=round(quantile(ESAL,0.5),0),temp_75=round(quantile(ESAL,0.75),0),max_temp=round(max(ESAL),0))
      colnames(df)=c('Year','Min ESAL','25th','50th','75th','Maximum ESAL')
      df
      
    }))
    

    output$IRI_time_plot <-renderPlot({

      df=df_IRI %>% filter(STATE_CODE_EXP==input$IRI_state_1&SHRP_ID==input$IRI_shrp) %>% select(Date,CONSTRUCTION_NO,IRI)
      min_date=min(df$Date)
      df$day=as.integer(df$Date-min_date)
      
      ggplot(df)+geom_point(aes(x=day,y=IRI,color=factor(df$CONSTRUCTION_NO)),size=5)+
      geom_line(aes(x=day,y=IRI),color='black',linetype='dashed')+
      labs(title=paste('IRI Variation for State',input$IRI_state_1, 'SHRP id :',input$IRI_shrp,sep = ' '),
      caption=('Data from LTPP'),tag='Figure 1',x='Day after first Construction',y='IRI',color='Construction No')+theme_bw()+
      theme(axis.text.x = element_text(face="bold", color="#0f0000",size=12))+theme(axis.text.y = element_text(face="bold", color="#0f0000",size=12))+
      theme(axis.title = element_text(size = 20,color='#121111',face='bold'))

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
      geom_point(aes(x=dyear,y=dIRI,color=Tave),size=3)+scale_color_gradient(low="blue", high="red")+

      labs(title=paste('IRI Evolution for',input$IRI_state_2,sep = ' '),
      caption=('Data from LTPP'),tag='Figure 2',x='Year after Construction',y='IRI(t)-Minimum IRI')+theme_bw()+
      theme(axis.text.x = element_text(face="bold", color="#0f0000",size=12))+theme(axis.text.y = element_text(face="bold", color="#0f0000",size=12))+
      theme(axis.title = element_text(size = 20,color='#121111',face='bold'))

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

      ggplot(df3,aes(x=dyear,y=dIRI))+geom_boxplot(aes(group=dyear))+geom_point(aes(color=ESAL),size=3)+
      scale_color_gradient(low="blue", high="red") +
      labs(title=paste('IRI Evolution for ',input$IRI_state_2,sep = ' '),
      caption=('Data from LTPP'),tag='Figure 3',x='Year after Construction',y='IRI(t)-Minimum IRI')+theme_bw()+
      theme(axis.text.x = element_text(face="bold", color="#0f0000",size=12))+theme(axis.text.y = element_text(face="bold", color="#0f0000",size=12))+
      theme(axis.title = element_text(size = 20,color='#121111',face='bold'))

    })
    # 
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
        group_by(SHRP_ID) %>% summarise(AC_Thickness=sum(REPR_THICKNESS))

      df3=inner_join(df1,df2,by='SHRP_ID')
      df3$dIRI_ac=df3$dIRI/df3$AC_Thickness
      
      ggplot(df3,aes(x=dyear,y=dIRI_ac))+geom_boxplot(aes(group=dyear))+geom_point(aes(color=AC_Thickness),size=3)+
      scale_color_gradient(low="blue", high="red") +
      labs(title=paste('(IRI/AC Layer Thickness) for ',input$IRI_state_2,sep = ' '),
      caption=('Data from LTPP'),tag='Figure 4',x='Year after Construction',y='(IRI(t)-Minimum IRI)/Ac Layer Thickness')+theme_bw()+
      theme(axis.text.x = element_text(face="bold", color="#0f0000",size=12))+theme(axis.text.y = element_text(face="bold", color="#0f0000",size=12))+
      theme(axis.title = element_text(size = 20,color='#121111',face='bold'))

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

      df3=df_pavement_info %>% filter(STATE_CODE_EXP==input$IRI_state_2&CONSTRUCTION_NO==1&LAYER_TYPE=='AC') %>%
        group_by(SHRP_ID) %>% summarise(ac_th=sum(REPR_THICKNESS))
      
      df4=inner_join(df1,df3,by=c('SHRP_ID'='SHRP_ID'))
      df5=inner_join(df4,df2,by=c('SHRP_ID'='SHRP_ID','survey_year'='YEAR'))
      df5$dIRI_ac=df5$dIRI/df5$ac_th

      ggplot(df5,aes(x=dyear,y=dIRI_ac))+geom_boxplot(aes(group=dyear))+geom_point(aes(color=Tave),size=3)+
      scale_color_gradient(low="blue", high="red") +
      labs(title=paste('(IRI/AC Layer Thickness) for',input$IRI_state_2,sep = ' '),
      caption=('Data from LTPP'),tag='Figure 5',x='Year after Construction',y='(IRI(t)-Minimum IRI)/Ac Layer Thickness')+theme_bw()+
      theme(axis.text.x = element_text(face="bold", color="#0f0000",size=12))+theme(axis.text.y = element_text(face="bold", color="#0f0000",size=12))+
      theme(axis.title = element_text(size = 20,color='#121111',face='bold'))


    })
    # 
    # 
     output$IRI_th_traffic_plot <- renderPlot({
     
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
       df3=df_pavement_info %>% filter(STATE_CODE_EXP==input$IRI_state_2&CONSTRUCTION_NO==1&LAYER_TYPE=='AC') %>%
         group_by(SHRP_ID) %>% summarise(ac_th=sum(REPR_THICKNESS))
       
       df4=inner_join(df1,df3,by=c('SHRP_ID'='SHRP_ID'))
       df5=inner_join(df4,df2,by=c('SHRP_ID'='SHRP_ID','survey_year'='YEAR'))
       df5$dIRI_ac=df5$dIRI/df5$ac_th
       
       ggplot(df5,aes(x=dyear,y=dIRI_ac))+geom_boxplot(aes(group=dyear))+geom_point(aes(color=ESAL),size=3)+
       scale_color_gradient(low="blue", high="red") +
       labs(title=paste('(IRI/AC Layer Thickness) for',input$IRI_state_2,sep = ' '),
       caption=('Data from LTPP'),tag='Figure 6',x='Year after Construction',y='(IRI(t)-Minimum IRI)/Ac Layer Thickness')+theme_bw()+
       theme(axis.text.x = element_text(face="bold", color="#0f0000",size=12))+theme(axis.text.y = element_text(face="bold", color="#0f0000",size=12))+
       theme(axis.title = element_text(size = 20,color='#121111',face='bold'))
       
     
     })
     
     
     output$crack_shrp_temp_plot <- renderPlot({
       
       df=df_cracking %>% filter(STATE==input$crack_state,SHRP==input$crack_shrp) %>% arrange(Date)
       df$day=as.integer(df$Date-df$Date[1])
       
       ggplot(df,aes(x=day,y=F))+geom_point(size=5,color='red')+geom_line(color='blue',linetype='dashed')+
       labs(title = paste('Fatigue Cracking for',input$crack_state,sep=' '),
       caption='Data from LTPP',tag='Figure 1',x='Day after Construction',y='Fatigue cracking Area (ft2)')+theme_bw()+
       theme(axis.text.x = element_text(face="bold", color="#0f0000",size=12))+theme(axis.text.y = element_text(face="bold", color="#0f0000",size=12))+
       theme(axis.title = element_text(size = 20,color='#121111',face='bold'))
       
     })
     
     output$crack_ac_traffic_plot <- renderPlot({
       
       df=df_cracking %>% filter(STATE==input$crack_state)
       df$dyear=0
       
       for (ishrp in unique(df$SHRP)) {
         
         data=df %>% filter(SHRP==ishrp) %>% arrange(year)
         df[df$SHRP==ishrp,'dyear']=as.integer(data$year-data$year[1])
        
       }
       
       df2=df_pavement_info %>% filter(STATE_CODE_EXP==input$crack_state&CONSTRUCTION_NO==1&LAYER_TYPE=='AC') %>%
         group_by(SHRP_ID) %>% summarise(AC_Thickness=sum(REPR_THICKNESS))
       
       df3=inner_join(df,df2,by=c('SHRP'='SHRP_ID'))
       df3$F_th=df3$F/df3$AC_Thickness
       
       df4=df_traffic %>% filter(STATE_CODE_EXP==input$crack_state) %>% filter(CONSTRUCTION_NO==1) %>% select(SHRP_ID,YEAR,ESAL)
       df5=inner_join(df3,df4,by=c('SHRP'='SHRP_ID','year'='YEAR'))
       
       ggplot(df5,aes(x=dyear,y=F_th))+geom_boxplot(aes(group=dyear))+
       geom_point(aes(color=AC_Thickness),size=3)+
       labs(title = paste('Fatigue Cracking for',input$crack_state,sep=' '),
       caption='Data from LTPP',tag='Figure 2',x='Year after Construction',y='Fatigue cracking Area/Ac Layer Thickness')+theme_bw()+
       theme(axis.text.x = element_text(face="bold", color="#0f0000",size=12))+theme(axis.text.y = element_text(face="bold", color="#0f0000",size=12))+
       theme(axis.title = element_text(size = 20,color='#121111',face='bold'))+
       scale_color_gradient(low="blue", high="red")
       
     })
     
     
     output$crack_ac_thermal_plot <- renderPlot({
       
       df=df_cracking %>% filter(STATE==input$crack_state)
       df$dyear=0
       
       for (ishrp in unique(df$SHRP)) {
         
         data=df %>% filter(SHRP==ishrp) %>% arrange(year)
         df[df$SHRP==ishrp,'dyear']=as.integer(data$year-data$year[1])
         
       }
       
       df2=df_pavement_info %>% filter(STATE_CODE_EXP==input$crack_state&CONSTRUCTION_NO==1&LAYER_TYPE=='AC') %>%
         group_by(SHRP_ID) %>% summarise(AC_Thickness=sum(REPR_THICKNESS))
       
       df3=inner_join(df,df2,by=c('SHRP'='SHRP_ID'))
       df3$F_th=df3$F/df3$AC_Thickness
       
       df4=df_temperature %>% filter(STATE_CODE_EXP==input$crack_state) %>% group_by(SHRP_ID,YEAR) %>% summarise(Tave=mean(Tave_F))
       
       df5=inner_join(df3,df4,by=c('SHRP'='SHRP_ID','year'='YEAR'))
       
       ggplot(df5,aes(x=dyear,y=F_th))+geom_boxplot(aes(group=dyear))+
         geom_point(aes(color=Tave),size=3)+
         labs(title = paste('Fatigue Cracking for',input$crack_state,sep=' '),
              caption='Data from LTPP',tag='Figure 2',x='Year after Construction',y='Fatigue cracking Area/Ac Layer Thickness')+theme_bw()+
         theme(axis.text.x = element_text(face="bold", color="#0f0000",size=12))+theme(axis.text.y = element_text(face="bold", color="#0f0000",size=12))+
         theme(axis.title = element_text(size = 20,color='#121111',face='bold'))+
         scale_color_gradient(low="blue", high="red")
       
     })
     
     output$crack_table1 <- DT::renderDataTable(DT::datatable({
       
       df=df_cracking %>% filter(STATE==input$crack_state) %>% select(STATE,SHRP,Date,Fatigue=F)
       #df$Fatifue=
       df
       
     }))
     
     output$crack_table2 <- DT::renderDataTable(DT::datatable({
       
       df=df_cracking %>% filter(STATE==input$crack_state)
       df$dyear=0
       
       for (ishrp in unique(df$SHRP)) {
         
         data=df %>% filter(SHRP==ishrp) %>% arrange(year)
         df[df$SHRP==ishrp,'dyear']=as.integer(data$year-data$year[1])
         
       }
       
       df2=df_pavement_info %>% filter(STATE_CODE_EXP==input$crack_state&CONSTRUCTION_NO==1&LAYER_TYPE=='AC') %>%
         group_by(SHRP_ID) %>% summarise(AC_Thickness=sum(REPR_THICKNESS))
       
       df3=inner_join(df,df2,by=c('SHRP'='SHRP_ID'))
       df3$F_th=df3$F/df3$AC_Thickness
       
       df4=df_temperature %>% filter(STATE_CODE_EXP==input$crack_state) %>% group_by(SHRP_ID,YEAR) %>% summarise(Tave=mean(Tave_F))
       df5=df_traffic %>% filter(STATE_CODE_EXP==input$crack_state) %>% filter(CONSTRUCTION_NO==1) %>% select(SHRP_ID,YEAR,ESAL)
       
       df6=inner_join(df3,df4,by=c('SHRP'='SHRP_ID','year'='YEAR'))
       df7=inner_join(df6,df5,by=c('SHRP'='SHRP_ID','year'='YEAR'))
       #df7$AC_Thickness=round(df7$AC_Thickness,2)
       df7$Fatigue_Thickness=round(df7$F_th,2)
       df7$Temperature=round(df7$Tave,1)
       
       df7=df7 %>% select(STATE,SHRP,Date,Fatigue=round(F,1),Year=year,
                          Fatigue_Thickness,Temperature)
       df7

     }))
     
    
})

    