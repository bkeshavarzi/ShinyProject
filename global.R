library(tidyverse)
library(googleVis)
setwd("/Users/behroozkeshavarzi/Documents/RProject/ShinyProj/")
rm(list=ls())

df_pavement_SHRP=read.csv("PavementStructure_SHRP_Info.csv",stringsAsFactors = FALSE) 
df_pavement_info=read.csv("PavementStructure_Pavement_Info.csv") 
Out_of_US=c("Alberta","British Columbia","New Brunswick","Newfoundland","Nova Scotia","Ontario","Prince Edward Island","Quebec","Saskatchewan","Manitoba")

df_pavement_info=df_pavement_info %>% filter(!(STATE_CODE_EXP %in% Out_of_US))%>%select(STATE_CODE_EXP,SHRP_ID,CONSTRUCTION_NO,LAYER_NO,LAYER_TYPE,LAYER_TYPE_EXP,REPR_THICKNESS)
df_pavement_SHRP=df_pavement_SHRP %>% filter(!(STATE_CODE_EXP %in% Out_of_US))%>% select(STATE_CODE_EXP,STATE_CODE,SHRP_ID,START_DATE,END_DATE) 

df_pavement_SHRP$START_DATE=as.Date(df_pavement_SHRP$START_DATE,"%m/%d/%Y")
df_pavement_SHRP$END_DATE=as.Date(df_pavement_SHRP$END_DATE,"%m/%d/%Y")
index=substring(df_pavement_SHRP$END_DATE,7,10)=="2050"
df_pavement_SHRP$END_DATE=str_replace(df_pavement_SHRP$END_DATE,"2050","2005")

df_gps=read.csv('GPS.csv',stringsAsFactors = FALSE)[,c(1,2,3,4,5,9)]
df_gps=df_gps %>% filter(!(STATE_CODE_EXP %in% Out_of_US))
df_gps$ELEVATION=round(df_gps$ELEVATION,digits=2)
df_gps=df_gps %>% mutate(tip=paste("SHRP Id is :",SHRP_ID,"\n","Elevation is:",ELEVATION,sep = " "))

#Find AC pavemetns
df_ac=df_pavement_info %>% filter(LAYER_TYPE=="AC") %>% group_by(STATE_CODE_EXP,SHRP_ID) %>% summarise(n_=n()) %>% group_by(STATE_CODE_EXP) %>% summarise(number=n())
colnames(df_ac)=c("States","Number")

#Find PC pavemetns
df_pc=df_pavement_info %>% filter(LAYER_TYPE=="PC") %>% group_by(STATE_CODE_EXP,SHRP_ID) %>% summarise(n_=n()) %>% group_by(STATE_CODE_EXP) %>% summarise(number=n())
colnames(df_pc)=c("States","Number")

#geo_ac=gvisGeoChart(df_ac, "States", "Number",options=list(title="Number of Asphalt pavements",region="US", displayMode="regions", resolution="provinces",width=450, height=300,colorAxis="{values:[1, 179],colors:[\'blue',\'red']}"))
#plot(geo_ac)
#geo_pc=gvisGeoChart(df_pc, "States", "Number",options=list(title="Number of Concrete pavements",region="US", displayMode="regions", resolution="provinces",width=450, height=300,colorAxis="{values:[1, 100],colors:[\'blue',\'red']}"))
#plot(geo_pc)

#Temperature data

df_mera_shrp=read.csv("MERA_SHRP.csv",stringsAsFactors = FALSE)
df_mera_temp_month=read.csv("MERRA_TEMP_MONTH.csv",stringsAsFactors = FALSE)
df_mera_temp_month=df_mera_temp_month %>% select(!FREEZE_INDEX & !FREEZE_THAW)
df_mera_temp_month=df_mera_temp_month %>% mutate(Tave=1.8*TEMP_AVG+32)
df_mera_temp_month=df_mera_temp_month %>% select(!TEMP_AVG & !TEMP_MEAN_AVG)
colnames(df_mera_temp_month)[4]="Average Teperature"

#year=2001 #input from slider
#month=3 #input from slider

df_temperature=inner_join(df_mera_shrp,df_gps,by.x=c("STATE_CODE","SHRP_ID"),by.y=c("STATE_CODE","SHRP_ID"))
df_temperature=df_temperature %>% mutate(tip=paste("SHRP Id is:",as.character(SHRP_ID),"Elevation is:",as.character(ELEVATION),sep = " "))
df_temperature=inner_join(df_temperature,df_mera_temp_month,by="MERRA_ID")
df_temperature=df_temperature %>% select(!MERRA_ID & !STATE_CODE & !SHRP_ID  & !ELEVATION)
temperature_month_vector=unique(df_temperature$MONTH)
temperature_year_vector=unique(df_temperature$YEAR)


#geo_temperature=gvisGeoChart(df_mera_temp_month,"LatLong",colorvar ='Average Teperature',sizevar='ELEVATION',
                             #options=list(region="US",colorAxis="{values:[6, 78],colors:[\'blue',\'red']}"))
#plot(geo_temperature)

#Traffic data

df_traffic=read.csv("Traffic.csv",stringsAsFactors = FALSE)[,seq(1,6)]
df_traffic=df_traffic %>% filter(!(STATE_CODE_EXP %in% Out_of_US))
df_traffic=inner_join(df_traffic,df_gps,by.x=c("STATE_CODE","SHRP_ID"),by.y=c("STATE_CODE","SHRP_ID"))
df_traffic=df_traffic %>% select(!STATE_CODE) %>% filter(CONSTRUCTION_NO==1)
traffic_year_vector=unique(df_traffic$YEAR)


SHRP_STATE=unique(df_traffic$SHRP_ID)
#year=2001 #input from dropdown menu
df_traffic_year=inner_join(df_traffic %>% filter(YEAR==year),df_gps,by.x=c("STATE_CODE","SHRP_ID"),by.y=c("STATE_CODE","SHRP_ID"))
colnames(df_traffic_year)[6]="ESAL"
colnames(df_traffic_year)[7]="Lat"
colnames(df_traffic_year)[8]="Long"
df_traffic_year=df_traffic_year %>% filter(!(STATE_CODE_EXP %in%Out_of_US))
df_traffic_year=df_traffic_year %>% select(STATE_CODE_EXP,Lat,Long,ELEVATION,ESAL,YEAR)
df_traffic_year=df_traffic_year %>% mutate(LatLong=paste(Lat,Long,sep=":"))
#geo_traffic=gvisGeoChart(df_traffic_year, "LatLong", colorvar='ESAL',sizevar='ELEVATION',
                         #options=list(region="US",resolution="provinces",colorAxis="{values:[0, 4000000],colors:[\'blue',\'red']}"))
#plot(geo_traffic)

#Subgrade

df_subgrade=read.csv("Subgrade.csv",stringsAsFactors = FALSE)%>% select(LINKED_SHRP_ID,STATE_CODE,SHRP_ID,SOIL_MOD_CORRECTED)
df_subgrade=inner_join(df_subgrade,df_gps,by.x=c("STATE_CODE","SHRP_ID"),by.y=c("STATE_CODE","SHRP_ID")) %>% select(STATE_CODE_EXP,SHRP_ID,LATITUDE,LONGITUDE,ELEVATION,SOIL_MOD_CORRECTED) %>% mutate(LatLong=paste(LATITUDE,LONGITUDE,sep = ":"))
df_subgrade=df_subgrade %>% select(STATE_CODE_EXP,SHRP_ID,ELEVATION,LatLong,SOIL_MOD_CORRECTED)
geo_subgrade=gvisGeoChart(df_subgrade, "LatLong", colorvar='SOIL_MOD_CORRECTED',sizevar='ELEVATION',
                          options=list(region="US",colors="['#0713f2', '#ed1109']"))
plot(geo_subgrade)

#IRI
df_IRI=read.csv("IRI.csv",stringsAsFactors = FALSE)
df_IRI=df_IRI %>% filter(!(STATE_CODE_EXP %in%Out_of_US))
df_IRI=df_IRI %>% filter(!(STATE_CODE_EXP %in%Out_of_US)) %>% select(STATE_CODE,STATE_CODE_EXP,SHRP_ID,VISIT_DATE,IRI_LEFT_WHEEL_PATH,IRI_RIGHT_WHEEL_PATH,CONSTRUCTION_NO)
df_IRI=df_IRI %>% mutate(IRI_ave=ave(IRI_LEFT_WHEEL_PATH,IRI_RIGHT_WHEEL_PATH))
df_IRI$VISIT_DATE=as.Date(df_IRI$VISIT_DATE,format ="%m/%d/%Y")
df_IRI=df_IRI %>% group_by(STATE_CODE,SHRP_ID) %>% arrange(VISIT_DATE,CONSTRUCTION_NO)

df_pavement_info_ac_only=df_pavement_info %>% filter(LAYER_TYPE=="AC") %>% group_by(STATE_CODE_EXP,SHRP_ID,CONSTRUCTION_NO) %>% summarise(ac_th=sum(REPR_THICKNESS))

###Structure

state_name=unique(df_pavement_info$STATE_CODE_EXP)
layer_type=unique(df_pavement_info$LAYER_TYPE_EXP)
construction_no=unique(df_pavement_info$CONSTRUCTION_NO)

###plotting histogram for thickness
df_structure=df_pavement_info %>% filter(STATE_CODE_EXP=="Texas") %>% group_by(SHRP_ID,LAYER_TYPE,CONSTRUCTION_NO) %>%
summarise(Thickness=sum(REPR_THICKNESS))
#geo_plot_structure_ac=gvisHistogram(df_structure_ac,options=list(colors="['#f5160a']"))

#df_structure_pc=df_pavement_info %>% filter(STATE_CODE_EXP=="Texas" & CONSTRUCTION_NO==1 &LAYER_TYPE=="PC") %>% group_by(SHRP_ID) %>% summarise(Thickness=sum(REPR_THICKNESS))
#geo_plot_structure_pc=gvisHistogram(df_structure_pc,options=list(colors="['#a3a8a4']"))

traffic_state_unique=unique(df_traffic$STATE_CODE_EXP)
df_traffic_ac=df_traffic %>% filter(STATE_CODE_EXP=="Texas" & CONSTRUCTION_NO==1 & YEAR==2000) %>% group_by(SHRP_ID) %>% select(SHRP_ID,ANNUAL_ESAL_TREND)
df_traffic_ac=inner_join(df_traffic_ac,df_structure_ac,by="SHRP_ID") 
df_traffic_ac=df_traffic_ac[,c(2,3)]
geo_plot_traffic_ac=gvisScatterChart(df_traffic_ac,options = list(title="Aspahlt Thickness Vs. Traffic",vAxis="{title:'Aspahlt Thickness'}",hAxis="{title:'Traffic'}",width=400))

df_traffic_pc=df_traffic %>% filter(STATE_CODE_EXP=="Texas" & CONSTRUCTION_NO==1 & YEAR==2000) %>% group_by(SHRP_ID) %>% select(SHRP_ID,ANNUAL_ESAL_TREND)
df_traffic_pc=inner_join(df_traffic_pc,df_structure_pc,by="SHRP_ID") 
df_traffic_pc=df_traffic_pc[,c(2,3)]
geo_plot_traffic_pc=gvisScatterChart(df_traffic_pc,options = list(title="Concrete Thickness Vs. Traffic",vAxis="{title:'Concrete Thickness'}",hAxis="{title:'Traffic'}",width=400))


df_IRI_state=df_IRI %>% filter(STATE_CODE_EXP=="Texas") %>% select(SHRP_ID,VISIT_DATE,IRI_ave) %>% group_by(SHRP_ID,VISIT_DATE) %>% mutate(IRI_final=max(IRI_ave)) %>% select(SHRP_ID,VISIT_DATE,IRI_final) %>% group_by(SHRP_ID,VISIT_DATE) %>% summarise(IRI=max(IRI_final))
df_IRI_state=df_IRI_state %>% filter(!is.na(IRI))



