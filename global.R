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

#Temperature data

df_mera_shrp=read.csv("MERA_SHRP.csv",stringsAsFactors = FALSE)
df_mera_temp_month=read.csv("MERRA_TEMP_MONTH.csv",stringsAsFactors = FALSE)
df_mera_temp_month=df_mera_temp_month %>% select(!FREEZE_INDEX & !FREEZE_THAW)
df_mera_temp_month=df_mera_temp_month %>% mutate(Tave=1.8*TEMP_AVG+32)
df_mera_temp_month=df_mera_temp_month %>% select(!TEMP_AVG & !TEMP_MEAN_AVG)
colnames(df_mera_temp_month)[4]="Average Teperature"

df_temperature=inner_join(df_mera_shrp,df_gps,by.x=c("STATE_CODE","SHRP_ID"),by.y=c("STATE_CODE","SHRP_ID"))
df_temperature=df_temperature %>% mutate(tip=paste("SHRP Id is:",as.character(SHRP_ID),"Elevation is:",as.character(ELEVATION),sep = " "))
df_temperature=inner_join(df_temperature,df_mera_temp_month,by="MERRA_ID")
df_temperature=df_temperature %>% select(!MERRA_ID & !STATE_CODE & !SHRP_ID  & !ELEVATION)
df_temperature=df_temperature %>% mutate(LatLong=paste(LATITUDE,LONGITUDE,sep=":"))
df_temperature=df_temperature %>% select(!LATITUDE & !LONGITUDE)

temperature_month_vector=unique(df_temperature$MONTH)
temperature_year_vector=unique(df_temperature$YEAR)

#Traffic data

df_traffic=read.csv("Traffic.csv",stringsAsFactors = FALSE)[,seq(1,6)]
df_traffic=df_traffic %>% filter(!(STATE_CODE_EXP %in% Out_of_US))
df_traffic=inner_join(df_traffic,df_gps,by.x=c("STATE_CODE","SHRP_ID"),by.y=c("STATE_CODE","SHRP_ID"))
df_traffic=df_traffic %>% mutate(LatLong=paste(LATITUDE,LONGITUDE,sep=":"))
colnames(df_traffic)[6]="ESAL"
traffic_year_vector=unique(df_traffic$YEAR)
SHRP_STATE=unique(df_traffic$SHRP_ID)

#IRI

df_IRI=read.csv("IRI.csv",stringsAsFactors = FALSE)
df_IRI=df_IRI %>% filter(!(STATE_CODE_EXP %in% Out_of_US))
df_IRI=df_IRI %>% filter(!(STATE_CODE_EXP %in% Out_of_US)) %>% select(STATE_CODE,STATE_CODE_EXP,SHRP_ID,VISIT_DATE,IRI_LEFT_WHEEL_PATH,IRI_RIGHT_WHEEL_PATH,CONSTRUCTION_NO)
df_IRI=df_IRI %>% mutate(IRI_ave=ave(IRI_LEFT_WHEEL_PATH,IRI_RIGHT_WHEEL_PATH))
df_IRI$VISIT_DATE=as.Date(df_IRI$VISIT_DATE,format ="%m/%d/%Y")
df_IRI=df_IRI %>% group_by(STATE_CODE,SHRP_ID) %>% arrange(VISIT_DATE,CONSTRUCTION_NO)

df_pavement_info_ac_only=df_pavement_info %>% filter(LAYER_TYPE=="AC") %>% group_by(STATE_CODE_EXP,SHRP_ID,CONSTRUCTION_NO) %>% summarise(ac_th=sum(REPR_THICKNESS))

###Structure

state_name=unique(df_pavement_info$STATE_CODE_EXP)
layer_type=unique(df_pavement_info$LAYER_TYPE_EXP)
construction_no=unique(df_pavement_info$CONSTRUCTION_NO)
