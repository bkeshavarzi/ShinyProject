library(tidyverse)
library(googleVis)
rm(list=ls())
setwd("C:\\Users\\behroozk\\Documents\\RProject\\ShinyApp\\")
rm(list=ls())

df_pavement_SHRP=read.csv("PavementStructure_SHRP_Info.csv",stringsAsFactors = FALSE)
df_pavement_info=read.csv("PavementStructure_Pavement_Info.csv")
Out_of_US=c("Alberta","British Columbia","New Brunswick","Newfoundland","Nova Scotia","Ontario","Prince Edward Island","Quebec","Saskatchewan","Manitoba")

df_pavement_info=df_pavement_info %>% filter(!(STATE_CODE_EXP %in% Out_of_US))%>%select(STATE_CODE_EXP,SHRP_ID,CONSTRUCTION_NO,LAYER_NO,LAYER_TYPE,LAYER_TYPE_EXP,REPR_THICKNESS)
df_pavement_SHRP=df_pavement_SHRP %>% filter(!(STATE_CODE_EXP %in% Out_of_US))%>% select(STATE_CODE_EXP,STATE_CODE,SHRP_ID,START_DATE,END_DATE) 
df_pavement_info$LAYER_TYPE_EXP=str_replace(df_pavement_info$LAYER_TYPE_EXP,"layer","")
df_pavement_info=df_pavement_info %>% filter(!is.na(REPR_THICKNESS))

df_pavement_SHRP$START_DATE=as.Date(df_pavement_SHRP$START_DATE,"%m/%d/%Y")
df_pavement_SHRP$END_DATE=as.Date(df_pavement_SHRP$END_DATE,"%m/%d/%Y")

df_pavement_SHRP$duration=as.integer(substr(df_pavement_SHRP$END_DATE,1,4))-as.integer(substr(df_pavement_SHRP$START_DATE,1,4))
df_pavement_SHRP$END_DATE=str_replace(df_pavement_SHRP$END_DATE,"2050","2005")
df_pavement_SHRP$duration=as.integer(substr(df_pavement_SHRP$END_DATE,1,4))-as.integer(substr(df_pavement_SHRP$START_DATE,1,4))

df_pavement_SHRP=df_pavement_SHRP %>% filter(!(duration<0))

df_pavement_SHRP=df_pavement_SHRP %>% group_by(STATE_CODE_EXP,STATE_CODE,SHRP_ID) %>% summarise(syear=min(as.integer(substr(START_DATE,1,4))),eyear=max(as.integer(substr(END_DATE,1,4))),duration=sum(duration))

df_pavement_info_AC=df_pavement_info %>% filter(LAYER_TYPE=='AC') %>% group_by(STATE_CODE_EXP,SHRP_ID) %>% summarise(th=sum(REPR_THICKNESS))
df_pavement_info_PC=df_pavement_info %>% filter(LAYER_TYPE=='PC') %>% group_by(STATE_CODE_EXP,SHRP_ID) %>% summarise(th=sum(REPR_THICKNESS))

pavement_data_AC=inner_join(df_pavement_SHRP,df_pavement_info_AC,by.x=c('STATE_CODE_EXP','SHRP_ID'),by.y=c('STATE_CODE_EXP','SHRP_ID'))
pavement_data_PC=inner_join(df_pavement_SHRP,df_pavement_info_PC,by.x=c('STATE_CODE_EXP','SHRP_ID'),by.y=c('STATE_CODE_EXP','SHRP_ID'))

df_gps=read.csv('GPS.csv',stringsAsFactors = FALSE)[,c(1,2,3,4,5,9)]
df_gps=df_gps %>% filter(!(STATE_CODE_EXP %in% Out_of_US))
df_gps$ELEVATION=round(df_gps$ELEVATION,digits=2)
df_mera_shrp=read.csv("MERA_SHRP.csv",stringsAsFactors = FALSE)
df_mera_temp_month=read.csv("MERRA_TEMP_MONTH.csv",stringsAsFactors = FALSE)
df_mera_temp_month=df_mera_temp_month %>% select(!FREEZE_INDEX & !FREEZE_THAW)
df_mera_temp_month=df_mera_temp_month %>% mutate(Tave_F=1.8*TEMP_AVG+32)
df_mera_temp_month=df_mera_temp_month %>% select(!TEMP_AVG & !TEMP_MEAN_AVG)
df_temperature=inner_join(df_mera_shrp,df_gps,by.x=c("STATE_CODE","SHRP_ID"),by.y=c("STATE_CODE","SHRP_ID"))

df_traffic=read.csv("Traffic.csv",stringsAsFactors = FALSE)[,seq(1,6)]
df_traffic=df_traffic %>% filter(!(STATE_CODE_EXP %in% Out_of_US))
df_traffic=inner_join(df_traffic,df_gps,by.x=c("STATE_CODE","SHRP_ID"),by.y=c("STATE_CODE","SHRP_ID"))
df_traffic=df_traffic %>% mutate(LatLong=paste(LATITUDE,LONGITUDE,sep=":"))
colnames(df_traffic)[6]="ESAL"