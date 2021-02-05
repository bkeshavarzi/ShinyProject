rm(list=ls())
library(tidyverse)
library(usmap)
library(googleVis)

df_pavement_SHRP=read.csv("PavementStructure_SHRP_Info.csv",stringsAsFactors = FALSE)
df_pavement_Info=read.csv("PavementStructure_Pavement_Info.csv")
df_pavement_SHRP=df_pavement_SHRP %>% select(STATE_CODE,SHRP_ID,STATE_CODE_EXP,START_DATE,END_DATE)
df_pavement_Info=df_pavement_Info %>% select(STATE_CODE,SHRP_ID,STATE_CODE_EXP,CONSTRUCTION_NO,LAYER_NO,LAYER_TYPE,LAYER_TYPE_EXP,REPR_THICKNESS)
Out_of_US=c("Alberta","British Columbia","New Brunswick","Newfoundland","Nova Scotia","Ontario","Prince Edward Island","Quebec","Saskatchewan","Manitoba")
df_pavement_info=df_pavement_Info %>% filter(!(STATE_CODE_EXP %in% Out_of_US))
df_pavement_SHRP=df_pavement_SHRP %>% filter(!(STATE_CODE_EXP %in% Out_of_US))
index=substring(df_pavement_SHRP$END_DATE,7,10)=="2050"
df_pavement_SHRP$END_DATE[index]=paste0(substring(df_pavement_SHRP$END_DATE[index],1,6),"2005")
df_pavement_SHRP$START_DATE=as.Date(df_pavement_SHRP$START_DATE,"%m/%d/%Y")
df_pavement_SHRP$END_DATE=as.Date(df_pavement_SHRP$END_DATE,"%m/%d/%Y")

df_pavement_data=merge(df_pavement_Info,df_pavement_SHRP,by.x=c("STATE_CODE","SHRP_ID","STATE_CODE_EXP"),by.y=c("STATE_CODE","SHRP_ID","STATE_CODE_EXP"))
df_pavement_data=df_pavement_data %>% mutate(SHRP_STATE=paste0(STATE_CODE_EXP,SHRP_ID))
rm(df_pavement_Info,df_pavement_SHRP)

df_section_per_state=df_pavement_data %>% group_by(STATE_CODE_EXP,SHRP_ID) %>% summarise(number=n()) %>% select(STATE_CODE_EXP) %>%group_by(STATE_CODE_EXP) %>% summarise(n_=n()) 
colnames(df_section_per_state)=c('state','number')

plot_usmap(regions = 'states',data = df_section_per_state,values = "number",labels = FALSE)+scale_fill_continuous(
  low = "blue", high = "red", name = "Number of Sections in the Data Base")+theme(legend.position = "right")

df_gps=read.csv("GPS.csv",stringsAsFactors = FALSE)
df_gps=df_gps %>% filter(!(STATE_CODE_EXP %in% Out_of_US))
df_gps=df_gps %>% select(STATE_CODE,STATE_CODE_EXP,SHRP_ID,LATITUDE,LONGITUDE,ELEVATION)

df_gps_transformed=usmap_transform(data.frame(lon=df_gps$LONGITUDE,lat=df_gps$LATITUDE))
plot_usmap(labels=FALSE) +geom_point(data = df_gps_transformed, aes(x = lon.1, y = lat.1),color="blue",alpha=0.1)

df_cracking_data=read.csv("AC_Cracking_Crack_Data.csv",stringsAsFactors = FALSE)
df_cracking_data=df_cracking_data %>% filter(!(STATE_CODE_EXP %in% Out_of_US))


df_cracking_data$Edge=df_cracking_data$EDGE_CRACK_L_L+df_cracking_data$EDGE_CRACK_L_M+df_cracking_data$EDGE_CRACK_L_H
df_cracking_data$Long_WP=df_cracking_data$LONG_CRACK_WP_L_L+df_cracking_data$LONG_CRACK_WP_L_M+df_cracking_data$LONG_CRACK_WP_L_H
df_cracking_data$Long_WP_Seal=df_cracking_data$LONG_CRACK_WP_SEAL_L_L+df_cracking_data$LONG_CRACK_WP_SEAL_L_M+df_cracking_data$LONG_CRACK_WP_SEAL_L_H
df_cracking_data$Long_NWP=df_cracking_data$LONG_CRACK_NWP_L_L+df_cracking_data$LONG_CRACK_NWP_L_M+df_cracking_data$LONG_CRACK_NWP_L_H
df_cracking_data$Long_NWP_Seal=df_cracking_data$LONG_CRACK_NWP_SEAL_L_L+df_cracking_data$LONG_CRACK_NWP_SEAL_L_M+df_cracking_data$LONG_CRACK_NWP_SEAL_L_H
df_cracking_data$Transverse=df_cracking_data$TRANS_CRACK_NO_L+df_cracking_data$TRANS_CRACK_NO_M+df_cracking_data$TRANS_CRACK_NO_H

df_cracking_final=df_cracking_data %>% select(STATE_CODE,STATE_CODE_EXP,SHRP_ID,SURVEY_DATE,CONSTRUCTION_NO,Edge,Long_WP,Long_WP_Seal,Long_NWP,Long_NWP_Seal,Transverse)
rm(df_cracking_data)

df_cracking_gps=merge(df_cracking_final,df_gps,by.x=c("STATE_CODE_EXP","SHRP_ID"),by.y=c("STATE_CODE_EXP","SHRP_ID"))
df_cracking_gps=merge(df_cracking_gps,df_gps_transformed,by.x=c("LATITUDE","LONGITUDE"),by.y=c("lat","lon"))
df_cracking_gps=df_cracking_gps %>% group_by(STATE_CODE_EXP,SHRP_ID,CONSTRUCTION_NO) %>% arrange(SURVEY_DATE)
df_cracking_gps$SURVEY_DATE=as.Date(df_cracking_gps$SURVEY_DATE,format="%m/%d/%Y")

index_pc=df_pavement_data %>% filter(LAYER_TYPE=="PC") %>% select(SHRP_ID,STATE_CODE_EXP)
df_pavement_data_pc=df_pavement_data %>% filter((SHRP_ID %in% index_pc$SHRP_ID)&(STATE_CODE_EXP %in%index_pc$STATE_CODE_EXP))

index_pc=df_pavement_data %>% filter(LAYER_TYPE=="PC") %>% select(SHRP_ID,STATE_CODE_EXP,SHRP_STATE)
df_pavement_data_pc=df_pavement_data %>% filter(SHRP_STATE %in% index_pc$SHRP_STATE)
df_pavement_data_ac=df_pavement_data %>% filter(!(SHRP_STATE %in%index_pc$SHRP_STATE))

df_section_per_state_pc=df_pavement_data_pc %>% group_by(SHRP_ID,STATE_CODE_EXP) %>% summarise(n_=n()) %>% group_by(STATE_CODE_EXP) %>% summarise(number_pc=n())
df_section_per_state_ac=df_pavement_data_ac %>% group_by(SHRP_ID,STATE_CODE_EXP) %>% summarise(n_=n()) %>% group_by(STATE_CODE_EXP) %>% summarise(number_pc=n())
colnames(df_section_per_state_ac)=c("State","Number of Section")
colnames(df_section_per_state_pc)=c("State","Number of Section")

df_seal=read.csv("Seal.csv",stringsAsFactors = FALSE)
df_seal=df_seal %>% mutate(SHRP_STATE=paste0(STATE_CODE_EXP,SHRP_ID))

df_IRI=read.csv("IRI.csv",stringsAsFactors = FALSE)
df_IRI=df_IRI %>% mutate(SHRP_STATE=paste0(STATE_CODE_EXP,SHRP_ID))
df_IRI=df_IRI %>% filter(!(STATE_CODE_EXP %in% Out_of_US))
df_IRI$IRI_LEFT_WHEEL_PATH=as.double(df_IRI$IRI_LEFT_WHEEL_PATH)
df_IRI$IRI_RIGHT_WHEEL_PATH=as.double(df_IRI$IRI_RIGHT_WHEEL_PATH)
df_IRI=df_IRI %>% mutate(IRI_ave=0.5*(IRI_LEFT_WHEEL_PATH+IRI_RIGHT_WHEEL_PATH))
df_IRI$VISIT_DATE=as.Date(df_IRI$VISIT_DATE,format="%m/%d/%Y")

geo_ac=gvisGeoChart(df_section_per_state_ac,"State","Number of Section",options=list(region="US",displayMode="regions",resolution="provinces"))
geo_pc=gvisGeoChart(df_section_per_state_pc,"State","Number of Section",options=list(region="US",displayMode="regions",resolution="provinces"))
plot(geo_ac)
plot(geo_pc)

df_mera_shrp=read.csv("MERA_SHRP.csv",stringsAsFactors = FALSE)
df_mera_temp_month=read.csv("MERRA_TEMP_MONTH.csv",stringsAsFactors = FALSE)

df_mera=merge(df_mera_shrp,df_mera_temp_month,by="MERRA_ID")



