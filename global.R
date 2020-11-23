library(tidyverse)
df_pavement_SHRP=read.csv("PavementStructure_SHRP_Info.csv",stringsAsFactors = FALSE)
df_pavement_Info=read_csv("PavementStructure_Pavement_Info.csv")
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

index_pc=df_pavement_data %>% filter(LAYER_TYPE=="PC") %>% select(SHRP_ID,STATE_CODE_EXP)
df_pavement_data_pc=df_pavement_data %>% filter((SHRP_ID %in% index_pc$SHRP_ID)&(STATE_CODE_EXP %in%index_pc$STATE_CODE_EXP))

index_pc=df_pavement_data %>% filter(LAYER_TYPE=="PC") %>% select(SHRP_ID,STATE_CODE_EXP,SHRP_STATE)
df_pavement_data_pc=df_pavement_data %>% filter(SHRP_STATE %in% index_pc$SHRP_STATE)
df_pavement_data_ac=df_pavement_data %>% filter(!(SHRP_STATE %in%index_pc$SHRP_STATE))

df_section_per_state_pc=df_pavement_data_pc %>% group_by(SHRP_ID,STATE_CODE_EXP) %>% summarise(n_=n()) %>% group_by(STATE_CODE_EXP) %>% summarise(number_pc=n())
df_section_per_state_ac=df_pavement_data_ac %>% group_by(SHRP_ID,STATE_CODE_EXP) %>% summarise(n_=n()) %>% group_by(STATE_CODE_EXP) %>% summarise(number_pc=n())