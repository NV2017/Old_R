#################################################################################
#################################### Input ######################################

Ticker <- "NIFTY" # Example: MINDTREE
Expiry <- "26NOV2020" # Keep format for '26NOV2020', otherwise code will not work

Project_Directory <- "C:/Users/aurnabha/Desktop/Codes_Handling_Central_Database/Live_NSE_Option_Chain_Data"

#################################################################################
#################################################################################

#################################################################################
#################################### Goals ######################################

# Date: 2020-November-19
# Author: Arunabha Sarkar

# Goals: Live_NSE_Option_Chain_Data
# File Name: Live_NSE_Option_Chain_Data

#################################################################################
#################################################################################

#################################################################################
##################### Initializing and loading Libraries ########################

library(mgsub)
library(dplyr)
library(rvest)
library(stringr)
library(magrittr)

#################################################################################
#################################################################################

#################################################################################
################## Set Directories & other Hyperparameters ######################

Output_Folder_Name <- "Output"

Project_Directory %>% setwd

if(!file.exists(Output_Folder_Name))
{
  Output_Folder_Name %>% paste0("Creating '",.,"' directory.",sep="") %>% print
  dir.create(file.path(Project_Directory, Output_Folder_Name))
}else{
  Output_Folder_Name %>% paste0("Directory '",.,"' already exists.",sep="") %>% print
} # End of 'if(!file.exists(Output_Folder_Name))'

Project_Directory %>% paste0(.,"/",Output_Folder_Name,sep='') -> Output_Folder

#################################################################################
#################################################################################

#################################################################################
############################# Download Data #####################################

Output_Folder %>% setwd %T>% {list.files(.) ->> Output_Folder_Files;tempfile() ->> t_f;3->>curseT;.} %>% 
  {.} %T>% {paste0(mgsub::mgsub(substr(as.character(Sys.time()),1,16),c("-"," ",":"),c("","","")),"_",
                   Ticker,"_Expiry_",Expiry,".csv",sep="")->>Output_File_Name;.} %T>% 
  {P1OURL <<- "https://www1.nseindia.com/live_market/dynaContent/live_watch/option_chain/optionKeys.jsp?segmentLink=17&instrument=OPT"; 
  ;P2OURL <<- "&symbol=";P3OURL <<- "&date=";.}%T>% 
  {toupper(Ticker)->>Ticker;toupper(Expiry)->>Expiry;.} %>% 
  {if_else(.%in%list.files(),paste0("'",Output_File_Name,"' is already present in '",Output_Folder,"'",sep=""),"Do")} %T>% 
  {if(grepl("NIFTY",Ticker)){eval(parse(text="TTT<<-c('IDX');"))}else{eval(parse(text="TTT<<-c('STK');"))};.} %>% 
  {if(grepl("Do",.)){paste0(P1OURL,TTT,P2OURL,Ticker,P3OURL,Expiry,sep="")->teHT;
  as.character(html_nodes(read_html(teHT),"#wrapper_btm")[1])->tePrDaTi;
  gregexpr("As on ",tePrDaTi)[[1]][1]->test;gregexpr(" IST<a> <img",tePrDaTi)[[1]][1]->teen;
  substr(tePrDaTi,test+6,teen-1)->teDaTi;html_table(read_html(teHT),fill=TRUE)[[curseT]]->teoudf;
  colnames(teoudf)->teoudfco;gsub(" ","_",paste(teoudfco,as.character(unlist(teoudf[1,])),sep=" "))->colnames(teoudf);
  rev(rev(teoudf[,-1])[,-1])->teoudf;teoudf[-1,]->teoudf;teoudf$SYMBOL<-Ticker;teoudf$EXPIRY<-Expiry;
  teoudf$DATE<-substr(teDaTi,1,14);teoudf$TIME<-substr(teDaTi,14,21);
  if(nrow(teoudf)>0){write.csv(teoudf,file=Output_File_Name,row.names = F,na="");}else{print("No trade/data on website")};
  print("DONE");}else{print("File already present")}}

#################################################################################
#################################################################################