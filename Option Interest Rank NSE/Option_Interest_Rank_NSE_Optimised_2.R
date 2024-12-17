#################################################################################
#################################### Goals ######################################

# Date: 2020-July-21
# Author: Arunabha Sarkar

# Goals: Get Top 'n' OI of each stock in N200 derivative universe with strike, both PE/CE
# File Name: Option_Interest_Rank_NSE_Optimised_2

#################################################################################
#################################################################################

#################################################################################
##################### Initializing and loading Libraries ########################

library(dplyr)

#################################################################################
#################################################################################

#################################################################################
################## Set Directories & other Hyperparameters ######################

Top_N <- 5

Min_Cutoff_Year <- 2011

Step_Counter_Expiries <- 12

c("Dates_YYYYMMDD","SYMBOL","STRIKE_PR","OPEN_INT","Rank","OPTIONTYPE","CLOSE_PR","VAL_INLAKH","P_CHG_IN_OI_1_Day",
  "P_CHG_IN_OI_2_Day","P_CHG_IN_OI_1_Week","Peak_OI_in_Prev_Expiry","Date_Peak_OI_in_Prev_Expiry",
  "CLOSE_PR_Peak_OI_in_Prev_Expiry","VAL_INLAKH_Peak_OI_in_Prev_Expiry","Expiry_during_Peak_OI_in_Prev_Expiry",
  "Peak_OI_in_last_2_expiries","Date_Peak_OI_in_last_2_expiries","CLOSE_PR_Peak_OI_in_last_2_expiries",
  "VAL_INLAKH_Peak_OI_in_last_2_expiries","Expiry_during_Peak_OI_in_last_2_expiries","Peak_OI_in_last_6_expiries",
  "Date_Peak_OI_in_last_6_expiries","CLOSE_PR_Peak_OI_in_last_6_expiries",
  "VAL_INLAKH_Peak_OI_in_last_6_expiries","Expiry_during_Peak_OI_in_last_6_expiries") -> Output_Colnames

Output_Folder_Name <- "Daily_Option_Interest_Rank_NSE"

"C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% paste0("/NSE FnO Bhav Copies",sep='') -> FnO_Bhav_Database

Central_Database %>% setwd

if(!file.exists(Output_Folder_Name))
{
  Output_Folder_Name %>% paste0("Creating '",.,"' directory.",sep="") %>% print
  dir.create(file.path(Central_Database, Output_Folder_Name))
}else{
  Output_Folder_Name %>% paste0("Directory '",.,"' already exists.",sep="") %>% print
} # End of 'if(!file.exists('Survivorship Adjustments Indexes'))'

temp_FnO_colnames <- c("INSTRUMENT","SYMBOL","EXPIRY_DT","STRIKE_PR","OPTIONTYPE",
                       "OPEN","HIGH","LOW","CLOSE","SETTLE_PR","CONTRACTS","VAL_INLAKH",
                       "OPEN_INT","CHG_IN_OI","TIMESTAMP")

Central_Database %>% setwd

if(!file.exists(Output_Folder_Name))
{
  "Creating '" %>% paste0(.,Output_Folder_Name, "' directory",sep='') %>% print
  dir.create(file.path(Central_Database, Output_Folder_Name))
}else{
  "Directory '" %>% paste0(.,Output_Folder_Name, "' already exists",sep='') %>% print
} # End of 'if(!file.exists(Output_Folder_Name))'

Central_Database %>% paste0(.,"/",Output_Folder_Name,sep='') -> Output_Folder

#################################################################################
#################################################################################

#################################################################################
#################### Getting relevant bhav copy file list #######################

FnO_Bhav_Database %>% setwd

list.files() -> FnO_Bhav_Database_Files

FnO_Bhav_Database_Files %>% as.Date(format="fo%d%b%Ybhav.csv") %>% format("%Y") %>% 
  as.numeric %>% {which(. >= Min_Cutoff_Year)} %>% 
  FnO_Bhav_Database_Files[.] -> FnO_Bhav_Database_Files_relevant

FnO_Bhav_Database_Files_relevant %>% as.Date(format="fo%d%b%Ybhav.csv") %>% 
  format("%Y%m%d") %>% as.numeric -> FnO_Bhav_Database_Files_relevant_Dates

names(FnO_Bhav_Database_Files_relevant_Dates) <- FnO_Bhav_Database_Files_relevant

FnO_Bhav_Database_Files_relevant_Dates %>% sort -> FnO_Bhav_Database_Files_relevant_Dates

FnO_Bhav_Database_Files_relevant_Dates %>% names -> FnO_Bhav_Database_Files_relevant

#################################################################################
#################################################################################

#################################################################################
####################### Looping over each relevant file #########################

if(length(FnO_Bhav_Database_Files_relevant) > 0)
{
  "Found FnO_Bhav_Database_Files" %>% print
  # i_relevant <- 2470
  for (i_relevant in 1:length(FnO_Bhav_Database_Files_relevant))
  # for (i_relevant in 1:400)
  {
    Output_Folder %>% setwd
    list.files() -> Output_Folder_Files
    
    i_relevant %>% FnO_Bhav_Database_Files_relevant[.] %>% 
      as.Date(format="fo%d%b%Ybhav.csv") %>% format("%Y%m%d") %>% 
      paste0(.,"_Option_Interest_Rank_with_Strike.csv",sep='') -> temp_Output_Filename
    
    if(!(temp_Output_Filename %in% Output_Folder_Files))
    {
      i_relevant %>% paste0(.," out of ", length(FnO_Bhav_Database_Files_relevant), sep="") %>% 
        paste(., ", for '", temp_Output_Filename,"'",sep='') %>% print
      
      FnO_Bhav_Database %>% setwd
      i_relevant %>% FnO_Bhav_Database_Files_relevant[.] %>% read.csv %>% 
        .[,1:length(temp_FnO_colnames)] %>% data.frame(stringsAsFactors = F) %>% 
        `colnames<-`(temp_FnO_colnames) -> temp_file
      
      # Re-formatting Date
      temp_file[1,"EXPIRY_DT"] %>% as.character -> temp_sample_date
      temp_date_format <- c()
      if(nchar(temp_sample_date) >= 11)
      {
        temp_date_format <- c("%d-%b-%Y")
      }else
      {
        temp_date_format <- c("%d-%b-%y")
      } # End of 'if(nchar(temp_sample_date) >= 11)'
      
      temp_file %>% select(EXPIRY_DT) %>% unlist %>% as.character %>% as.Date(format=temp_date_format) %>% 
        format("%Y%m%d") %>% as.numeric -> temp_file$EXPIRY_DT
      
      temp_file %>% select(TIMESTAMP) %>% unlist %>% as.character %>% as.Date(format=temp_date_format) %>% 
        format("%Y%m%d") %>% as.numeric -> temp_file$TIMESTAMP
      # End of Re-formatting Date
      
      # Date
      i_relevant %>% FnO_Bhav_Database_Files_relevant[.] %>% 
        as.Date(format = "fo%d%b%Ybhav.csv") %>% format("%Y%m%d") %>%
        as.numeric -> temp_date
      
      # Finding number of stocks
      temp_file %>% filter(INSTRUMENT == "OPTSTK") -> temp_file_OPTSTK
      
      temp_file_OPTSTK[order(temp_file_OPTSTK$EXPIRY_DT),] -> temp_file_OPTSTK
      
      temp_file_OPTSTK %>% select(SYMBOL) %>% unlist %>% as.character %>% unique -> temp_NSE_Tickers
      
      # Making the output dataframe with blanks initially
      Output_Colnames %>% length %>% matrix(data=NA,nrow=2*Top_N*length(temp_NSE_Tickers),ncol=.) %>% 
        data.frame(stringsAsFactors = F) %>% 
        `colnames<-`(Output_Colnames) -> temp_output_df
      
      # Filling 'Dates_YYYYMMDD'
      temp_date -> temp_output_df$Dates_YYYYMMDD
      
      # Filling 'SYMBOL', 'STRIKE_PR', 'OPEN_INT', 'Rank', 'OPTIONTYPE','CLOSE_PR','VAL_INLAKH'
      i_relevant %>% paste0(.," out of ", length(FnO_Bhav_Database_Files_relevant), sep="") %>% 
        paste(., ", for '", temp_Output_Filename,"', filling today's details",sep='') %>% print
      
      temp_SYMBOL <- c()
      temp_OPTIONTYPE <- c()
      
      # i_tick <- 1
      for(i_tick in 1:length(temp_NSE_Tickers))
      {
        ########################### Delete Old loop values
        if(exists("temp_Peak_OI_in_last_6_expiries"))
        {
          rm(temp_Peak_OI_in_last_6_expiries)
        } # End of 'if(exists("temp_Peak_OI_in_last_6_expiries"))'
        
        if(exists("temp_Peak_OI_in_last_2_expiries"))
        {
          rm(temp_Peak_OI_in_last_2_expiries)
        } # End of 'if(exists("temp_Peak_OI_in_last_2_expiries"))'
        
        if(exists("temp_Peak_OI_in_Prev_Expiry"))
        {
          rm(temp_Peak_OI_in_Prev_Expiry)
        } # End of 'if(exists("temp_Peak_OI_in_Prev_Expiry"))'
        
        if(exists("temp_Date_Peak_OI_in_Prev_Expiry"))
        {
          rm(temp_Date_Peak_OI_in_Prev_Expiry)
        } # End of 'if(exists("temp_Date_Peak_OI_in_Prev_Expiry"))'
        
        if(exists("temp_CLOSE_PR_Peak_OI_in_Prev_Expiry"))
        {
          rm(temp_CLOSE_PR_Peak_OI_in_Prev_Expiry)
        } # End of 'if(exists("temp_CLOSE_PR_Peak_OI_in_Prev_Expiry"))'
        
        if(exists("temp_VAL_INLAKH_Peak_OI_in_Prev_Expiry"))
        {
          rm(temp_VAL_INLAKH_Peak_OI_in_Prev_Expiry)
        } # End of 'if(exists("temp_VAL_INLAKH_Peak_OI_in_Prev_Expiry"))'
        
        if(exists("temp_Expiry_during_Peak_OI_in_Prev_Expiry"))
        {
          rm(temp_Expiry_during_Peak_OI_in_Prev_Expiry)
        } # End of 'if(exists("temp_Expiry_during_Peak_OI_in_Prev_Expiry"))'
        
        if(exists("temp_Date_Peak_OI_last_2_expiries"))
        {
          rm(temp_Date_Peak_OI_last_2_expiries)
        } # End of 'if(exists("temp_Date_Peak_OI_last_2_expiries"))'
        
        if(exists("temp_CLOSE_PR_Peak_OI_last_2_expiries"))
        {
          rm(temp_CLOSE_PR_Peak_OI_last_2_expiries)
        } # End of 'if(exists("temp_CLOSE_PR_Peak_OI_last_2_expiries"))'
        
        if(exists("temp_VAL_INLAKH_Peak_OI_last_2_expiries"))
        {
          rm(temp_VAL_INLAKH_Peak_OI_last_2_expiries)
        } # End of 'if(exists("temp_VAL_INLAKH_Peak_OI_last_2_expiries"))'
        
        if(exists("temp_Expiry_during_Peak_OI_last_2_expiries"))
        {
          rm(temp_Expiry_during_Peak_OI_last_2_expiries)
        } # End of 'if(exists("temp_Expiry_during_Peak_OI_last_2_expiries"))'
        
        if(exists("temp_Date_Peak_OI_last_6_expiries"))
        {
          rm(temp_Date_Peak_OI_last_6_expiries)
        } # End of 'if(exists("temp_Date_Peak_OI_last_6_expiries"))'
        
        if(exists("temp_CLOSE_PR_Peak_OI_last_6_expiries"))
        {
          rm(temp_CLOSE_PR_Peak_OI_last_6_expiries)
        } # End of 'if(exists("temp_CLOSE_PR_Peak_OI_last_6_expiries"))'
        
        if(exists("temp_VAL_INLAKH_Peak_OI_last_6_expiries"))
        {
          rm(temp_VAL_INLAKH_Peak_OI_last_6_expiries)
        } # End of 'if(exists("temp_VAL_INLAKH_Peak_OI_last_6_expiries"))'
        
        if(exists("temp_Expiry_during_Peak_OI_last_6_expiries"))
        {
          rm(temp_Expiry_during_Peak_OI_last_6_expiries)
        } # End of 'if(exists("temp_Expiry_during_Peak_OI_last_6_expiries"))'
        ##################################################
        
        temp_file_OPTSTK %>% filter(SYMBOL==temp_NSE_Tickers[i_tick]) %>% 
          filter(OPTIONTYPE == "CE") %>% select(EXPIRY_DT) %>% unlist %>% 
          unique %>% min -> temp_most_recent_expiry
        
        # CE first
        temp_file_OPTSTK %>% filter(SYMBOL==temp_NSE_Tickers[i_tick]) %>% 
          filter(OPTIONTYPE == "CE") %>% filter(EXPIRY_DT == temp_most_recent_expiry) %>% 
          .[order(.$OPEN_INT,decreasing = TRUE),] -> temp_CE
        
        if(nrow(temp_CE) < Top_N)
        {
          temp_CE %>% colnames %>% length %>% matrix(data=NA,nrow=(Top_N-nrow(temp_CE)),ncol=.) %>% 
            data.frame(stringsAsFactors = F) %>% `colnames<-`(colnames(temp_CE)) -> temp_temp
          
          "OPTSTK" -> temp_temp$INSTRUMENT
          
          i_tick %>% temp_NSE_Tickers[.] -> temp_temp$SYMBOL
          
          "CE" -> temp_temp$OPTIONTYPE
          
          temp_date -> temp_temp$TIMESTAMP
          
          temp_most_recent_expiry -> temp_temp$EXPIRY_DT
          
          0 -> temp_temp$OPEN_INT
          
          temp_temp %>% rbind(temp_CE,.) -> temp_CE
        } # End of 'if(nrow(temp_CE) < Top_N)'
        
        temp_CE %>% .[order(temp_CE$OPEN_INT, decreasing = T),] %>% .[1:Top_N,] %>% 
          .[,c("SYMBOL","STRIKE_PR","OPEN_INT","OPTIONTYPE","CLOSE","VAL_INLAKH")] -> temp_CE
        
        temp_CE$Rank <- 1:Top_N
        
        temp_CE <- temp_CE[,c("SYMBOL","STRIKE_PR","OPEN_INT","Rank","OPTIONTYPE","CLOSE","VAL_INLAKH")]
        
        # PE second
        temp_file_OPTSTK %>% filter(SYMBOL==temp_NSE_Tickers[i_tick]) %>% 
          filter(OPTIONTYPE == "PE") %>% filter(EXPIRY_DT == temp_most_recent_expiry) %>% 
          .[order(.$OPEN_INT,decreasing = TRUE),] -> temp_PE
        
        if(nrow(temp_PE) < Top_N)
        {
          temp_PE %>% colnames %>% length %>% matrix(data=NA,nrow=(Top_N-nrow(temp_PE)),ncol=.) %>% 
            data.frame(stringsAsFactors = F) %>% `colnames<-`(colnames(temp_PE)) -> temp_temp
          
          "OPTSTK" -> temp_temp$INSTRUMENT
          
          i_tick %>% temp_NSE_Tickers[.] -> temp_temp$SYMBOL
          
          "PE" -> temp_temp$OPTIONTYPE
          
          temp_date -> temp_temp$TIMESTAMP
          
          temp_most_recent_expiry -> temp_temp$EXPIRY_DT
          
          0 -> temp_temp$OPEN_INT
          
          temp_temp %>% rbind(temp_PE,.) -> temp_PE
        } # End of 'if(nrow(temp_PE) < Top_N)'
        
        temp_PE %>% .[order(temp_PE$OPEN_INT, decreasing = T),] %>% .[1:Top_N,] %>% 
          .[,c("SYMBOL","STRIKE_PR","OPEN_INT","OPTIONTYPE","CLOSE","VAL_INLAKH")] -> temp_PE
        
        temp_PE$Rank <- 1:Top_N
        
        temp_PE <- temp_PE[,c("SYMBOL","STRIKE_PR","OPEN_INT","Rank","OPTIONTYPE","CLOSE","VAL_INLAKH")]
        
        # Merge 'temp_PE' & 'temp_CE'
        temp_CE %>% rbind(.,temp_PE) -> temp
        c("SYMBOL","STRIKE_PR","OPEN_INT","Rank","OPTIONTYPE","CLOSE_PR","VAL_INLAKH") -> colnames(temp)
        
        which(Output_Colnames %in% colnames(temp)) -> temp_correct_cols
        {(i_tick-1)*2*Top_N} %>% {. + 1} -> temp_correct_row_start
        {(i_tick)*2*Top_N} -> temp_correct_row_end
        
        temp %>% select(SYMBOL) %>% unlist %>% as.character %>% 
          c(temp_SYMBOL,.) -> temp_SYMBOL
        
        temp %>% select(OPTIONTYPE) %>% unlist %>% as.character %>% 
          c(temp_OPTIONTYPE,.) -> temp_OPTIONTYPE
        
        temp -> temp_output_df[(temp_correct_row_start:temp_correct_row_end),temp_correct_cols]
      } # End of 'for(i_tick in 1:length(temp_NSE_Tickers))'
      
      temp_SYMBOL -> temp_output_df$SYMBOL
      temp_OPTIONTYPE -> temp_output_df$OPTIONTYPE
      
      # Making 'P_CHG_IN_OI_1_Day', iterating over each row of 'temp_output_df'
      i_relevant %>% paste0(.," out of ", length(FnO_Bhav_Database_Files_relevant), sep="") %>% 
        paste(., ", for '", temp_Output_Filename,"', P_CHG_IN_OI_1_Day",sep='') %>% print
      
      temp_file_OPTSTK %>% select(EXPIRY_DT) %>% unlist %>% unique %>% min -> temp_most_recent_expiry
      i_relevant %>% {.-1} %>% {if_else(.>0,TRUE,FALSE)} -> Do_P_CHG_IN_OI_1_Day
      
      if(Do_P_CHG_IN_OI_1_Day)
      {
        # i_temp_output_df <- 1
        for(i_temp_output_df in 1:nrow(temp_output_df))
        {
          i_relevant %>% paste0(.," out of ", length(FnO_Bhav_Database_Files_relevant), sep="") %>% 
            paste(., ", for '", temp_Output_Filename,"', P_CHG_IN_OI_1_Day, ",sep='') %>% 
            paste(.,i_temp_output_df, " out of ",nrow(temp_output_df),sep='') %>% print
          
          i_temp_output_df %>% temp_output_df[.,"SYMBOL"] -> temp_SYMBOL
          i_temp_output_df %>% temp_output_df[.,"STRIKE_PR"] -> temp_STRIKE_PR
          i_temp_output_df %>% temp_output_df[.,"OPEN_INT"] -> temp_today_OPEN_INT
          i_temp_output_df %>% temp_output_df[.,"OPTIONTYPE"] -> temp_OPTIONTYPE
          
          FnO_Bhav_Database %>% setwd
          i_relevant %>% {.-1} %>% FnO_Bhav_Database_Files_relevant[.] %>% read.csv %>% 
            .[,1:length(temp_FnO_colnames)] %>% data.frame(stringsAsFactors = F) %>% 
            `colnames<-`(temp_FnO_colnames) -> temp_prev_file
          
          # Re-formatting Date
          temp_prev_file[1,"EXPIRY_DT"] %>% as.character -> temp_sample_date
          temp_date_format <- c()
          if(nchar(temp_sample_date) >= 11)
          {
            temp_date_format <- c("%d-%b-%Y")
          }else
          {
            temp_date_format <- c("%d-%b-%y")
          } # End of 'if(nchar(temp_sample_date) >= 11)'
          
          temp_prev_file %>% select(EXPIRY_DT) %>% unlist %>% as.character %>% as.Date(format=temp_date_format) %>% 
            format("%Y%m%d") %>% as.numeric -> temp_prev_file$EXPIRY_DT
          
          temp_prev_file %>% select(TIMESTAMP) %>% unlist %>% as.character %>% as.Date(format=temp_date_format) %>% 
            format("%Y%m%d") %>% as.numeric -> temp_prev_file$TIMESTAMP
          # End of Re-formatting Date
          
          # Filtering 'temp_prev_file' with all criterias
          temp_prev_file %>% filter(INSTRUMENT == "OPTSTK") %>% filter(EXPIRY_DT == temp_most_recent_expiry) %>% 
            filter(STRIKE_PR == temp_STRIKE_PR) %>% filter(OPTIONTYPE == temp_OPTIONTYPE) %>% 
            filter(SYMBOL == temp_SYMBOL) -> temp_prev_file
          
          if(nrow(temp_prev_file) > 0)
          {
            temp_prev_file[1,"OPEN_INT"] -> temp_prev_OPEN_INT
            if((!is.na(temp_prev_OPEN_INT)) & (!is.na(temp_today_OPEN_INT)))
            {
              ((temp_today_OPEN_INT - temp_prev_OPEN_INT)/(temp_prev_OPEN_INT)) %>% 
                {.*100} %>% round(.,digits=2) -> temp_output_df[i_temp_output_df,"P_CHG_IN_OI_1_Day"]
            } # End of 'if((!is.na(temp_prev_OPEN_INT)) & (!is.na(temp_prev_OPEN_INT)))'
          } # End of 'if(nrow(temp_prev_file) > 0)'
          
        } # End of 'for(i_temp_output_df in 1:nrow(temp_output_df))'
      } # End of 'if(Do_P_CHG_IN_OI_1_Day)'
      
      # End of # Making 'P_CHG_IN_OI_1_Day', iterating over each row of 'temp_output_df'
      
      # Making 'P_CHG_IN_OI_2_Day', iterating over each row of 'temp_output_df'
      i_relevant %>% paste0(.," out of ", length(FnO_Bhav_Database_Files_relevant), sep="") %>% 
        paste(., ", for '", temp_Output_Filename,"', P_CHG_IN_OI_2_Day",sep='') %>% print
      
      temp_file_OPTSTK %>% select(EXPIRY_DT) %>% unlist %>% unique %>% min -> temp_most_recent_expiry
      i_relevant %>% {.-2} %>% {if_else(.>0,TRUE,FALSE)} -> Do_P_CHG_IN_OI_2_Day
      
      if(Do_P_CHG_IN_OI_2_Day)
      {
        # i_temp_output_df <- 1; i_temp_output_df <- 691
        for(i_temp_output_df in 1:nrow(temp_output_df))
        {
          i_relevant %>% paste0(.," out of ", length(FnO_Bhav_Database_Files_relevant), sep="") %>% 
            paste(., ", for '", temp_Output_Filename,"', P_CHG_IN_OI_2_Day, ",sep='') %>% 
            paste(.,i_temp_output_df, " out of ",nrow(temp_output_df),sep='') %>% print
          
          i_temp_output_df %>% temp_output_df[.,"SYMBOL"] -> temp_SYMBOL
          i_temp_output_df %>% temp_output_df[.,"STRIKE_PR"] -> temp_STRIKE_PR
          i_temp_output_df %>% temp_output_df[.,"OPEN_INT"] -> temp_today_OPEN_INT
          i_temp_output_df %>% temp_output_df[.,"OPTIONTYPE"] -> temp_OPTIONTYPE
          
          FnO_Bhav_Database %>% setwd
          i_relevant %>% {.-2} %>% FnO_Bhav_Database_Files_relevant[.] %>% read.csv %>% 
            .[,1:length(temp_FnO_colnames)] %>% data.frame(stringsAsFactors = F) %>% 
            `colnames<-`(temp_FnO_colnames) -> temp_prev_file
          
          # Re-formatting Date
          temp_prev_file[1,"EXPIRY_DT"] %>% as.character -> temp_sample_date
          temp_date_format <- c()
          if(nchar(temp_sample_date) >= 11)
          {
            temp_date_format <- c("%d-%b-%Y")
          }else
          {
            temp_date_format <- c("%d-%b-%y")
          } # End of 'if(nchar(temp_sample_date) >= 11)'
          
          temp_prev_file %>% select(EXPIRY_DT) %>% unlist %>% as.character %>% as.Date(format=temp_date_format) %>% 
            format("%Y%m%d") %>% as.numeric -> temp_prev_file$EXPIRY_DT
          
          temp_prev_file %>% select(TIMESTAMP) %>% unlist %>% as.character %>% as.Date(format=temp_date_format) %>% 
            format("%Y%m%d") %>% as.numeric -> temp_prev_file$TIMESTAMP
          # End of Re-formatting Date
          
          # Filtering 'temp_prev_file' with all criterias
          temp_prev_file %>% filter(INSTRUMENT == "OPTSTK") %>% filter(EXPIRY_DT == temp_most_recent_expiry) %>% 
            filter(STRIKE_PR == temp_STRIKE_PR) %>% filter(OPTIONTYPE == temp_OPTIONTYPE) %>% 
            filter(SYMBOL == temp_SYMBOL) -> temp_prev_file
          
          if(nrow(temp_prev_file) > 0)
          {
            temp_prev_file[1,"OPEN_INT"] -> temp_prev_OPEN_INT
            if((!is.na(temp_prev_OPEN_INT)) & (!is.na(temp_today_OPEN_INT)))
            {
              ((temp_today_OPEN_INT - temp_prev_OPEN_INT)/(temp_prev_OPEN_INT)) %>% 
                {.*100} %>% round(.,digits=2) -> temp_output_df[i_temp_output_df,"P_CHG_IN_OI_2_Day"]
            } # End of 'if((!is.na(temp_prev_OPEN_INT)) & (!is.na(temp_prev_OPEN_INT)))'
          } # End of 'if(nrow(temp_prev_file) > 0)'
          
        } # End of 'for(i_temp_output_df in 1:nrow(temp_output_df))'
      } # End of 'if(Do_P_CHG_IN_OI_2_Day)'
      
      # End of # Making 'P_CHG_IN_OI_2_Day', iterating over each row of 'temp_output_df'
      
      # Making 'P_CHG_IN_OI_1_Week', iterating over each row of 'temp_output_df'
      i_relevant %>% paste0(.," out of ", length(FnO_Bhav_Database_Files_relevant), sep="") %>% 
        paste(., ", for '", temp_Output_Filename,"', P_CHG_IN_OI_1_Week",sep='') %>% print
      
      temp_file_OPTSTK %>% select(EXPIRY_DT) %>% unlist %>% unique %>% min -> temp_most_recent_expiry
      i_relevant %>% {.-5} %>% {if_else(.>0,TRUE,FALSE)} -> Do_P_CHG_IN_OI_1_Week
      
      if(Do_P_CHG_IN_OI_1_Week)
      {
        # i_temp_output_df <- 1
        for(i_temp_output_df in 1:nrow(temp_output_df))
        {
          i_relevant %>% paste0(.," out of ", length(FnO_Bhav_Database_Files_relevant), sep="") %>% 
            paste(., ", for '", temp_Output_Filename,"', P_CHG_IN_OI_1_Week, ",sep='') %>% 
            paste(.,i_temp_output_df, " out of ",nrow(temp_output_df),sep='') %>% print
          
          i_temp_output_df %>% temp_output_df[.,"SYMBOL"] -> temp_SYMBOL
          i_temp_output_df %>% temp_output_df[.,"STRIKE_PR"] -> temp_STRIKE_PR
          i_temp_output_df %>% temp_output_df[.,"OPEN_INT"] -> temp_today_OPEN_INT
          i_temp_output_df %>% temp_output_df[.,"OPTIONTYPE"] -> temp_OPTIONTYPE
          
          FnO_Bhav_Database %>% setwd
          i_relevant %>% {.-5} %>% FnO_Bhav_Database_Files_relevant[.] %>% read.csv %>% 
            .[,1:length(temp_FnO_colnames)] %>% data.frame(stringsAsFactors = F) %>% 
            `colnames<-`(temp_FnO_colnames) -> temp_prev_file
          
          # Re-formatting Date
          temp_prev_file[1,"EXPIRY_DT"] %>% as.character -> temp_sample_date
          temp_date_format <- c()
          if(nchar(temp_sample_date) >= 11)
          {
            temp_date_format <- c("%d-%b-%Y")
          }else
          {
            temp_date_format <- c("%d-%b-%y")
          } # End of 'if(nchar(temp_sample_date) >= 11)'
          
          temp_prev_file %>% select(EXPIRY_DT) %>% unlist %>% as.character %>% as.Date(format=temp_date_format) %>% 
            format("%Y%m%d") %>% as.numeric -> temp_prev_file$EXPIRY_DT
          
          temp_prev_file %>% select(TIMESTAMP) %>% unlist %>% as.character %>% as.Date(format=temp_date_format) %>% 
            format("%Y%m%d") %>% as.numeric -> temp_prev_file$TIMESTAMP
          # End of Re-formatting Date
          
          # Filtering 'temp_prev_file' with all criterias
          temp_prev_file %>% filter(INSTRUMENT == "OPTSTK") %>% filter(EXPIRY_DT == temp_most_recent_expiry) %>% 
            filter(STRIKE_PR == temp_STRIKE_PR) %>% filter(OPTIONTYPE == temp_OPTIONTYPE) %>% 
            filter(SYMBOL == temp_SYMBOL) -> temp_prev_file
          
          if(nrow(temp_prev_file) > 0)
          {
            temp_prev_file[1,"OPEN_INT"] -> temp_prev_OPEN_INT
            if((!is.na(temp_prev_OPEN_INT)) & (!is.na(temp_today_OPEN_INT)))
            {
              ((temp_today_OPEN_INT - temp_prev_OPEN_INT)/(temp_prev_OPEN_INT)) %>% 
                {.*100} %>% round(.,digits=2) -> temp_output_df[i_temp_output_df,"P_CHG_IN_OI_1_Week"]
            } # End of 'if((!is.na(temp_prev_OPEN_INT)) & (!is.na(temp_prev_OPEN_INT)))'
          } # End of 'if(nrow(temp_prev_file) > 0)'
          
        } # End of 'for(i_temp_output_df in 1:nrow(temp_output_df))'
      } # End of 'if(Do_P_CHG_IN_OI_1_Week)'
      
      # End of # Making 'P_CHG_IN_OI_1_Week', iterating over each row of 'temp_output_df'
      
      # Finding all 6 Expiries, recording the approx. first and last hits
      i_relevant %>% paste0(.," out of ", length(FnO_Bhav_Database_Files_relevant), sep="") %>% 
        paste(., ", for '", temp_Output_Filename,"', finding 6 prev expiries",sep='') %>% print
      
      old_expiries <- NA
      counter <- i_relevant
      First_Detect <- FALSE
      First_counter <- NA
      Last_counter <- NA
      while(length(old_expiries) < 6)
      {
        # old_expiries %>% print
        if(counter == 0)
        {
          break
        } # End of 'if(counter == 0)'
        FnO_Bhav_Database %>% setwd
        counter %>% FnO_Bhav_Database_Files_relevant[.] %>% read.csv %>% 
          .[,1:length(temp_FnO_colnames)] %>% data.frame(stringsAsFactors = F) %>% 
          `colnames<-`(temp_FnO_colnames) %>% 
          filter(INSTRUMENT == "OPTSTK") -> temp_bhav
        
        if(nrow(temp_bhav) > 0)
        {
          # Re-formatting Date
          temp_bhav[1,"EXPIRY_DT"] %>% as.character -> temp_sample_date
          temp_date_format <- c()
          if(nchar(temp_sample_date) >= 11)
          {
            temp_date_format <- c("%d-%b-%Y")
          }else{
            temp_date_format <- c("%d-%b-%y")
          } # End of 'if(nchar(temp_sample_date) >= 11)'
          
          temp_bhav %>% select(EXPIRY_DT) %>% unlist %>% as.character %>% as.Date(format=temp_date_format) %>% 
            format("%Y%m%d") %>% as.numeric -> temp_bhav$EXPIRY_DT
          
          temp_bhav %>% select(TIMESTAMP) %>% unlist %>% as.character %>% as.Date(format=temp_date_format) %>% 
            format("%Y%m%d") %>% as.numeric -> temp_bhav$TIMESTAMP
          # End of Re-formatting Date
          
          temp_bhav %>% select(EXPIRY_DT) %>% unlist %>% as.numeric %>% unique -> temp_bhav_EXP_unique
          
          {which(temp_bhav_EXP_unique < temp_most_recent_expiry)} %>% 
            temp_bhav_EXP_unique[.] -> temp_bhav_EXP_unique
          
          if(length(temp_bhav_EXP_unique) > 0)
          {
            temp_bhav_EXP_unique %>% c(old_expiries,.) %>% unique %>% sort %>% rev -> old_expiries
            
            if(!First_Detect)
            {
              First_Detect <- TRUE
              First_counter <- counter
            } # End of 'if(!First_Detect)'
            
          } # End of 'if(length(temp_bhav_EXP_unique) > 0)'
          
        } # End of 'if(nrow(temp_bhav) > 0)'
        
        counter <- counter - Step_Counter_Expiries
        
        Breaker_Check <- FALSE
        
        if(counter < 0)
        {
          counter <- 1
          Breaker_Check <- TRUE
        } # End of 'if(counter < 0)'
        
        if(Breaker_Check)
        {
          break
        } # End of 'if(Breaker_Check)'
      } # End of 'while(length(old_expiries) < 2 & counter > 0)'
      Last_counter <- counter
      # End of finding all 6 Expiries
      
      # Filling 
      i_relevant %>% paste0(.," out of ", length(FnO_Bhav_Database_Files_relevant), sep="") %>% 
        paste(., ", for '", temp_Output_Filename,"', Filling data for 6 prev expiries",sep='') %>% print
      
      if(length(old_expiries) > 0 & !is.na(Last_counter) & !is.na(First_counter))
      {
        # Modifying First & Last entry for looping over 'FnO_Bhav_Database_Files_relevant'
        # Looping over 'FnO_Bhav_Database_Files_relevant' is in reverse order !!!
        First_limit <- min(i_relevant, (First_counter + Step_Counter_Expiries + 1))
        End_limit <- max(1,(Last_counter - (25*3)))
        
        # initialising variables to record 
        rep(-Inf,nrow(temp_output_df)) -> temp_Peak_OI_in_Prev_Expiry
        rep(NA,nrow(temp_output_df)) -> temp_Date_Peak_OI_in_Prev_Expiry
        rep(NA,nrow(temp_output_df)) -> temp_CLOSE_PR_Peak_OI_in_Prev_Expiry
        rep(NA,nrow(temp_output_df)) -> temp_VAL_INLAKH_Peak_OI_in_Prev_Expiry
        rep(NA,nrow(temp_output_df)) -> temp_Expiry_during_Peak_OI_in_Prev_Expiry
        
        rep(-Inf,nrow(temp_output_df)) -> temp_Peak_OI_in_last_2_expiries
        rep(NA,nrow(temp_output_df)) -> temp_Date_Peak_OI_last_2_expiries
        rep(NA,nrow(temp_output_df)) -> temp_CLOSE_PR_Peak_OI_last_2_expiries
        rep(NA,nrow(temp_output_df)) -> temp_VAL_INLAKH_Peak_OI_last_2_expiries
        rep(NA,nrow(temp_output_df)) -> temp_Expiry_during_Peak_OI_last_2_expiries
        
        rep(-Inf,nrow(temp_output_df)) -> temp_Peak_OI_in_last_6_expiries
        rep(NA,nrow(temp_output_df)) -> temp_Date_Peak_OI_last_6_expiries
        rep(NA,nrow(temp_output_df)) -> temp_CLOSE_PR_Peak_OI_last_6_expiries
        rep(NA,nrow(temp_output_df)) -> temp_VAL_INLAKH_Peak_OI_last_6_expiries
        rep(NA,nrow(temp_output_df)) -> temp_Expiry_during_Peak_OI_last_6_expiries
        
        # i_limit <- First_limit; i_limit <- First_limit - Step_Counter_Expiries - 1
        for(i_limit in First_limit:End_limit)
        {
          i_relevant %>% paste0(.," out of ", length(FnO_Bhav_Database_Files_relevant), sep="") %>% 
            paste(., ", for '", temp_Output_Filename,sep='') %>% 
            paste(.,", ",i_limit, " to limit ",End_limit,sep='') %>% print
          
          # Loading  bhav copy file
          FnO_Bhav_Database %>% setwd
          i_limit %>% FnO_Bhav_Database_Files_relevant[.] %>% read.csv %>% 
            .[,1:length(temp_FnO_colnames)] %>% data.frame(stringsAsFactors = F) %>% 
            `colnames<-`(temp_FnO_colnames) %>% 
            filter(INSTRUMENT == "OPTSTK") -> temp_bhav
          
          # Re-formatting Date
          temp_bhav[1,"EXPIRY_DT"] %>% as.character -> temp_sample_date
          temp_date_format <- c()
          if(nchar(temp_sample_date) >= 11)
          {
            temp_date_format <- c("%d-%b-%Y")
          }else
          {
            temp_date_format <- c("%d-%b-%y")
          } # End of 'if(nchar(temp_sample_date) >= 11)'
          
          temp_bhav %>% select(EXPIRY_DT) %>% unlist %>% as.character %>% as.Date(format=temp_date_format) %>% 
            format("%Y%m%d") %>% as.numeric -> temp_bhav$EXPIRY_DT
          
          temp_bhav %>% select(TIMESTAMP) %>% unlist %>% as.character %>% as.Date(format=temp_date_format) %>% 
            format("%Y%m%d") %>% as.numeric -> temp_bhav$TIMESTAMP
          # End of Re-formatting Date
          
          # Filling data for expiries
          # i_temp_output_df <- 1
          for(i_temp_output_df in 1:nrow(temp_output_df))
          {
            i_relevant %>% paste0(.," out of ", length(FnO_Bhav_Database_Files_relevant), sep="") %>% 
              paste(., ", for '", temp_Output_Filename,sep='') %>% 
              paste(.,", ",i_limit, " to limit ",End_limit,sep='') %>% 
              paste(.,"', All Expiry, ",sep='') %>% 
              paste(.,i_temp_output_df, " out of ",nrow(temp_output_df),sep='') %>% print
            
            i_temp_output_df %>% temp_output_df[.,"SYMBOL"] -> temp_SYMBOL
            i_temp_output_df %>% temp_output_df[.,"STRIKE_PR"] -> temp_STRIKE_PR
            i_temp_output_df %>% temp_output_df[.,"OPEN_INT"] -> temp_today_OPEN_INT
            i_temp_output_df %>% temp_output_df[.,"OPTIONTYPE"] -> temp_OPTIONTYPE
            
            temp_bhav %>% filter(INSTRUMENT == "OPTSTK") %>% filter(SYMBOL == temp_SYMBOL) %>% 
              filter(STRIKE_PR == temp_STRIKE_PR) %>% 
              filter(OPTIONTYPE == temp_OPTIONTYPE) -> temp_bhav_filtered
            
            # Updating for 'Prev_Expiry'
            if(length(old_expiries) > 0)
            {
              1 %>% old_expiries[.] -> temp_expiry
              
              temp_bhav_filtered %>% filter(EXPIRY_DT == temp_expiry) -> temp_temp_bhav_filtered_expiry
              
              if(nrow(temp_temp_bhav_filtered_expiry) > 0)
              {
                temp_temp_bhav_filtered_expiry[1,] -> temp_temp_bhav_filtered_expiry
                temp_temp_bhav_filtered_expiry$OPEN_INT -> temp_temp_OI
                
                if(temp_temp_OI > temp_Peak_OI_in_Prev_Expiry[i_temp_output_df])
                {
                  # Filling: temp_Peak_OI_in_Prev_Expiry, temp_Date_Peak_OI_in_Prev_Expiry,
                  # temp_CLOSE_PR_Peak_OI_in_Prev_Expiry, temp_VAL_INLAKH_Peak_OI_in_Prev_Expiry,
                  # temp_Expiry_during_Peak_OI_in_Prev_Expiry
                  
                  temp_temp_OI -> temp_Peak_OI_in_Prev_Expiry[i_temp_output_df]
                  temp_temp_bhav_filtered_expiry$TIMESTAMP -> temp_Date_Peak_OI_in_Prev_Expiry[i_temp_output_df]
                  temp_temp_bhav_filtered_expiry$CLOSE -> temp_CLOSE_PR_Peak_OI_in_Prev_Expiry[i_temp_output_df]
                  temp_temp_bhav_filtered_expiry$VAL_INLAKH -> temp_VAL_INLAKH_Peak_OI_in_Prev_Expiry[i_temp_output_df]
                  temp_temp_bhav_filtered_expiry$EXPIRY_DT -> temp_Expiry_during_Peak_OI_in_Prev_Expiry[i_temp_output_df]
                  
                } # End of 'if(temp_temp_OI > temp_Peak_OI_in_Prev_Expiry[i_temp_output_df])'
                
              } # End of 'if(nrow(temp_temp_bhav_filtered_expiry) > 0)'
            } # End of 'if(length(old_expiries) > 0)'
            
            # Updating for 'last_2_expiries'
            if(length(old_expiries) > 0)
            {
              min(2,length(old_expiries)) -> old_expiries_seq_limit
              1 %>% seq(.,old_expiries_seq_limit,1) %>% old_expiries[.] -> temp_expiry
              
              temp_bhav_filtered %>% select(EXPIRY_DT) %>% unlist %>% as.numeric %>% 
                {which(. %in% temp_expiry)} %>% temp_bhav_filtered[.,] -> temp_temp_bhav_filtered_expiry
              
              temp_temp_bhav_filtered_expiry %>% select(OPEN_INT) %>% unlist %>% as.numeric %>% 
                which.max %>% temp_temp_bhav_filtered_expiry[.,] -> temp_temp_bhav_filtered_expiry
              
              if(nrow(temp_temp_bhav_filtered_expiry) > 0)
              {
                temp_temp_bhav_filtered_expiry[1,] -> temp_temp_bhav_filtered_expiry
                temp_temp_bhav_filtered_expiry$OPEN_INT -> temp_temp_OI
                
                if(temp_temp_OI > temp_Peak_OI_in_last_2_expiries[i_temp_output_df])
                {
                  # Filling: temp_Peak_OI_in_last_2_expiries, temp_Date_Peak_OI_last_2_expiries,
                  # temp_CLOSE_PR_Peak_OI_last_2_expiries, temp_VAL_INLAKH_Peak_OI_last_2_expiries,
                  # temp_Expiry_during_Peak_OI_last_2_expiries
                  
                  temp_temp_OI -> temp_Peak_OI_in_last_2_expiries[i_temp_output_df]
                  temp_temp_bhav_filtered_expiry$TIMESTAMP -> temp_Date_Peak_OI_last_2_expiries[i_temp_output_df]
                  temp_temp_bhav_filtered_expiry$CLOSE -> temp_CLOSE_PR_Peak_OI_last_2_expiries[i_temp_output_df]
                  temp_temp_bhav_filtered_expiry$VAL_INLAKH -> temp_VAL_INLAKH_Peak_OI_last_2_expiries[i_temp_output_df]
                  temp_temp_bhav_filtered_expiry$EXPIRY_DT -> temp_Expiry_during_Peak_OI_last_2_expiries[i_temp_output_df]
                  
                } # End of 'if(temp_temp_OI > temp_Peak_OI_in_last_2_expiries[i_temp_output_df])'
                
              } # End of 'if(nrow(temp_temp_bhav_filtered_expiry) > 0)'
            } # End of 'if(length(old_expiries) > 0)'
            
            # Updating for 'last_6_expiries'
            if(length(old_expiries) > 0)
            {
              min(6,length(old_expiries)) -> old_expiries_seq_limit
              1 %>% seq(.,old_expiries_seq_limit,1) %>% old_expiries[.] -> temp_expiry
              
              temp_bhav_filtered %>% select(EXPIRY_DT) %>% unlist %>% as.numeric %>% 
                {which(. %in% temp_expiry)} %>% temp_bhav_filtered[.,] -> temp_temp_bhav_filtered_expiry
              
              temp_temp_bhav_filtered_expiry %>% select(OPEN_INT) %>% unlist %>% as.numeric %>% 
                which.max %>% temp_temp_bhav_filtered_expiry[.,] -> temp_temp_bhav_filtered_expiry
              
              if(nrow(temp_temp_bhav_filtered_expiry) > 0)
              {
                temp_temp_bhav_filtered_expiry[1,] -> temp_temp_bhav_filtered_expiry
                temp_temp_bhav_filtered_expiry$OPEN_INT -> temp_temp_OI
                
                if(temp_temp_OI > temp_Peak_OI_in_last_2_expiries[i_temp_output_df] &
                   exists("temp_Peak_OI_in_last_6_expiries"))
                {
                  # Filling: temp_Peak_OI_in_last_6_expiries, temp_Date_Peak_OI_last_6_expiries,
                  # temp_CLOSE_PR_Peak_OI_last_6_expiries, temp_VAL_INLAKH_Peak_OI_last_6_expiries,
                  # temp_Expiry_during_Peak_OI_last_6_expiries
                  
                  temp_temp_OI -> temp_Peak_OI_in_last_6_expiries[i_temp_output_df]
                  temp_temp_bhav_filtered_expiry$TIMESTAMP -> temp_Date_Peak_OI_last_6_expiries[i_temp_output_df]
                  temp_temp_bhav_filtered_expiry$CLOSE -> temp_CLOSE_PR_Peak_OI_last_6_expiries[i_temp_output_df]
                  temp_temp_bhav_filtered_expiry$VAL_INLAKH -> temp_VAL_INLAKH_Peak_OI_last_6_expiries[i_temp_output_df]
                  temp_temp_bhav_filtered_expiry$EXPIRY_DT -> temp_Expiry_during_Peak_OI_last_6_expiries[i_temp_output_df]
                  
                } # End of 'if(temp_temp_OI > temp_Peak_OI_in_last_6_expiries[i_temp_output_df])'
                
              } # End of 'if(nrow(temp_temp_bhav_filtered_expiry) > 0)'
            } # End of 'if(length(old_expiries) > 0)'
            
            # Updating for 'last_6_expiries'
            
          } # End of 'for(i_fill in 1:nrow(temp_output_df))'
          
        } # End of 'for(i_limit in First_limit:End_limit)'
        
      } # End of 'if(length(old_expiries) > 0 & !is.na(Last_counter) & !is.na(First_counter))'
      
      # Finalysing variables to temp_output_df
      
      # Replacing '-Inf' with NA 
      if(exists("temp_Peak_OI_in_last_6_expiries"))
      {
        temp_Peak_OI_in_last_6_expiries %>% na_if(.,-Inf) -> temp_Peak_OI_in_last_6_expiries
        temp_output_df$Peak_OI_in_last_6_expiries <- temp_Peak_OI_in_last_6_expiries
      } # End of 'if(exists("temp_Peak_OI_in_last_6_expiries"))'
      
      if(exists("temp_Peak_OI_in_last_2_expiries"))
      {
        temp_Peak_OI_in_last_2_expiries %>% na_if(.,-Inf) -> temp_Peak_OI_in_last_2_expiries
        temp_output_df$Peak_OI_in_last_2_expiries <- temp_Peak_OI_in_last_2_expiries
      } # End of 'if(exists("temp_Peak_OI_in_last_2_expiries"))'
      
      if(exists("temp_Peak_OI_in_Prev_Expiry"))
      {
        temp_Peak_OI_in_Prev_Expiry %>% na_if(.,-Inf) -> temp_Peak_OI_in_Prev_Expiry
        temp_output_df$Peak_OI_in_Prev_Expiry <- temp_Peak_OI_in_Prev_Expiry
      } # End of 'if(exists("temp_Peak_OI_in_Prev_Expiry"))'
      
      if(exists("temp_Date_Peak_OI_in_Prev_Expiry"))
      {
        temp_output_df$Date_Peak_OI_in_Prev_Expiry <- temp_Date_Peak_OI_in_Prev_Expiry 
      } # End of 'if(exists("temp_Date_Peak_OI_in_Prev_Expiry"))'
      
      if(exists("temp_CLOSE_PR_Peak_OI_in_Prev_Expiry"))
      {
        temp_output_df$CLOSE_PR_Peak_OI_in_Prev_Expiry <- temp_CLOSE_PR_Peak_OI_in_Prev_Expiry
      } # End of 'if(exists("temp_CLOSE_PR_Peak_OI_in_Prev_Expiry"))'
      
      if(exists("temp_VAL_INLAKH_Peak_OI_in_Prev_Expiry"))
      {
        temp_output_df$VAL_INLAKH_Peak_OI_in_Prev_Expiry <- temp_VAL_INLAKH_Peak_OI_in_Prev_Expiry
      } # End of 'if(exists("temp_VAL_INLAKH_Peak_OI_in_Prev_Expiry"))'
      
      if(exists("temp_Expiry_during_Peak_OI_in_Prev_Expiry"))
      {
        temp_output_df$Expiry_during_Peak_OI_in_Prev_Expiry <- temp_Expiry_during_Peak_OI_in_Prev_Expiry
      } # End of 'if(exists("temp_Expiry_during_Peak_OI_in_Prev_Expiry"))'
      
      if(exists("temp_Date_Peak_OI_last_2_expiries"))
      {
        temp_output_df$Date_Peak_OI_in_last_2_expiries <- temp_Date_Peak_OI_last_2_expiries
      } # End of 'if(exists("temp_Date_Peak_OI_last_2_expiries"))'
      
      if(exists("temp_CLOSE_PR_Peak_OI_last_2_expiries"))
      {
        temp_output_df$CLOSE_PR_Peak_OI_in_last_2_expiries <- temp_CLOSE_PR_Peak_OI_last_2_expiries
      } # End of 'if(exists("temp_CLOSE_PR_Peak_OI_last_2_expiries"))'
      
      if(exists("temp_VAL_INLAKH_Peak_OI_last_2_expiries"))
      {
        temp_output_df$VAL_INLAKH_Peak_OI_in_last_2_expiries <- temp_VAL_INLAKH_Peak_OI_last_2_expiries
      } # End of 'if(exists("temp_VAL_INLAKH_Peak_OI_last_2_expiries"))'
      
      if(exists("temp_Expiry_during_Peak_OI_last_2_expiries"))
      {
        temp_output_df$Expiry_during_Peak_OI_in_last_2_expiries <- temp_Expiry_during_Peak_OI_last_2_expiries
      } # End of 'if(exists("temp_Expiry_during_Peak_OI_last_2_expiries"))'
      
      if(exists("temp_Date_Peak_OI_last_6_expiries"))
      {
        temp_output_df$Date_Peak_OI_in_last_6_expiries <- temp_Date_Peak_OI_last_6_expiries
      } # End of 'if(exists("temp_Date_Peak_OI_last_6_expiries"))'
      
      if(exists("temp_CLOSE_PR_Peak_OI_last_6_expiries"))
      {
        temp_output_df$CLOSE_PR_Peak_OI_in_last_6_expiries <- temp_CLOSE_PR_Peak_OI_last_6_expiries
      } # End of 'if(exists("temp_CLOSE_PR_Peak_OI_last_6_expiries"))'
      
      if(exists("temp_VAL_INLAKH_Peak_OI_last_6_expiries"))
      {
        temp_output_df$VAL_INLAKH_Peak_OI_in_last_6_expiries <- temp_VAL_INLAKH_Peak_OI_last_6_expiries
      } # End of 'if(exists("temp_VAL_INLAKH_Peak_OI_last_6_expiries"))'
      
      if(exists("temp_Expiry_during_Peak_OI_last_6_expiries"))
      {
        temp_output_df$Expiry_during_Peak_OI_in_last_6_expiries <- temp_Expiry_during_Peak_OI_last_6_expiries
      } # End of 'if(exists("temp_Expiry_during_Peak_OI_last_6_expiries"))'
      
      # Save
      Output_Folder %>% setwd
      temp_output_df %>% write.csv(.,file=temp_Output_Filename,na = "",row.names = FALSE)
    }else{
      i_relevant %>% paste0(.," out of ", length(FnO_Bhav_Database_Files_relevant), sep="") %>% 
        paste(., " '", temp_Output_Filename,"' already present",sep='') %>% print
    } # End of 'if(temp_Output_Filename %in% Output_Folder_Files)'
    
  } # End of 'for (i_relevant in 1:length(FnO_Bhav_Database_Files_relevant))'
}else{
  "No relevant FnO_Bhav_Database_Files found" %>% print
} # End of 'if(length(FnO_Bhav_Database_Files_relevant) > 0)'

#################################################################################
#################################################################################