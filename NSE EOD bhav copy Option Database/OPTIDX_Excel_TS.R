#################################################################################
#################################### Goals ######################################

# Date: 2020-June-11
# Author: Arunabha Sarkar

# Goals: Using files in 'OPTIDX_TS', make excel file for drag and drop Option Strategy
# File Name: OPTIDX_Excel_TS

#################################################################################
#################################################################################

#################################################################################
##################### Initializing and loading Libraries ########################

library(dplyr)
library(purrr)
library(openxlsx)

#################################################################################
#################################################################################

#################################################################################
############################### Set Directories #################################

"C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% paste0("/Nifty 50 Historical Data Investing dot com",
                            sep='') -> Nifty50_Database  # To find out which day is open & NIFTY Spot

Central_Database %>% paste0("/NSE EQ Bhav Copies",sep='') -> EQ_Bhav_Database # To add spot value

Central_Database %>% paste0("/NSE FnO Bhav Copies",sep='') -> FnO_Bhav_Database # To get the raw data

Central_Database %>% paste0("/OPTIDX_TS",sep='') -> OPTIDX_TS_Database

Central_Database %>% paste0("/BANKNIFTY 50 Historical Data Investing dot com",sep='') -> BankNifty_Database

Central_Database %>% setwd

if(!file.exists('OPTIDX_Excel_TS'))
{
  print("Creating 'OPTIDX_Excel_TS' directory.")
  dir.create(file.path(Central_Database, 'OPTIDX_Excel_TS'))
}else{
  print("'OPTIDX_Excel_TS' directory already exists.")
} # End of 'if(!file.exists('OPTIDX_Excel_TS'))'

Central_Database %>% paste0("/OPTIDX_Excel_TS",sep='') -> OPTIDX_Excel_TS_Database

#################################################################################
#################################################################################

#################################################################################
############################## Hyper parameters #################################

MAX_Expiry_Trials <- 20

MAX_Strike_p_Trials <- 40

temp_FnO_colnames <- c("INSTRUMENT","SYMBOL","EXPIRY_DT","STRIKE_PR","OPTIONTYPE",
                       "OPEN","HIGH","LOW","CLOSE","SETTLE_PR","CONTRACTS","VAL_INLAKH",
                       "OPEN_INT","CHG_IN_OI","TIMESTAMP")

temp_EQ_colnames <- c("SYMBOL","SERIES","OPEN","HIGH","LOW","CLOSE","LAST","PREVCLOSE",
                      "TOTTRDQTY","TOTTRDVAL","TIMESTAMP")

#################################################################################
#################################################################################

#################################################################################
############################# Finding Latest Date ###############################

# Find latest date of NIFTY50
Nifty50_Database %>% setwd

list.files() %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>% 
  as.Date(format="%Y%m%d") %>% which.max %>% list.files()[.] %>% 
  read.csv %>% {. ->> Nifty_Data} %>% select(Date) %>% unlist %>% as.character %>% 
  as.Date(format="%b %d, %Y") %>% max %>% format("%Y%m%d") %>% as.numeric -> Max_Nifty_Date

colnames(Nifty_Data) <- c("Date","Last","Open","High","Low","Volume","Change")

Nifty_Data %>% select(Date) %>% unlist %>% as.character %>% as.Date(format("%b %d, %Y")) %>% 
  format("%Y%m%d") %>% as.numeric -> Nifty_Data$Date

Nifty_Data$Date %>% unlist %>% as.numeric %>% sort -> N50_Dates_YYYYMMDD

#################################################################################
#################################################################################

#################################################################################
########################## Making OPTIDX_Excel files ############################

OPTIDX_TS_Database %>% setwd
list.files() -> OPTIDX_TS_Database_Files

if (length(OPTIDX_TS_Database_Files) > 0)
{
  print(paste0("Found '", length(OPTIDX_TS_Database_Files), "' files in 'OPTIDX_TS_Database'",sep=''))
  
  # i_1 <- 1
  for (i_1 in 1: length(OPTIDX_TS_Database_Files))
  {
    if(i_1 > 2)
    {
      break
    } # End of 'if(i_1 > 1)'
    
    OPTIDX_TS_Database %>% setwd
    
    # Need top tackle existing file and new file case
    Old_Excel_Exists <- FALSE
    Overwrite_Required <- FALSE
    
    i_1 %>% OPTIDX_TS_Database_Files[.] -> temp_Options_csv_filename
    
    temp_Options_csv_filename %>% strsplit(split='_') %>% .[[1]] %>% .[1] -> temp_Symbol
    
    # Checking if previous excel output exists
    OPTIDX_Excel_TS_Database %>% setwd
    
    temp_Symbol %>% paste0(.,"_Excel_TS.xlsx",sep='') -> temp_Options_excel_filename
    OPTIDX_Excel_TS_Database %>% setwd
    list.files() -> OPTIDX_Excel_TS_Database_Files
    
    if(temp_Options_excel_filename %in% OPTIDX_Excel_TS_Database_Files)
    {
      Old_Excel_Exists <- TRUE
      print(paste0("Prior Excel file found for ", temp_Symbol,sep=''))
    }else{
      Old_Excel_Exists <- FALSE
      print(paste0("NO prior Excel file for ", temp_Symbol,sep=''))
    } # End of 'if(temp_Options_excel_filename %in% OPTIDX_Excel_TS_Database_Files)'
    
    if(Old_Excel_Exists)
    {
      # Find relevant new rows & Option Chain
      temp_Options_excel_filename %>% strsplit(split="_") %>% .[[1]] %>% .[1] -> temp_Symbol_from_filename
      
      temp_Symbol_from_filename %>% gsub("&","_and_",x=.) -> temp_Symbol_in_sheet_names
      
      # Finding latest date in sheet and comparing to 'Max_Nifty_Date'
      OPTIDX_Excel_TS_Database %>% setwd
      
      temp_Options_excel_filename %>% read.xlsx(sheet = 1) %>% select(Date_YYYYMMDD_C) %>% unlist %>% 
        max -> temp_old_file_max_date
      
      if (temp_old_file_max_date >= as.numeric(Max_Nifty_Date))
      {
        # Up to date, no update required
        Old_Excel_Exists <- TRUE
        Overwrite_Required <- FALSE
      }else{
        # Not up to date, update is required
        Old_Excel_Exists <- TRUE
        Overwrite_Required <- TRUE
        
        # Finding number of expiries from 'OPTIDX_TS_Database' database
        OPTIDX_TS_Database %>% setwd
        
        temp_Symbol_from_filename %>% paste0(.,"_Options.csv",sep='') %>% read.csv %T>% 
          {. ->> temp_Symbol_raw_csv} %>% colnames -> temp_Symbol_raw_csv_colnames
        
        expiry_counter <- MAX_Expiry_Trials
        Found_Expiry_limit <- FALSE
        while (!Found_Expiry_limit)
        {
          expiry_trial <- grep(paste0("_Expiry_",expiry_counter,"_",sep=''),temp_Symbol_raw_csv_colnames)
          if (length(expiry_trial) > 0 & expiry_counter > 0)
          {
            Found_Expiry_limit <- TRUE
          } # End of 'if (length(expiry_trial) > 0)'
          
          if (Found_Expiry_limit)
          {
            break
          } # End of 'if (Found_Expiry_limit)'
          expiry_counter <- expiry_counter - 1
          
          if (expiry_counter < 0)
          {
            print(paste0("Check source csv file: '", temp_Symbol_from_filename, "_Options.csv",
                         "', no expiry column found",sep=""))
            break
          } # End of 'if (expiry_counter < 0)'
        } # End of 'while (!Found_Expiry_limit)'
        
        if (expiry_counter > 0 & Found_Expiry_limit)
        {
          "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
            paste0(" found ", expiry_counter, " expiries",sep='') %>% print
          
          # From 'expiry_counter', finding out sheet number of ATMs, expiries & other sheets
          1 -> temp_sheet_num_CE_ATM
          temp_sheet_num_CE_ATM %>% {(.+1):(.+expiry_counter)} -> temp_sheet_num_CE_expiries
          temp_sheet_num_CE_expiries %>% max %>% {.+1} -> temp_sheet_num_PE_ATM
          temp_sheet_num_PE_ATM %>% {(.+1):(.+expiry_counter)} -> temp_sheet_num_PE_expiries
          temp_sheet_num_PE_expiries %>% max %>% {.+1} -> temp_sheet_num_EQ
          temp_sheet_num_EQ %>% {.+1} -> temp_sheet_num_F
          temp_sheet_num_F %>% {.+1} -> temp_sheet_num_O_CE
          temp_sheet_num_O_CE %>% {.+1} -> temp_sheet_num_O_PE
          
          # Constructing output Excel Sheet:
          temp_Symbol_Output_excel <- createWorkbook()
          
          ################## Tackling 'temp_sheet_num_CE_ATM'
          "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
            paste0(" updating CE ATM",sep='') %>% print
          
          Symbol_CE_ATM_Sheetname <- paste0(temp_Symbol_in_sheet_names,"_CE_ATM",sep='')
          addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_CE_ATM_Sheetname)
          
          OPTIDX_Excel_TS_Database %>% setwd
          
          temp_Options_excel_filename %>% read.xlsx(sheet = temp_sheet_num_CE_ATM) -> temp_Old_CE_ATM_sheet
          
          temp_Old_CE_ATM_sheet[,1] %>% unlist %>% as.numeric -> temp_Old_CE_ATM_sheet_dates
          temp_Old_CE_ATM_sheet_dates %>% min -> temp_Old_CE_ATM_sheet_dates_min
          
          which(N50_Dates_YYYYMMDD >= temp_Old_CE_ATM_sheet_dates_min) %>% 
            N50_Dates_YYYYMMDD[.] -> temp_relevant_N50_Dates_YYYYMMDD
          
          temp_relevant_N50_Dates_YYYYMMDD %>% {. %in% temp_Old_CE_ATM_sheet_dates} %>% {!.} %>% 
            temp_relevant_N50_Dates_YYYYMMDD[.] -> temp_missing_dates_temp_sheet_num_CE_ATM
          
          temp_Old_CE_ATM_sheet %>% colnames -> temp_correct_colnames
          
          temp_correct_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
            `colnames<-`(temp_correct_colnames) -> temp_new_CE_ATM
          
          if (length(temp_missing_dates_temp_sheet_num_CE_ATM) > 0)
          {
            temp_correct_colnames <- colnames(temp_Old_CE_ATM_sheet)
            
            # i_temp_missing_dates_temp_sheet_num_CE_ATM <- 1
            for (i_temp_missing_dates_temp_sheet_num_CE_ATM in 1:length(temp_missing_dates_temp_sheet_num_CE_ATM)) 
            {
              temp_correct_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
                `colnames<-`(temp_correct_colnames) -> temp_temp_new_CE_ATM
              
              i_temp_missing_dates_temp_sheet_num_CE_ATM %>% 
                temp_missing_dates_temp_sheet_num_CE_ATM[.] %>% 
                {which(temp_Symbol_raw_csv$Date_YYYYMMDD_C == .)} %>% 
                temp_Symbol_raw_csv[.,temp_correct_colnames] -> temp_temp_new_CE_ATM
              
              if(!is.na(temp_temp_new_CE_ATM[1,1]))
              {
                temp_temp_new_CE_ATM %>% rbind(temp_new_CE_ATM,.) -> temp_new_CE_ATM
              } # End of 'if(!is.na(temp_temp_new_CE_ATM[1,1]))'
            } # End of 'for (i_temp_missing_dates_temp_sheet_num_CE_ATM in 1:length(temp_missing_dates_temp_sheet_num_CE_ATM))'
          } # End of 'if (length(temp_missing_dates_temp_sheet_num_CE_ATM) > 0)'
          
          temp_new_CE_ATM %>% rbind(temp_Old_CE_ATM_sheet,.) -> temp_new_CE_ATM
          
          writeData(temp_Symbol_Output_excel, sheet = Symbol_CE_ATM_Sheetname, x = temp_new_CE_ATM)
          ################## 
          
          ################## Tackling 'temp_sheet_num_CE_expiries'
          
          # i_temp_sheet_num_CE_expiries <- 1
          for (i_temp_sheet_num_CE_expiries in 1:length(temp_sheet_num_CE_expiries))
          {
            "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
              paste0(" updating CE Expiry #: ", i_temp_sheet_num_CE_expiries,
                     " of ",length(temp_sheet_num_CE_expiries),sep='') %>% print
            
            # For each expiry, there is % difference based strike, both plus and minus
            Symbol_CE_Expiry_Sheetname <- paste0(temp_Symbol_in_sheet_names,"_CE_Expiry_",
                                                 i_temp_sheet_num_CE_expiries,sep='')
            
            addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_CE_Expiry_Sheetname)
            
            OPTIDX_Excel_TS_Database %>% setwd
            
            temp_Options_excel_filename %>% 
              read.xlsx(sheet = temp_sheet_num_CE_expiries[i_temp_sheet_num_CE_expiries]) -> temp_Old_CE_Expiry_sheet
            
            temp_Old_CE_Expiry_sheet %>% colnames -> temp_correct_colnames
            
            temp_Old_CE_Expiry_sheet[,1] %>% unlist %>% as.numeric -> temp_Old_CE_Expiry_sheet_dates
            temp_Old_CE_Expiry_sheet_dates %>% min -> temp_Old_CE_Expiry_sheet_dates_min
            
            which(N50_Dates_YYYYMMDD >= temp_Old_CE_Expiry_sheet_dates_min) %>% 
              N50_Dates_YYYYMMDD[.] -> temp_relevant_N50_Dates_YYYYMMDD
            
            temp_relevant_N50_Dates_YYYYMMDD %>% {. %in% temp_Old_CE_Expiry_sheet_dates} %>% {!.} %>% 
              temp_relevant_N50_Dates_YYYYMMDD[.] -> temp_missing_dates_temp_sheet_num_CE_Expiry
            
            temp_correct_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
              `colnames<-`(temp_correct_colnames) -> temp_new_CE_Expiry
            
            if (length(temp_missing_dates_temp_sheet_num_CE_Expiry) > 0)
            {
              temp_correct_colnames <- colnames(temp_Old_CE_Expiry_sheet)
              
              # i_temp_missing_dates_temp_sheet_num_CE_Expiry <- 1
              for (i_temp_missing_dates_temp_sheet_num_CE_Expiry in 1:length(temp_missing_dates_temp_sheet_num_CE_Expiry)) 
              {
                temp_correct_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
                  `colnames<-`(temp_correct_colnames) -> temp_temp_new_CE_Expiry
                
                i_temp_missing_dates_temp_sheet_num_CE_Expiry %>% 
                  temp_missing_dates_temp_sheet_num_CE_Expiry[.] %>% 
                  {which(temp_Symbol_raw_csv$Date_YYYYMMDD_C == .)} %>% 
                  temp_Symbol_raw_csv[.,temp_correct_colnames] -> temp_temp_new_CE_Expiry
                
                if(!is.na(temp_temp_new_CE_Expiry[1,1]))
                {
                  temp_temp_new_CE_Expiry %>% rbind(temp_new_CE_Expiry,.) -> temp_new_CE_Expiry
                } # End of 'if(!is.na(temp_temp_new_CE_Expiry[1,1]))'
              } # End of 'for (i_temp_missing_dates_temp_sheet_num_CE_Expiry in 1:length(temp_missing_dates_temp_sheet_num_CE_Expiry)) '
            } # End of 'if (length(temp_missing_dates_temp_sheet_num_CE_Expiry) > 0)'
            
            temp_new_CE_Expiry %>% rbind(temp_Old_CE_Expiry_sheet,.) -> temp_new_CE_Expiry
            
            writeData(temp_Symbol_Output_excel, sheet = Symbol_CE_Expiry_Sheetname, x = temp_new_CE_Expiry)
            
          }# End of 'for (i_temp_sheet_num_CE_expiries in 1:length(temp_sheet_num_CE_expiries))'
          ################## 
          
          ################## Tackling 'temp_sheet_num_PE_ATM'
          "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
            paste0(" updating PE ATM",sep='') %>% print
          
          Symbol_PE_ATM_Sheetname <- paste0(temp_Symbol_in_sheet_names,"_PE_ATM",sep='')
          addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_PE_ATM_Sheetname)
          
          OPTIDX_Excel_TS_Database %>% setwd
          
          temp_Options_excel_filename %>% read.xlsx(sheet = temp_sheet_num_PE_ATM) -> temp_Old_PE_ATM_sheet
          
          temp_Old_PE_ATM_sheet[,1] %>% unlist %>% as.numeric -> temp_Old_PE_ATM_sheet_dates
          temp_Old_PE_ATM_sheet_dates %>% min -> temp_Old_PE_ATM_sheet_dates_min
          
          which(N50_Dates_YYYYMMDD >= temp_Old_PE_ATM_sheet_dates_min) %>% 
            N50_Dates_YYYYMMDD[.] -> temp_relevant_N50_Dates_YYYYMMDD
          
          temp_relevant_N50_Dates_YYYYMMDD %>% {. %in% temp_Old_PE_ATM_sheet_dates} %>% {!.} %>% 
            temp_relevant_N50_Dates_YYYYMMDD[.] -> temp_missing_dates_temp_sheet_num_PE_ATM
          
          temp_Old_PE_ATM_sheet %>% colnames -> temp_correct_colnames
          
          temp_correct_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
            `colnames<-`(temp_correct_colnames) -> temp_new_PE_ATM
          
          if (length(temp_missing_dates_temp_sheet_num_PE_ATM) > 0)
          {
            temp_correct_colnames <- colnames(temp_Old_PE_ATM_sheet)
            
            # i_temp_missing_dates_temp_sheet_num_PE_ATM <- 1
            for (i_temp_missing_dates_temp_sheet_num_PE_ATM in 1:length(temp_missing_dates_temp_sheet_num_PE_ATM)) 
            {
              temp_correct_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
                `colnames<-`(temp_correct_colnames) -> temp_temp_new_PE_ATM
              
              i_temp_missing_dates_temp_sheet_num_PE_ATM %>% 
                temp_missing_dates_temp_sheet_num_PE_ATM[.] %>% 
                {which(temp_Symbol_raw_csv$Date_YYYYMMDD_C == .)} %>% 
                temp_Symbol_raw_csv[.,temp_correct_colnames] -> temp_temp_new_PE_ATM
              
              if(!is.na(temp_temp_new_PE_ATM[1,1]))
              {
                temp_temp_new_PE_ATM %>% rbind(temp_new_PE_ATM,.) -> temp_new_PE_ATM
              } # End of 'if(!is.na(temp_temp_new_PE_ATM[1,1]))'
            } # End of 'for (i_temp_missing_dates_temp_sheet_num_PE_ATM in 1:length(temp_missing_dates_temp_sheet_num_PE_ATM))'
          } # End of 'if (length(temp_missing_dates_temp_sheet_num_PE_ATM) > 0)'
          
          temp_new_PE_ATM %>% rbind(temp_Old_PE_ATM_sheet,.) -> temp_new_PE_ATM
          
          writeData(temp_Symbol_Output_excel, sheet = Symbol_PE_ATM_Sheetname, x = temp_new_PE_ATM)
          ################## 
          
          ################## Tackling 'temp_sheet_num_PE_expiries'
          
          # i_temp_sheet_num_PE_expiries <- 1
          for (i_temp_sheet_num_PE_expiries in 1:length(temp_sheet_num_PE_expiries))
          {
            "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
              paste0(" updating PE Expiry #: ", i_temp_sheet_num_PE_expiries,
                     " of ",length(temp_sheet_num_PE_expiries),sep='') %>% print
            
            # For each expiry, there is % difference based strike, both plus and minus
            Symbol_PE_Expiry_Sheetname <- paste0(temp_Symbol_in_sheet_names,"_PE_Expiry_",
                                                 i_temp_sheet_num_PE_expiries,sep='')
            
            addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_PE_Expiry_Sheetname)
            
            OPTIDX_Excel_TS_Database %>% setwd
            
            temp_Options_excel_filename %>% 
              read.xlsx(sheet = temp_sheet_num_PE_expiries[i_temp_sheet_num_PE_expiries]) -> temp_Old_PE_Expiry_sheet
            
            temp_Old_PE_Expiry_sheet %>% colnames -> temp_correct_colnames
            
            temp_Old_PE_Expiry_sheet[,1] %>% unlist %>% as.numeric -> temp_Old_PE_Expiry_sheet_dates
            temp_Old_PE_Expiry_sheet_dates %>% min -> temp_Old_PE_Expiry_sheet_dates_min
            
            which(N50_Dates_YYYYMMDD >= temp_Old_PE_Expiry_sheet_dates_min) %>% 
              N50_Dates_YYYYMMDD[.] -> temp_relevant_N50_Dates_YYYYMMDD
            
            temp_relevant_N50_Dates_YYYYMMDD %>% {. %in% temp_Old_PE_Expiry_sheet_dates} %>% {!.} %>% 
              temp_relevant_N50_Dates_YYYYMMDD[.] -> temp_missing_dates_temp_sheet_num_PE_Expiry
            
            temp_correct_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
              `colnames<-`(temp_correct_colnames) -> temp_new_PE_Expiry
            
            if (length(temp_missing_dates_temp_sheet_num_PE_Expiry) > 0)
            {
              temp_correct_colnames <- colnames(temp_Old_PE_Expiry_sheet)
              
              # i_temp_missing_dates_temp_sheet_num_PE_Expiry <- 1
              for (i_temp_missing_dates_temp_sheet_num_PE_Expiry in 1:length(temp_missing_dates_temp_sheet_num_PE_Expiry)) 
              {
                temp_correct_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
                  `colnames<-`(temp_correct_colnames) -> temp_temp_new_PE_Expiry
                
                i_temp_missing_dates_temp_sheet_num_PE_Expiry %>% 
                  temp_missing_dates_temp_sheet_num_PE_Expiry[.] %>% 
                  {which(temp_Symbol_raw_csv$Date_YYYYMMDD_C == .)} %>% 
                  temp_Symbol_raw_csv[.,temp_correct_colnames] -> temp_temp_new_PE_Expiry
                
                if(!is.na(temp_temp_new_PE_Expiry[1,1]))
                {
                  temp_temp_new_PE_Expiry %>% rbind(temp_new_PE_Expiry,.) -> temp_new_PE_Expiry
                } # End of 'if(!is.na(temp_temp_new_PE_Expiry[1,1]))'
              } # End of 'for (i_temp_missing_dates_temp_sheet_num_PE_Expiry in 1:length(temp_missing_dates_temp_sheet_num_PE_Expiry)) '
            } # End of 'if (length(temp_missing_dates_temp_sheet_num_PE_Expiry) > 0)'
            
            temp_new_PE_Expiry %>% rbind(temp_Old_PE_Expiry_sheet,.) -> temp_new_PE_Expiry
            
            writeData(temp_Symbol_Output_excel, sheet = Symbol_PE_Expiry_Sheetname, x = temp_new_PE_Expiry)
            
          }# End of 'for (i_temp_sheet_num_PE_expiries in 1:length(temp_sheet_num_PE_expiries))'
          ##################
          
          ################## Tackling 'temp_sheet_num_EQ'
          "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
            paste0(" updating EQ",sep='') %>% print
          
          Symbol_EQ_Sheetname <- paste0(temp_Symbol_in_sheet_names,"_EQ",sep='')
          addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_EQ_Sheetname)
          
          OPTIDX_Excel_TS_Database %>% setwd
          
          temp_Options_excel_filename %>% read.xlsx(sheet = temp_sheet_num_EQ) -> temp_Old_EQ_sheet
          
          temp_Old_EQ_sheet[,which(colnames(temp_Old_EQ_sheet) == "TIMESTAMP")] %>% unlist %>% 
            as.numeric -> temp_Old_EQ_sheet_dates
          temp_Old_EQ_sheet_dates %>% min -> temp_Old_EQ_sheet_dates_min
          
          which(N50_Dates_YYYYMMDD >= temp_Old_EQ_sheet_dates_min) %>% 
            N50_Dates_YYYYMMDD[.] -> temp_relevant_N50_Dates_YYYYMMDD
          
          temp_relevant_N50_Dates_YYYYMMDD %>% {. %in% temp_Old_EQ_sheet_dates} %>% {!.} %>% 
            temp_relevant_N50_Dates_YYYYMMDD[.] -> temp_missing_dates_temp_sheet_num_EQ
          
          temp_Old_EQ_sheet %>% colnames -> temp_correct_colnames
          
          temp_correct_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
            `colnames<-`(temp_correct_colnames) -> temp_new_EQ
          
          if (length(temp_missing_dates_temp_sheet_num_EQ) > 0)
          {
            temp_correct_colnames <- colnames(temp_Old_EQ_sheet)
            
            # i_temp_missing_dates_temp_sheet_num_EQ <- 1
            for (i_temp_missing_dates_temp_sheet_num_EQ in 1:length(temp_missing_dates_temp_sheet_num_EQ)) 
            {
              temp_correct_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
                `colnames<-`(temp_correct_colnames) -> temp_temp_new_EQ
              
              ##########################
              
              temp_Spot_Directory <- c()
              
              if (temp_Symbol == "BANKNIFTY")
              {
                temp_Spot_Directory <- BankNifty_Database
              }else{
                temp_Spot_Directory <- Nifty50_Database
              } # End of 'if (temp_Symbol == "BANKNIFTY")'
              
              temp_Spot_Directory %>% setwd
              
              list.files() -> temp_Spot_Directory_files
              
              temp_Spot_Directory_files %>% strsplit(split=" ") %>% lapply(., function(YY){YY[1]}) %>% unlist %>% 
                as.numeric %>% sort %>% length %>% temp_Spot_Directory_files[.] %>% read.csv -> temp_Spot_Latest
              
              temp_Spot_Latest$Date %>% unlist %>% as.character %>% as.Date(format="%b %d, %Y") %>% 
                format("%Y%m%d") %>% as.numeric -> temp_Spot_Latest$Date
              
              # Find min & max date from temp_Symbol_raw_csv
              
              temp_Symbol_raw_csv %>% select(Date_YYYYMMDD_C) %>% unlist %>% as.character %>% 
                as.Date(format="%Y%m%d") -> All_temp_Symbol_raw_csv_dates
              
              All_temp_Symbol_raw_csv_dates %>% as.Date(format="%b %d, %Y") %>% 
                format("%Y%m%d") %>% as.numeric %>% max -> All_temp_Symbol_raw_csv_dates_Max
              
              All_temp_Symbol_raw_csv_dates %>% as.Date(format="%b %d, %Y") %>% 
                format("%Y%m%d") %>% as.numeric %>% min -> All_temp_Symbol_raw_csv_dates_Min
              
              temp_Spot_Latest %>% select(Date) %>% unlist %>% as.character %>% as.numeric %>% 
                {. <= All_temp_Symbol_raw_csv_dates_Max & . >= All_temp_Symbol_raw_csv_dates_Min} %>% 
                temp_Spot_Latest[.,-c(ncol(temp_Spot_Latest))] %>% 
                `colnames<-`(c("TIMESTAMP","CLOSE","OPEN","HIGH","LOW","TOTTRDVAL")) -> temp_Spot_Latest_relevant
              
              temp_Spot_Latest_relevant %>% select(TOTTRDVAL) %>% unlist %>% as.character %>% 
                gsub("-","0",.) %>% gsub("K","*1000",.) %>% gsub("M","*1000000",.) %>% 
                gsub("B","*1000000000",.) %>% gsub("T","*1000000000000",.) %>% as.list(.) %>% 
                lapply(.,function(MM){eval(parse(text=MM))}) %>% unlist -> temp_Spot_Latest_relevant$TOTTRDVAL
              
              temp_Spot_Latest_relevant$SYMBOL <- temp_Symbol
              
              temp_Spot_Latest_relevant$SERIES <- ""
              
              temp_Spot_Latest_relevant$PREVCLOSE <- ""
              
              temp_Spot_Latest_relevant$TOTTRDQTY <- ""
              
              temp_Spot_Latest_relevant[,c(7,8,3,4,2,5,9,10,6,1)] -> temp_Spot_Latest_relevant
              
              temp_Spot_Latest_relevant[with(temp_Spot_Latest_relevant, order(TIMESTAMP)), ] -> temp_Spot_Latest_relevant
              
              ##########################
              
              if(nrow(temp_Spot_Latest_relevant) > 0)
              {
                temp_Spot_Latest_relevant -> temp_new_EQ
                
              } # End of 'if(length(temp_relevant_EQ_file) > 0)'
              
            } # End of 'for (i_temp_missing_dates_temp_sheet_num_EQ in 1:length(temp_missing_dates_temp_sheet_num_EQ)) '
          } # End of 'if (length(temp_missing_dates_temp_sheet_num_EQ) > 0)'
          
          temp_new_EQ[with(temp_new_EQ, order(TIMESTAMP)), ] -> temp_new_EQ
          
          writeData(temp_Symbol_Output_excel, sheet = Symbol_EQ_Sheetname, x = temp_new_EQ)
          ##################
          
          ################## Tackling 'temp_sheet_num_F'
          "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
            paste0(" updating F",sep='') %>% print
          
          Symbol_F_Sheetname <- paste0(temp_Symbol_in_sheet_names,"_F",sep='')
          addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_F_Sheetname)
          
          OPTIDX_Excel_TS_Database %>% setwd
          
          temp_Options_excel_filename %>% read.xlsx(sheet = temp_sheet_num_F) -> temp_Old_F_sheet
          
          temp_Old_F_sheet[,which(colnames(temp_Old_F_sheet) == "TIMESTAMP")] %>% unlist %>% 
            as.numeric -> temp_Old_F_sheet_dates
          
          temp_Old_F_sheet_dates %>% max -> temp_Old_F_sheet_dates_max
          
          which(N50_Dates_YYYYMMDD >= temp_Old_F_sheet_dates_max) %>% 
            N50_Dates_YYYYMMDD[.] -> temp_relevant_N50_Dates_YYYYMMDD
          
          temp_relevant_N50_Dates_YYYYMMDD %>% {. %in% temp_Old_F_sheet_dates} %>% {!.} %>% 
            temp_relevant_N50_Dates_YYYYMMDD[.] -> temp_missing_dates_temp_sheet_num_F
          
          temp_Old_F_sheet %>% colnames -> temp_correct_colnames
          
          temp_correct_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
            `colnames<-`(temp_correct_colnames) -> temp_new_F
          
          if (length(temp_missing_dates_temp_sheet_num_F) > 0)
          {
            temp_correct_colnames <- colnames(temp_Old_F_sheet)
            
            # i_temp_missing_dates_temp_sheet_num_F <- 1
            for (i_temp_missing_dates_temp_sheet_num_F in 1:length(temp_missing_dates_temp_sheet_num_F)) 
            {
              temp_correct_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
                `colnames<-`(temp_correct_colnames) -> temp_temp_new_F
              
              FnO_Bhav_Database %>% setwd
              
              list.files() -> All_FnO_files
              
              All_FnO_files %>% as.Date(format = "fo%d%b%Ybhav.csv") %>% 
                format("%Y%m%d") %>% as.character %>% as.numeric -> All_FnO_dates_YYYYMMDD
              
              which(All_FnO_dates_YYYYMMDD == temp_missing_dates_temp_sheet_num_F[i_temp_missing_dates_temp_sheet_num_F]) %>% 
                .[1] %>% All_FnO_files[.] -> temp_relevant_F_file
              
              if(length(temp_relevant_F_file) > 0)
              {
                temp_relevant_F_file %>% read.csv -> temp_relevant_F_file_df
                
                temp_relevant_F_file_df %>% colnames %>% {which(. == "TIMESTAMP")} %>% 
                  {1:.} %>% temp_relevant_F_file_df[,.] %>% 
                  `colnames<-`(temp_FnO_colnames) -> temp_relevant_F_file_df
                
                temp_relevant_F_file_df %>% filter(SYMBOL == temp_Symbol_from_filename) %>% 
                  filter(INSTRUMENT == "FUTIDX") -> temp_temp_new_F
                
                Correct_Date_format <- c()
                
                if(nchar(as.character(temp_temp_new_F[1,"TIMESTAMP"])) >= 10)
                {
                  Correct_Date_format <- "%d-%b-%Y"
                }else{
                  Correct_Date_format <- "%d-%b-%y"
                } # End of 'if(nchar(as.character(temp_temp_new_F[1,"TIMESTAMP"])) >= 10)'
                
                temp_temp_new_F$TIMESTAMP %>% unlist %>% as.character %>% as.Date(format=Correct_Date_format) %>% 
                  format("%Y%m%d") %>% as.numeric -> temp_temp_new_F$TIMESTAMP
                
                temp_temp_new_F$EXPIRY_DT %>% unlist %>% as.character %>% as.Date(format=Correct_Date_format) %>% 
                  format("%Y%m%d") %>% as.numeric -> temp_temp_new_F$EXPIRY_DT
                
              } # End of 'if(length(temp_relevant_F_file) > 0)'
              
              if(!is.na(temp_temp_new_F[1,1]))
              {
                temp_temp_new_F %>% rbind(temp_new_F,.) -> temp_new_F
              } # End of 'if(!is.na(temp_temp_new_F[1,1]))'
            } # End of 'for (i_temp_missing_dates_temp_sheet_num_F in 1:length(temp_missing_dates_temp_sheet_num_F))'
          } # End of 'if (length(temp_missing_dates_temp_sheet_num_F) > 0)'
          
          temp_new_F[with(temp_new_F, order(TIMESTAMP,EXPIRY_DT,OPTIONTYPE,STRIKE_PR)), ] -> temp_new_F
          
          temp_new_F %>% rbind(temp_Old_F_sheet,.) -> temp_new_F
          
          writeData(temp_Symbol_Output_excel, sheet = Symbol_F_Sheetname, x = temp_new_F)
          ##################
          
          ################## Tackling 'temp_sheet_num_O_CE'
          "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
            paste0(" updating O CE",sep='') %>% print
          
          Symbol_O_CE_Sheetname <- paste0(temp_Symbol_in_sheet_names,"_O_CE",sep='')
          addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_O_CE_Sheetname)
          
          OPTIDX_Excel_TS_Database %>% setwd
          
          temp_Options_excel_filename %>% read.xlsx(sheet = temp_sheet_num_O_CE) -> temp_Old_O_CE_sheet
          
          temp_Old_O_CE_sheet[,which(colnames(temp_Old_O_CE_sheet) == "TIMESTAMP")] %>% unlist %>% 
            as.numeric -> temp_Old_O_CE_sheet_dates
          temp_Old_O_CE_sheet_dates %>% min -> temp_Old_O_CE_sheet_dates_min
          
          which(N50_Dates_YYYYMMDD >= temp_Old_O_CE_sheet_dates_min) %>% 
            N50_Dates_YYYYMMDD[.] -> temp_relevant_N50_Dates_YYYYMMDD
          
          temp_relevant_N50_Dates_YYYYMMDD %>% {. %in% temp_Old_O_CE_sheet_dates} %>% {!.} %>% 
            temp_relevant_N50_Dates_YYYYMMDD[.] -> temp_missing_dates_temp_sheet_num_O_CE
          
          temp_Old_O_CE_sheet %>% colnames -> temp_correct_colnames
          
          temp_correct_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
            `colnames<-`(temp_correct_colnames) -> temp_new_O_CE
          
          if (length(temp_missing_dates_temp_sheet_num_O_CE) > 0)
          {
            temp_correct_colnames <- colnames(temp_Old_O_CE_sheet)
            
            # i_temp_missing_dates_temp_sheet_num_O_CE <- 1
            for (i_temp_missing_dates_temp_sheet_num_O_CE in 1:length(temp_missing_dates_temp_sheet_num_O_CE)) 
            {
              temp_correct_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
                `colnames<-`(temp_correct_colnames) -> temp_temp_new_O_CE
              
              FnO_Bhav_Database %>% setwd
              
              list.files() -> All_FnO_files
              
              All_FnO_files %>% as.Date(format = "fo%d%b%Ybhav.csv") %>% 
                format("%Y%m%d") %>% as.character %>% as.numeric -> All_FnO_dates_YYYYMMDD
              
              which(All_FnO_dates_YYYYMMDD == temp_missing_dates_temp_sheet_num_O_CE[i_temp_missing_dates_temp_sheet_num_O_CE]) %>% 
                .[1] %>% All_FnO_files[.] -> temp_relevant_O_CE_file
              
              if(length(temp_relevant_O_CE_file) > 0)
              {
                temp_relevant_O_CE_file %>% read.csv -> temp_relevant_O_CE_file_df
                
                temp_relevant_O_CE_file_df %>% colnames %>% {which(. == "TIMESTAMP")} %>% 
                  {1:.} %>% temp_relevant_O_CE_file_df[,.] %>% 
                  `colnames<-`(temp_FnO_colnames) -> temp_relevant_O_CE_file_df
                
                temp_relevant_O_CE_file_df %>% filter(SYMBOL == temp_Symbol_from_filename) %>% 
                  filter(INSTRUMENT == "OPTIDX") %>% filter(OPTIONTYPE == "CE") -> temp_temp_new_O_CE
                
                Correct_Date_format <- c()
                
                if(nchar(as.character(temp_temp_new_O_CE[1,"TIMESTAMP"])) >= 10)
                {
                  Correct_Date_format <- "%d-%b-%Y"
                }else{
                  Correct_Date_format <- "%d-%b-%y"
                } # End of 'if(nchar(as.character(temp_temp_new_O_CE[1,"TIMESTAMP"])) >= 10)'
                
                temp_temp_new_O_CE$TIMESTAMP %>% unlist %>% as.character %>% as.Date(format=Correct_Date_format) %>% 
                  format("%Y%m%d") %>% as.numeric -> temp_temp_new_O_CE$TIMESTAMP
                
                temp_temp_new_O_CE$EXPIRY_DT %>% unlist %>% as.character %>% as.Date(format=Correct_Date_format) %>% 
                  format("%Y%m%d") %>% as.numeric -> temp_temp_new_O_CE$EXPIRY_DT
                
              } # End of 'if(length(temp_relevant_O_CE_file) > 0)'
              
              if(!is.na(temp_temp_new_O_CE[1,1]))
              {
                temp_temp_new_O_CE %>% rbind(temp_new_O_CE,.) -> temp_new_O_CE
              } # End of 'if(!is.na(temp_temp_new_O_CE[1,1]))'
            } # End of 'for (i_temp_missing_dates_temp_sheet_num_O_CE in 1:length(temp_missing_dates_temp_sheet_num_O_CE))'
          } # End of 'if (length(temp_missing_dates_temp_sheet_num_O_CE) > 0)'
          
          temp_new_O_CE[with(temp_new_O_CE, order(TIMESTAMP,EXPIRY_DT,OPTIONTYPE,STRIKE_PR)), ] -> temp_new_O_CE
          
          temp_new_O_CE %>% rbind(temp_Old_O_CE_sheet,.) -> temp_new_O_CE
          
          writeData(temp_Symbol_Output_excel, sheet = Symbol_O_CE_Sheetname, x = temp_new_O_CE)
          ##################
          
          ################## Tackling 'temp_sheet_num_O_PE'
          "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
            paste0(" updating O PE",sep='') %>% print
          
          Symbol_O_PE_Sheetname <- paste0(temp_Symbol_in_sheet_names,"_O_PE",sep='')
          addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_O_PE_Sheetname)
          
          OPTIDX_Excel_TS_Database %>% setwd
          
          temp_Options_excel_filename %>% read.xlsx(sheet = temp_sheet_num_O_PE) -> temp_Old_O_PE_sheet
          
          temp_Old_O_PE_sheet[,which(colnames(temp_Old_O_PE_sheet) == "TIMESTAMP")] %>% unlist %>% 
            as.numeric -> temp_Old_O_PE_sheet_dates
          
          temp_Old_O_PE_sheet_dates %>% min -> temp_Old_O_PE_sheet_dates_min
          
          which(N50_Dates_YYYYMMDD >= temp_Old_O_PE_sheet_dates_min) %>% 
            N50_Dates_YYYYMMDD[.] -> temp_relevant_N50_Dates_YYYYMMDD
          
          temp_relevant_N50_Dates_YYYYMMDD %>% {. %in% temp_Old_O_PE_sheet_dates} %>% {!.} %>% 
            temp_relevant_N50_Dates_YYYYMMDD[.] -> temp_missing_dates_temp_sheet_num_O_PE
          
          temp_Old_O_PE_sheet %>% colnames -> temp_correct_colnames
          
          temp_correct_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
            `colnames<-`(temp_correct_colnames) -> temp_new_O_PE
          
          if (length(temp_missing_dates_temp_sheet_num_O_PE) > 0)
          {
            temp_correct_colnames <- colnames(temp_Old_O_PE_sheet)
            
            # i_temp_missing_dates_temp_sheet_num_O_PE <- 1
            for (i_temp_missing_dates_temp_sheet_num_O_PE in 1:length(temp_missing_dates_temp_sheet_num_O_PE)) 
            {
              temp_correct_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
                `colnames<-`(temp_correct_colnames) -> temp_temp_new_O_PE
              
              FnO_Bhav_Database %>% setwd
              
              list.files() -> All_FnO_files
              
              All_FnO_files %>% as.Date(format = "fo%d%b%Ybhav.csv") %>% 
                format("%Y%m%d") %>% as.character %>% as.numeric -> All_FnO_dates_YYYYMMDD
              
              which(All_FnO_dates_YYYYMMDD == temp_missing_dates_temp_sheet_num_O_CE[i_temp_missing_dates_temp_sheet_num_O_PE]) %>% 
                .[1] %>% All_FnO_files[.] -> temp_relevant_O_PE_file
              
              if(length(temp_relevant_O_PE_file) > 0)
              {
                temp_relevant_O_PE_file %>% read.csv -> temp_relevant_O_PE_file_df
                
                temp_relevant_O_PE_file_df %>% colnames %>% {which(. == "TIMESTAMP")} %>% 
                  {1:.} %>% temp_relevant_O_PE_file_df[,.] %>% 
                  `colnames<-`(temp_FnO_colnames) -> temp_relevant_O_PE_file_df
                
                temp_relevant_O_PE_file_df %>% filter(SYMBOL == temp_Symbol_from_filename) %>% 
                  filter(INSTRUMENT == "OPTIDX") %>% filter(OPTIONTYPE == "PE") -> temp_temp_new_O_PE
                
                Correct_Date_format <- c()
                
                if(nchar(as.character(temp_temp_new_O_PE[1,"TIMESTAMP"])) >= 10)
                {
                  Correct_Date_format <- "%d-%b-%Y"
                }else{
                  Correct_Date_format <- "%d-%b-%y"
                } # End of 'if(nchar(as.character(temp_temp_new_O_PE[1,"TIMESTAMP"])) >= 10)'
                
                temp_temp_new_O_PE$TIMESTAMP %>% unlist %>% as.character %>% as.Date(format=Correct_Date_format) %>% 
                  format("%Y%m%d") -> temp_temp_new_O_PE$TIMESTAMP
                
                temp_temp_new_O_PE$EXPIRY_DT %>% unlist %>% as.character %>% as.Date(format=Correct_Date_format) %>% 
                  format("%Y%m%d") -> temp_temp_new_O_PE$EXPIRY_DT
                
              } # End of 'if(length(temp_relevant_O_PE_file) > 0)'
              
              if(!is.na(temp_temp_new_O_PE[1,1]))
              {
                temp_temp_new_O_PE %>% rbind(temp_new_O_PE,.) -> temp_new_O_PE
              } # End of 'if(!is.na(temp_temp_new_O_PE[1,1]))'
            } # End of 'for (i_temp_missing_dates_temp_sheet_num_O_PE in 1:length(temp_missing_dates_temp_sheet_num_O_PE))'
          } # End of 'if (length(temp_missing_dates_temp_sheet_num_O_PE) > 0)'
          
          temp_new_O_PE[with(temp_new_O_PE, order(TIMESTAMP,EXPIRY_DT,OPTIONTYPE,STRIKE_PR)), ] -> temp_new_O_PE
          
          temp_new_O_PE %>% rbind(temp_Old_O_PE_sheet,.) -> temp_new_O_PE

          writeData(temp_Symbol_Output_excel, sheet = Symbol_O_PE_Sheetname, x = temp_new_O_PE)
          ##################
          
        } # End of 'if (expiry_counter > 0 & expiry_counter)'
        
      } # End of '(temp_old_file_max_date >= as.numeric(Max_Nifty_Date))'
      # Overwrite_Required <- FALSE # or TRUE, to be checked
    }else{
      Overwrite_Required <- FALSE # Because it is new writing
      
      # Reading old csv file
      OPTIDX_TS_Database %>% setwd
      temp_Options_csv_filename %>% read.csv -> temp_Symbol_raw_csv
      
      # Splitting into PE and CE
      temp_Symbol_raw_csv[,as.integer(1:((ncol(temp_Symbol_raw_csv))/2))] -> temp_Symbol_raw_csv_CE
      temp_Symbol_raw_csv[,((1 + ncol(temp_Symbol_raw_csv_CE)):ncol(temp_Symbol_raw_csv))] -> temp_Symbol_raw_csv_PE
      
      # Finding number of expiries
      temp_Symbol_raw_csv_PE %>% colnames -> temp_Symbol_raw_csv_PE_colnames
      temp_Symbol_raw_csv_CE %>% colnames -> temp_Symbol_raw_csv_CE_colnames
      expiry_counter <- MAX_Expiry_Trials
      Found_Expiry_limit <- FALSE
      while (!Found_Expiry_limit)
      {
        expiry_trial <- grep(paste0("_Expiry_",expiry_counter,"_",sep=''),temp_Symbol_raw_csv_PE_colnames)
        if (length(expiry_trial) > 0 & expiry_counter > 0)
        {
          Found_Expiry_limit <- TRUE
        } # End of 'if (length(expiry_trial) > 0)'
        
        if (Found_Expiry_limit)
        {
          break
        } # End of 'if (Found_Expiry_limit)'
        expiry_counter <- expiry_counter - 1
        
        if (expiry_counter < 0)
        {
          print(paste0("Check source csv file: '", temp_Options_csv_filename,"' no expiry column found",sep=""))
          break
        } # End of 'if (expiry_counter < 0)'
      } # End of 'while (!Found_Expiry_limit)'
      
      if (Found_Expiry_limit & expiry_counter > 0)
      {
        print(paste0("Found ", expiry_counter," expiries from file: '",temp_Options_csv_filename,"'",sep=""))
        
        # Constructing output Excel Sheet:
        temp_Symbol_Output_excel <- createWorkbook()
        temp_Symbol_Output_excel_sheetnames <- c()
        Symbol_CE_ATM_Sheetname <- paste0(temp_Symbol,"_CE_ATM",sep='')
        Symbol_CE_ATM_Sheetname %>% gsub("&","_and_",.) -> Symbol_CE_ATM_Sheetname
        temp_Symbol_Output_excel_sheetnames <- c(temp_Symbol_Output_excel_sheetnames,Symbol_CE_ATM_Sheetname)
        
        print(paste0("Making 'CE ATM' of ", temp_Symbol,sep=''))
        
        # Constructing CE ATM
        addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_CE_ATM_Sheetname)
        grep("_ATM",temp_Symbol_raw_csv_CE_colnames) %>% max %>% {.+1} -> temp_Symbol_raw_csv_CE_max_ATM_column
        writeData(temp_Symbol_Output_excel, sheet = Symbol_CE_ATM_Sheetname,
                  x = temp_Symbol_raw_csv_CE[,(1:temp_Symbol_raw_csv_CE_max_ATM_column)])
        
        print(paste0("Making 'CE Expiries' of ", temp_Symbol,sep=''))
        
        # Constructing CE Expiries
        # i_CE_expiry <- 1
        for(i_CE_expiry in 1:expiry_counter)
        {
          paste0(temp_Symbol,"_CE_Expiry_",i_CE_expiry,sep="") -> temp_CE_temp_expiry_sheetname
          temp_CE_temp_expiry_sheetname %>% gsub("&","_and_",.) -> temp_CE_temp_expiry_sheetname
          temp_Symbol_Output_excel_sheetnames <- c(temp_Symbol_Output_excel_sheetnames,temp_CE_temp_expiry_sheetname)
          
          i_CE_expiry %>% paste0("_CE_Expiry_",.,"_",sep='') %>% 
            grep(.,temp_Symbol_raw_csv_CE_colnames) -> temp_CE_temp_Expiry_columns
          
          which(temp_Symbol_raw_csv_CE_colnames == paste0("CE_Expiry_",i_CE_expiry,sep='')) -> temp_CE_expiry_date
          
          temp_Symbol_raw_csv_CE[,c(1,temp_CE_expiry_date,temp_CE_temp_Expiry_columns)] -> temp_CE_temp_Expiry_sheet
          
          addWorksheet(temp_Symbol_Output_excel, sheet = temp_CE_temp_expiry_sheetname)
          writeData(temp_Symbol_Output_excel, sheet = temp_CE_temp_expiry_sheetname,
                    x = temp_CE_temp_Expiry_sheet)
        } # End of 'for(i_CE_expiry in 1:expiry_counter)'
        
        print(paste0("Making 'PE ATM' of ", temp_Symbol,sep=''))
        
        # Constructing PE ATM
        Symbol_PE_ATM_Sheetname <- paste0(temp_Symbol,"_PE_ATM",sep='')
        Symbol_PE_ATM_Sheetname %>% gsub("&","_and_",.) -> Symbol_PE_ATM_Sheetname
        temp_Symbol_Output_excel_sheetnames <- c(temp_Symbol_Output_excel_sheetnames,Symbol_PE_ATM_Sheetname)
        addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_PE_ATM_Sheetname)
        grep("_ATM",temp_Symbol_raw_csv_PE_colnames) %>% max %>% {.+1} -> temp_Symbol_raw_csv_PE_max_ATM_column
        
        writeData(temp_Symbol_Output_excel, sheet = Symbol_PE_ATM_Sheetname,
                  x = temp_Symbol_raw_csv_PE[,(1:temp_Symbol_raw_csv_PE_max_ATM_column)])
        
        print(paste0("Making 'PE Expiries' of ", temp_Symbol,sep=''))
        
        # Constructing PE Expiries
        # i_PE_expiry <- 1
        for(i_PE_expiry in 1:expiry_counter)
        {
          paste0(temp_Symbol,"_PE_Expiry_",i_PE_expiry,sep="") -> temp_PE_temp_expiry_sheetname
          temp_PE_temp_expiry_sheetname %>% gsub("&","_and_",.) -> temp_PE_temp_expiry_sheetname
          temp_Symbol_Output_excel_sheetnames <- c(temp_Symbol_Output_excel_sheetnames,temp_PE_temp_expiry_sheetname)
          
          i_PE_expiry %>% paste0("_PE_Expiry_",.,"_",sep='') %>% 
            grep(.,temp_Symbol_raw_csv_PE_colnames) -> temp_PE_temp_Expiry_columns
          
          which(temp_Symbol_raw_csv_PE_colnames == paste0("PE_Expiry_",i_PE_expiry,sep='')) -> temp_PE_expiry_date
          
          temp_Symbol_raw_csv_PE[,c(1,temp_PE_expiry_date,temp_PE_temp_Expiry_columns)] -> temp_PE_temp_Expiry_sheet
          
          addWorksheet(temp_Symbol_Output_excel, sheet = temp_PE_temp_expiry_sheetname)
          writeData(temp_Symbol_Output_excel, sheet = temp_PE_temp_expiry_sheetname,
                    x = temp_PE_temp_Expiry_sheet)
        } # End of 'for(i_PE_expiry in 1:expiry_counter)'
        ############################
        # Getting EQ and FnO data for all relevant expiries on a sheet
        # Finding Min and max date from the 'temp_Symbol_raw_csv'
        # "TOTTRDVAL_SPOT_C"
        
        # Find min & max date from temp_Symbol_raw_csv
        
        temp_Symbol_raw_csv %>% select(Date_YYYYMMDD_C) %>% unlist %>% as.character %>% 
          as.Date(format="%Y%m%d") -> All_temp_Symbol_raw_csv_dates
        
        All_temp_Symbol_raw_csv_dates %>% as.Date(format="%Y-%m-%d") %>% 
          format("%Y%m%d") %>% as.numeric %>% max -> All_temp_Symbol_raw_csv_dates_Max
        
        All_temp_Symbol_raw_csv_dates %>% as.Date(format="%Y-%m-%d") %>% 
          format("%Y%m%d") %>% as.numeric %>% min -> All_temp_Symbol_raw_csv_dates_Min
        
        temp_Spot_Directory <- c()
        
        if (temp_Symbol == "BANKNIFTY")
        {
          temp_Spot_Directory <- BankNifty_Database
        }else{
          temp_Spot_Directory <- Nifty50_Database
        } # End of 'if (temp_Symbol == "BANKNIFTY")'
        
        temp_Spot_Directory %>% setwd
        
        list.files() -> temp_Spot_Directory_files
        
        temp_Spot_Directory_files %>% strsplit(split=" ") %>% lapply(., function(YY){YY[1]}) %>% unlist %>% 
          as.numeric %>% sort %>% length %>% temp_Spot_Directory_files[.] %>% read.csv -> temp_Spot_Latest
        
        temp_Spot_Latest$Date %>% unlist %>% as.character %>% as.Date(format="%b %d, %Y") %>% 
          format("%Y%m%d") %>% as.numeric -> temp_Spot_Latest$Date
        
        temp_Spot_Latest %>% select(Date) %>% unlist %>% as.character %>% as.numeric %>% 
          {. <= All_temp_Symbol_raw_csv_dates_Max & . >= All_temp_Symbol_raw_csv_dates_Min} %>% 
          temp_Spot_Latest[.,-c(ncol(temp_Spot_Latest))] %>% 
          `colnames<-`(c("TIMESTAMP","CLOSE","OPEN","HIGH","LOW","TOTTRDVAL")) -> temp_Spot_Latest_relevant
        
        temp_Spot_Latest_relevant %>% select(TOTTRDVAL) %>% unlist %>% as.character %>% 
          gsub("-","0",.) %>% gsub("K","*1000",.) %>% gsub("M","*1000000",.) %>% 
          gsub("B","*1000000000",.) %>% gsub("T","*1000000000000",.) %>% as.list(.) %>% 
          lapply(.,function(MM){eval(parse(text=MM))}) %>% unlist -> temp_Spot_Latest_relevant$TOTTRDVAL
        
        temp_Spot_Latest_relevant$SYMBOL <- temp_Symbol
        
        temp_Spot_Latest_relevant$SERIES <- ""
        
        temp_Spot_Latest_relevant$PREVCLOSE <- ""
        
        temp_Spot_Latest_relevant$TOTTRDQTY <- ""
        
        temp_Spot_Latest_relevant[,c(7,8,3,4,2,5,9,10,6,1)] -> temp_Spot_Latest_relevant

        temp_Spot_Latest_relevant[with(temp_Spot_Latest_relevant, order(TIMESTAMP)), ] -> temp_Spot_Latest_relevant
        
        if(nrow(temp_Spot_Latest_relevant) > 0)
        {
          temp_Symbol_relevant_EQ_sheetname <- paste0(temp_Symbol,"_EQ",sep='')
          temp_Symbol_relevant_EQ_sheetname %>% gsub("&","_and_",.) -> temp_Symbol_relevant_EQ_sheetname
          
          temp_Symbol_Output_excel_sheetnames <- c(temp_Symbol_Output_excel_sheetnames,temp_Symbol_relevant_EQ_sheetname)
          
          print(paste0("For Symbol ", temp_Symbol,", making new sheet: ",temp_Symbol_relevant_EQ_sheetname,sep=''))
          
          addWorksheet(temp_Symbol_Output_excel, sheet = temp_Symbol_relevant_EQ_sheetname)
          writeData(temp_Symbol_Output_excel, sheet = temp_Symbol_relevant_EQ_sheetname,
                    x = temp_Spot_Latest_relevant)
        } # End of 'if(nrow(temp_Spot_Latest_relevant) > 0)'
        ###################
        
        ###################
        
        # Finding relevant FnO separately and recoding it in a df, 
        # using All_Fno_relevant_dates, All_FnO_relevant_dates_Max & All_FnO_relevant_dates_Min
        
        temp_Symbol_raw_csv %>% select(Date_YYYYMMDD_C) %>% unlist %>% as.character %>% 
          as.Date(format="%Y%m%d") -> All_FnO_relevant_dates
        
        temp_FnO_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame(stringsAsFactors = F) %>% 
          `colnames<-`(temp_FnO_colnames) -> temp_Symbol_relevant_F
        
        temp_FnO_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame(stringsAsFactors = F) %>% 
          `colnames<-`(temp_FnO_colnames) -> temp_Symbol_relevant_O
        
        FnO_Bhav_Database %>% setwd
        
        if(length(All_FnO_relevant_dates) > 0)
        {
          FnO_Bhav_Database %>% setwd
          
          list.files() -> All_FnO_files
          
          All_FnO_relevant_dates %>% min -> All_FnO_relevant_dates_Min
          
          All_FnO_relevant_dates %>% max -> All_FnO_relevant_dates_Max
          
          All_FnO_files %>% as.Date(format="fo%d%b%Ybhav.csv") %>% {. <= All_FnO_relevant_dates_Max} %>% 
            All_FnO_files[.] %>% as.Date(format="fo%d%b%Ybhav.csv") %>% {. >= All_FnO_relevant_dates_Min} %>% 
            All_FnO_files[.] %>% unique -> All_relevant_FnO_files
          
          # Sorting All_relevant_FnO_files by date
          All_relevant_FnO_files %>% as.Date(format="fo%d%b%Ybhav.csv") -> All_relevant_FnO_dates
          
          (1:length(All_relevant_FnO_dates)) -> names(All_relevant_FnO_dates)
          
          All_relevant_FnO_dates %>% sort %>% names(.) %>% as.numeric %>% 
            All_relevant_FnO_files[.] %>% unlist %>% as.character -> All_relevant_FnO_files
          
            if (length(All_relevant_FnO_files) > 0)
            {
              # i_All_relevant_FnO_files <- 1
              for(i_All_relevant_FnO_files in 1:length(All_relevant_FnO_files))
              {
                print(paste0("For Symbol ", temp_Symbol,", reading ",i_All_relevant_FnO_files,
                             " out of ", length(All_relevant_FnO_files), " relevant FnO files.",sep=''))
                
                FnO_Bhav_Database %>% setwd
                
                i_All_relevant_FnO_files %>% All_relevant_FnO_files[.] %>% read.csv -> temp_FnO_csv
                
                which(colnames(temp_FnO_csv) == "TIMESTAMP") %>% {1:.} %>% temp_FnO_csv[,.] %>% 
                  `colnames<-`(temp_FnO_colnames)-> temp_FnO_csv
                
                temp_FnO_csv %>% filter(INSTRUMENT == "OPTIDX") %>% filter(SYMBOL == temp_Symbol) %>% 
                  filter(SYMBOL == temp_Symbol) -> temp_relevant_O_df
                
                temp_FnO_csv %>% filter(INSTRUMENT == "FUTIDX") %>% filter(SYMBOL == temp_Symbol) %>% 
                  filter(SYMBOL == temp_Symbol) -> temp_relevant_F_df
                
                if(nrow(temp_relevant_F_df) > 0 & nrow(temp_relevant_O_df) > 0)
                {
                  temp_relevant_F_df %>% rbind(temp_Symbol_relevant_F,.) -> temp_Symbol_relevant_F
                  
                  temp_relevant_O_df %>% rbind(temp_Symbol_relevant_O,.) -> temp_Symbol_relevant_O
                } # End of 'if(nrow(temp_relevant_F_df) > 0 & nrow(temp_relevant_O_df) > 0)'
              } # End of 'forfor(i_All_relevant_FnO_files in 1:length(All_relevant_FnO_files))'
            } # End of 'if (length(All_relevant_FnO_files) > 0)'
        } # End of 'if(length(All_FnO_relevant_dates) > 0)'
        ###################
        
        ################### Adding temp_Symbol_relevant_F to a sheet
        
        temp_Symbol_relevant_F %>% select(TIMESTAMP) %>% unlist %>% 
          as.character -> temp_Symbol_relevant_F$TIMESTAMP
        
        temp_Symbol_relevant_F %>% select(EXPIRY_DT) %>% unlist %>% 
          as.character -> temp_Symbol_relevant_F$EXPIRY_DT
        
        temp_Symbol_relevant_F %>% select(TIMESTAMP) %>% unlist %>% as.character %>% 
          nchar %>% unique -> temp_Symbol_relevant_F_nchar_unique
        
        if(length(temp_Symbol_relevant_F_nchar_unique) > 1)
        {
          which( (temp_Symbol_relevant_F %>% select(TIMESTAMP) %>% unlist %>% as.character %>% 
                    nchar) < 10) -> wrong_date_rows
          
          if ( length(wrong_date_rows) > 0)
          {
            wrong_date_rows %>% temp_Symbol_relevant_F[.,] %>% select(TIMESTAMP) %>% 
              unlist %>% as.character %>% as.Date(format="%d-%b-%y") %>% 
              format("%d-%b-%Y") -> temp_Symbol_relevant_F[wrong_date_rows,"TIMESTAMP"]
            
            wrong_date_rows %>% temp_Symbol_relevant_F[.,] %>% select(EXPIRY_DT) %>% 
              unlist %>% as.character %>% as.Date(format="%d-%b-%y") %>% 
              format("%d-%b-%Y") -> temp_Symbol_relevant_F[wrong_date_rows,"EXPIRY_DT"]
          } # End of 'if ( length(wrong_date_rows) > 0)'
          
        } # End of 'if(length(temp_Symbol_relevant_F_nchar_unique) > 0
        
        temp_Symbol_relevant_F %>% select(TIMESTAMP) %>% unlist %>% as.character %>% 
          as.Date(format="%d-%b-%Y") %>% format("%Y%m%d") %>% as.numeric -> temp_Symbol_relevant_F$TIMESTAMP
        
        temp_Symbol_relevant_F %>% select(EXPIRY_DT) %>% unlist %>% as.character %>% 
          as.Date(format="%d-%b-%Y") %>% format("%Y%m%d") %>% as.numeric -> temp_Symbol_relevant_F$EXPIRY_DT
        
        temp_Symbol_relevant_F[with(temp_Symbol_relevant_F, order(EXPIRY_DT,TIMESTAMP)), ] -> temp_Symbol_relevant_F
        
        if(nrow(temp_Symbol_relevant_F) > 0)
        {
          temp_Symbol_relevant_F[with(temp_Symbol_relevant_F, 
                                      order(TIMESTAMP,EXPIRY_DT,OPTIONTYPE,STRIKE_PR)), ] -> temp_Symbol_relevant_F
          
          temp_Symbol_relevant_F_sheetname <- paste0(temp_Symbol,"_F",sep='')
          temp_Symbol_relevant_F_sheetname %>% gsub("&","_and_",.) -> temp_Symbol_relevant_F_sheetname
          
          temp_Symbol_Output_excel_sheetnames <- c(temp_Symbol_Output_excel_sheetnames,temp_Symbol_relevant_F_sheetname)
          
          print(paste0("For Symbol ", temp_Symbol,", making new sheet: ",temp_Symbol_relevant_F_sheetname,sep=''))
          
          addWorksheet(temp_Symbol_Output_excel, sheet = temp_Symbol_relevant_F_sheetname)
          writeData(temp_Symbol_Output_excel, sheet = temp_Symbol_relevant_F_sheetname,
                    x = temp_Symbol_relevant_F)
        } # End of 'if(nrow(temp_Symbol_relevant_F) > 0)'
        ###################
        
        ################### Adding temp_Symbol_relevant_O to a sheet
        # temp_Symbol_relevant_O_backup <- temp_Symbol_relevant_O
        temp_Symbol_relevant_O %>% select(TIMESTAMP) %>% unlist %>% 
          as.character -> temp_Symbol_relevant_O$TIMESTAMP
        
        temp_Symbol_relevant_O %>% select(EXPIRY_DT) %>% unlist %>% 
          as.character -> temp_Symbol_relevant_O$EXPIRY_DT
        
        temp_Symbol_relevant_O %>% select(TIMESTAMP) %>% unlist %>% as.character %>% 
          nchar %>% unique -> temp_Symbol_relevant_O_nchar_unique
        
        if(length(temp_Symbol_relevant_O_nchar_unique) > 1)
        {
          which( (temp_Symbol_relevant_O %>% select(TIMESTAMP) %>% unlist %>% as.character %>% 
                    nchar) < 10) -> wrong_date_rows
          
          if ( length(wrong_date_rows) > 0)
          {
            wrong_date_rows %>% temp_Symbol_relevant_O[.,] %>% select(TIMESTAMP) %>% 
              unlist %>% as.character %>% as.Date(format="%d-%b-%y") %>% 
              format("%d-%b-%Y") %>% as.character -> temp_Symbol_relevant_O[wrong_date_rows,"TIMESTAMP"]
            
            wrong_date_rows %>% temp_Symbol_relevant_O[.,] %>% select(EXPIRY_DT) %>% 
              unlist %>% as.character %>% as.Date(format="%d-%b-%y") %>% 
              format("%d-%b-%Y") %>% as.character -> temp_Symbol_relevant_O[wrong_date_rows,"EXPIRY_DT"]
          } # End of 'if ( length(wrong_date_rows) > 0)'
          
        } # End of 'if(length(temp_Symbol_relevant_O_nchar_unique) > 0
        
        temp_Symbol_relevant_O %>% select(TIMESTAMP) %>% unlist %>% as.character %>% 
          as.Date(format="%d-%b-%Y") %>% format("%Y%m%d") %>% as.numeric -> temp_Symbol_relevant_O$TIMESTAMP
        
        temp_Symbol_relevant_O %>% select(EXPIRY_DT) %>% unlist %>% as.character %>% 
          as.Date(format="%d-%b-%Y") %>% format("%Y%m%d") %>% as.numeric -> temp_Symbol_relevant_O$EXPIRY_DT
        
        temp_Symbol_relevant_O[with(temp_Symbol_relevant_O, order(EXPIRY_DT,STRIKE_PR,TIMESTAMP)), ] -> temp_Symbol_relevant_O
        
        temp_Symbol_relevant_O[temp_Symbol_relevant_O$OPTIONTYPE == "CE",] -> temp_Symbol_relevant_O_CE
        temp_Symbol_relevant_O[temp_Symbol_relevant_O$OPTIONTYPE == "PE",] -> temp_Symbol_relevant_O_PE
        
        if(nrow(temp_Symbol_relevant_O_CE) > 0)
        {
          temp_Symbol_relevant_O_CE[with(temp_Symbol_relevant_O_CE, 
                                      order(TIMESTAMP,EXPIRY_DT,OPTIONTYPE,STRIKE_PR)), ] -> temp_Symbol_relevant_O_CE
          
          temp_Symbol_relevant_O_CE_sheetname <- paste0(temp_Symbol,"_O_CE",sep='')
          temp_Symbol_relevant_O_CE_sheetname %>% gsub("&","_and_",.) -> temp_Symbol_relevant_O_CE_sheetname
          temp_Symbol_Output_excel_sheetnames <- c(temp_Symbol_Output_excel_sheetnames,temp_Symbol_relevant_O_CE_sheetname)
          
          print(paste0("For Symbol ", temp_Symbol,", making new sheet: ",temp_Symbol_relevant_O_CE_sheetname,sep=''))
          
          addWorksheet(temp_Symbol_Output_excel, sheet = temp_Symbol_relevant_O_CE_sheetname)
          writeData(temp_Symbol_Output_excel, sheet = temp_Symbol_relevant_O_CE_sheetname,
                    x = temp_Symbol_relevant_O_CE)
        } # End of 'if(nrow(temp_Symbol_relevant_O_CE) > 0)'
        
        if(nrow(temp_Symbol_relevant_O_PE) > 0)
        {
          temp_Symbol_relevant_O_PE[with(temp_Symbol_relevant_O_PE, 
                                         order(TIMESTAMP,EXPIRY_DT,OPTIONTYPE,STRIKE_PR)), ] -> temp_Symbol_relevant_O_PE
          
          temp_Symbol_relevant_O_PE_sheetname <- paste0(temp_Symbol,"_O_PE",sep='')
          temp_Symbol_relevant_O_PE_sheetname %>% gsub("&","_and_",.) -> temp_Symbol_relevant_O_PE_sheetname
          temp_Symbol_Output_excel_sheetnames <- c(temp_Symbol_Output_excel_sheetnames,temp_Symbol_relevant_O_PE_sheetname)
          
          print(paste0("For Symbol ", temp_Symbol,", making new sheet: ",temp_Symbol_relevant_O_PE_sheetname,sep=''))
          
          addWorksheet(temp_Symbol_Output_excel, sheet = temp_Symbol_relevant_O_PE_sheetname)
          writeData(temp_Symbol_Output_excel, sheet = temp_Symbol_relevant_O_PE_sheetname,
                    x = temp_Symbol_relevant_O_PE)
        } # End of 'if(nrow(temp_Symbol_relevant_O_PE) > 0)'
        ###################
        
      } # End of 'if (Found_Expiry_limit)'
      
    } # End of 'if(Old_Excel_Exists)'
    
    # Saving the file, only when required
    
    if(!Old_Excel_Exists)
    {
      OPTIDX_Excel_TS_Database %>% setwd
      paste0("Making new file: ",temp_Options_excel_filename, sep='')
      saveWorkbook(temp_Symbol_Output_excel, temp_Options_excel_filename)
    }else{
      if(Overwrite_Required)
      {
        OPTIDX_Excel_TS_Database %>% setwd
        "For Symbol: '" %>% paste0(temp_Symbol_from_filename,sep='') %>% 
          paste0("' overwriting! Deleting old file: '",temp_Options_excel_filename,"'",sep='') %>% print
        
        # file.remove(temp_Options_excel_filename) # ', overwrite = T' argument is a better approach
        
        "For Symbol: '" %>% paste0(temp_Symbol_from_filename,sep='') %>% 
          paste0("' saving with new data, new filename: '",temp_Options_excel_filename,"'",sep='') %>% print
        
        saveWorkbook(temp_Symbol_Output_excel, temp_Options_excel_filename, overwrite = T)
      }else{
        "For Symbol: '" %>% paste0(temp_Symbol_from_filename,sep='') %>% 
          paste0("', Old file: '",temp_Options_excel_filename,"' was up to date",sep='') %>% print
      } # End of 'if(Overwrite_Required)'
    } # End of 'if(!Old_Excel_Exists)'
  } # End of 'for (i_1 in 1: length(OPTIDX_TS_Database_Files))'
}else{
  print("'OPTIDX_TS_Database' folder is empty")
} # End of 'if (length(OPTIDX_TS_Database_Files) > 0)'

#################################################################################
#################################################################################