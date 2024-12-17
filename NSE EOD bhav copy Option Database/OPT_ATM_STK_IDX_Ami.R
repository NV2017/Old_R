#################################################################################
#################################### Goals ######################################

# Date: 2020-June-10
# Author: Arunabha Sarkar

# Goals: Splitting OPTIDX & OPTSTK into PE and CE for ATM only, for Amibroker
# File Name: OPT_ATM_STK_IDX_Ami

#################################################################################
#################################################################################

#################################################################################
##################### Initializing and loading Libraries ########################

library(dplyr)

#################################################################################
#################################################################################

#################################################################################
############################### Set Directories #################################

"C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% paste0("/Latest_NSE_Index_Composition",
                            sep='') -> Latest_NSE_Index_Composition_Database

Central_Database %>% paste0("/OPTIDX_TS",sep='') -> OPTIDX_TS_Database

Central_Database %>% paste0("/OPTSTK_TS",sep='') -> OPTSTK_TS_Database

Central_Database %>% paste0("/OPT_ATM_STK_IDX_Ami",sep='') -> OPT_ATM_STK_IDX_Ami_Database

Central_Database %>% setwd()

if(!file.exists('OPT_ATM_STK_IDX_Ami'))
{
  print("Creating 'OPT_ATM_STK_IDX_Ami' directory.")
  dir.create(file.path(Central_Database, 'OPT_ATM_STK_IDX_Ami'))
}else{
  print("'OPT_ATM_STK_IDX_Ami' directory already exists.")
} # End of 'if(!file.exists('OPT_ATM_STK_IDX_Ami'))'

#################################################################################
#################################################################################

#################################################################################
################################ For OPTIDX_TS #################################

OPTIDX_TS_Database %>% setwd

list.files() -> OPTIDX_TS_Database_files

if(length(OPTIDX_TS_Database_files) > 0)
{
  # i_1 <- 1; i_1 <- 2
  for (i_1 in 1:length(OPTIDX_TS_Database_files))
  {
    OPTIDX_TS_Database %>% setwd
    
    i_1 %>% OPTIDX_TS_Database_files[.] -> temp_filename
    
    temp_filename %>% strsplit(split="_") %>% .[[1]] %>% .[1] -> temp_Symbol
    
    temp_Symbol %>% paste0(.,"_CE_ATM_Ami.csv",sep='') -> temp_filename_CE
    temp_Symbol %>% paste0(.,"_PE_ATM_Ami.csv",sep='') -> temp_filename_PE
    
    temp_filename %>% read.csv %>% select(Date_YYYYMMDD_C,OPEN_CE_ATM,HIGH_CE_ATM,LOW_CE_ATM,
                                          CLOSE_CE_ATM,VAL_INLAKH_CE_ATM,OPEN_INT_CE_ATM,STRIKE_CE_ATM) %>% 
      mutate(Date_YYYYMMDD_C = as.character(Date_YYYYMMDD_C)) %>% 
      mutate(Date_YYYYMMDD_C = as.Date(Date_YYYYMMDD_C,format="%Y%m%d",origin="01-01-1970")) %>% 
      mutate(Date_YYYYMMDD_C = format(Date_YYYYMMDD_C,"%d-%m-%y")) %>% 
      `colnames<-`(c("Date","Open","High","Low",
                     "Close","Volume","Open Interest","Strike")) -> temp_CE_ATM_csv
    
    temp_filename %>% read.csv %>% select(Date_YYYYMMDD_P,OPEN_PE_ATM,HIGH_PE_ATM,LOW_PE_ATM,
                                          CLOSE_PE_ATM,VAL_INLAKH_PE_ATM,OPEN_INT_PE_ATM,STRIKE_PE_ATM) %>% 
      mutate(Date_YYYYMMDD_P = as.character(Date_YYYYMMDD_P)) %>% 
      mutate(Date_YYYYMMDD_P = as.Date(Date_YYYYMMDD_P,format="%Y%m%d",origin="01-01-1970")) %>% 
      mutate(Date_YYYYMMDD_P = format(Date_YYYYMMDD_P,"%d-%m-%y")) %>% 
      `colnames<-`(c("Date","Open","High","Low",
                     "Close","Volume","Open Interest","Strike")) -> temp_PE_ATM_csv
    
    temp_PE_ATM_csv %>% select(Date) %>% unlist %>% as.character %>% 
      as.Date(format="%d-%m-%y") %>% format("%Y") %>% as.numeric %>% 
      {which(. > 2007)} %>% temp_PE_ATM_csv[.,] %>% na.omit -> temp_PE_ATM_csv
    
    temp_CE_ATM_csv %>% select(Date) %>% unlist %>% as.character %>% 
      as.Date(format="%d-%m-%y") %>% format("%Y") %>% as.numeric %>% 
      {which(. > 2007)} %>% temp_CE_ATM_csv[.,] %>% na.omit -> temp_CE_ATM_csv
    
    OPT_ATM_STK_IDX_Ami_Database %>% setwd
    
    list.files() -> Existing_files_OPT_split_PE_CE_Database
    
    if(temp_filename_PE %in% Existing_files_OPT_split_PE_CE_Database)
    {
      # Delete the file
      print(paste0(i_1, " out of ", length(OPTIDX_TS_Database_files), " symbols: ",
                   temp_Symbol, ". Deleting old file: ",temp_filename_PE, sep=''))
      file.remove(temp_filename_PE)
      print(paste0(i_1, " out of ", length(OPTIDX_TS_Database_files), " symbols: ",
                   temp_Symbol, ". Making new file: ",temp_filename_PE, sep=''))
      write.csv(temp_PE_ATM_csv,file = temp_filename_PE,row.names = FALSE,na="")
    }else{
      print(paste0(i_1, " out of ", length(OPTIDX_TS_Database_files), " symbols: ",
                   temp_Symbol, ". Making new file: ",temp_filename_PE, sep=''))
      write.csv(temp_PE_ATM_csv,file = temp_filename_PE,row.names = FALSE,na="")
    } # End of 'if(temp_filename_PE %in% Existing_files_OPT_split_PE_CE_Database)'
    
    if(temp_filename_CE %in% Existing_files_OPT_split_PE_CE_Database)
    {
      # Delete the file
      print(paste0(i_1, " out of ", length(OPTIDX_TS_Database_files), " symbols: ",
                   temp_Symbol, ". Deleting old file: ",temp_filename_CE, sep=''))
      file.remove(temp_filename_CE)
      print(paste0(i_1, " out of ", length(OPTIDX_TS_Database_files), " symbols: ",
                   temp_Symbol, ". Making new file: ",temp_filename_CE, sep=''))
      write.csv(temp_CE_ATM_csv,file = temp_filename_CE,row.names = FALSE,na="")
    }else{
      print(paste0(i_1, " out of ", length(OPTIDX_TS_Database_files), " symbols: ",
                   temp_Symbol, ". Making new file: ",temp_filename_CE, sep=''))
      write.csv(temp_CE_ATM_csv,file = temp_filename_CE,row.names = FALSE,na="")
    } # End of 'if(temp_filename_CE %in% Existing_files_OPT_split_PE_CE_Database)'
  } # End of 'for (i_1 in 1:length(OPTIDX_TS_Database_files))'
}else{
  print("No EXCEL files detected in the 'OPTSTK_Excel_TS_Database' folder")
} # End of 'if(length(OPTIDX_TS_Database_files) > 0)'

#################################################################################
#################################################################################

#################################################################################
################################ For OPTSTK_TS #################################

#### Arranging 'Latest_File_Symbols' with Nifty50 first

Latest_NSE_Index_Composition_Database %>% setwd

list.files() -> Latest_NSE_Index_Composition_Database_Files

"Nifty50.csv" %>% grep(Latest_NSE_Index_Composition_Database_Files) %>% 
  Latest_NSE_Index_Composition_Database_Files[.] -> All_Nifty_50_candidates

All_Nifty_50_candidates %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>%
  as.Date(format="%Y%m%d") %>% which.max %>% All_Nifty_50_candidates[.] %>% 
  read.csv %>% select(Symbol) %>% unlist %>% as.character -> Latest_Nifty_50_tickers

#######################################################

OPTSTK_TS_Database %>% setwd

list.files() -> OPTSTK_TS_Database_files

Latest_Nifty_50_tickers %>% paste0(.,"_Options.csv",sep="") %>% 
  c(.,OPTSTK_TS_Database_files) %>% unique -> OPTSTK_TS_Database_files

OPTSTK_TS_Database %>% setwd

if(length(OPTSTK_TS_Database_files) > 0)
{
  # i_1 <- 1
  for (i_1 in 1:length(OPTSTK_TS_Database_files))
  {
    OPTSTK_TS_Database %>% setwd
    
    i_1 %>% OPTSTK_TS_Database_files[.] -> temp_filename
    
    temp_filename %>% strsplit(split="_") %>% .[[1]] %>% .[1] -> temp_Symbol
    
    temp_Symbol %>% paste0(.,"_CE_ATM_Ami.csv",sep='') -> temp_filename_CE
    temp_Symbol %>% paste0(.,"_PE_ATM_Ami.csv",sep='') -> temp_filename_PE
    
    temp_filename %>% read.csv %>% select(Date_YYYYMMDD_C,OPEN_CE_ATM,HIGH_CE_ATM,LOW_CE_ATM,
                                          CLOSE_CE_ATM,VAL_INLAKH_CE_ATM,OPEN_INT_CE_ATM,STRIKE_CE_ATM) %>% 
      mutate(Date_YYYYMMDD_C = as.character(Date_YYYYMMDD_C)) %>% 
      mutate(Date_YYYYMMDD_C = as.Date(Date_YYYYMMDD_C,format="%Y%m%d",origin="01-01-1970")) %>% 
      mutate(Date_YYYYMMDD_C = format(Date_YYYYMMDD_C,"%d-%m-%y")) %>% 
      `colnames<-`(c("Date","Open","High","Low",
                     "Close","Volume","Open Interest","Strike")) -> temp_CE_ATM_csv
    
    temp_filename %>% read.csv %>% select(Date_YYYYMMDD_P,OPEN_PE_ATM,HIGH_PE_ATM,LOW_PE_ATM,
                                          CLOSE_PE_ATM,VAL_INLAKH_PE_ATM,OPEN_INT_PE_ATM,STRIKE_PE_ATM) %>% 
      mutate(Date_YYYYMMDD_P = as.character(Date_YYYYMMDD_P)) %>% 
      mutate(Date_YYYYMMDD_P = as.Date(Date_YYYYMMDD_P,format="%Y%m%d",origin="01-01-1970")) %>% 
      mutate(Date_YYYYMMDD_P = format(Date_YYYYMMDD_P,"%d-%m-%y")) %>% 
      `colnames<-`(c("Date","Open","High","Low",
                     "Close","Volume","Open Interest","Strike")) -> temp_PE_ATM_csv
    
    OPT_ATM_STK_IDX_Ami_Database %>% setwd
    
    list.files() -> Existing_files_OPT_split_PE_CE_Database
    
    if(temp_filename_PE %in% Existing_files_OPT_split_PE_CE_Database)
    {
      # Delete the file
      print(paste0(i_1, " out of ", length(OPTSTK_TS_Database_files), " symbols: ",
                   temp_Symbol, ". Deleting old file: ",temp_filename_PE, sep=''))
      file.remove(temp_filename_PE)
      print(paste0(i_1, " out of ", length(OPTSTK_TS_Database_files), " symbols: ",
                   temp_Symbol, ". Making new file: ",temp_filename_PE, sep=''))
      write.csv(temp_PE_ATM_csv,file = temp_filename_PE,row.names = FALSE)
    }else{
      print(paste0(i_1, " out of ", length(OPTSTK_TS_Database_files), " symbols: ",
                   temp_Symbol, ". Making new file: ",temp_filename_PE, sep=''))
      write.csv(temp_PE_ATM_csv,file = temp_filename_PE,row.names = FALSE)
    } # End of 'if(temp_filename_PE %in% Existing_files_OPT_split_PE_CE_Database)'
    
    if(temp_filename_CE %in% Existing_files_OPT_split_PE_CE_Database)
    {
      # Delete the file
      print(paste0(i_1, " out of ", length(OPTSTK_TS_Database_files), " symbols: ",
                   temp_Symbol, ". Deleting old file: ",temp_filename_CE, sep=''))
      file.remove(temp_filename_CE)
      print(paste0(i_1, " out of ", length(OPTSTK_TS_Database_files), " symbols: ",
                   temp_Symbol, ". Making new file: ",temp_filename_CE, sep=''))
      write.csv(temp_CE_ATM_csv,file = temp_filename_CE,row.names = FALSE)
    }else{
      print(paste0(i_1, " out of ", length(OPTSTK_TS_Database_files), " symbols: ",
                   temp_Symbol, ". Making new file: ",temp_filename_CE, sep=''))
      write.csv(temp_CE_ATM_csv,file = temp_filename_CE,row.names = FALSE)
    } # End of 'if(temp_filename_CE %in% Existing_files_OPT_split_PE_CE_Database)'
  } # End of 'for (i_1 in 1:length(OPTSTK_TS_Database_files))'
}else{
  print("No EXCEL files detected in the 'OPTSTK_Excel_TS_Database' folder")
} # End of 'if(length(OPTSTK_TS_Database_files) > 0)'

#################################################################################
#################################################################################