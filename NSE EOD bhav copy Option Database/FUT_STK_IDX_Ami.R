#################################################################################
#################################### Goals ######################################

# Date: 2020-Jun-01
# Author: Arunabha Sarkar

# Goals: Make Futures Stocks & Indices Data Time Series for Amibroker direct upload
# File Name: FUT_STK_IDX_Ami

#################################################################################
#################################################################################

#################################################################################
##################### Initializing and loading Libraries ########################

library(dplyr)
library(purrr)

#################################################################################
#################################################################################

#################################################################################
############################### Set Directories #################################

Desired_Colnames <- c("Date", "Open", "High", "Low", "Close", "Volume", "Open Interest")

"C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% paste0("/Latest_NSE_Index_Composition",
                            sep='') -> Latest_NSE_Index_Composition_Database

Central_Database %>% paste0("/FUTIDX_TS",sep='') -> FUTIDX_TS_Database

Central_Database %>% paste0("/FUTSTK_TS",sep='') -> FUTSTK_TS_Database

Central_Database %>% setwd()

if(!file.exists('FUT_STK_IDX_Ami'))
{
  print("Creating 'FUT_STK_IDX_Ami' directory.")
  dir.create(file.path(Central_Database, 'FUT_STK_IDX_Ami'))
}else{
  print("'FUT_STK_IDX_Ami' directory already exists.")
}

Central_Database %>% paste0("/FUT_STK_IDX_Ami",sep='') -> FUT_STK_IDX_Ami_Database

#################################################################################
#################################################################################

#################################################################################
########################### Deleting Prior Content ##############################

FUT_STK_IDX_Ami_Database %>% setwd
list.files() -> FUT_STK_IDX_Ami_Database_old_content

if (length(FUT_STK_IDX_Ami_Database_old_content) > 0)
{
  print(paste0("Deleting ", length(FUT_STK_IDX_Ami_Database_old_content), " prior files", sep=''))
  # i_1 <- 1
  for(i_1 in 1:length(FUT_STK_IDX_Ami_Database_old_content))
  {
    print(paste0("Deleting ", i_1, " out of ", length(FUT_STK_IDX_Ami_Database_old_content), sep=''))
    i_1 %>% FUT_STK_IDX_Ami_Database_old_content[.] %>% file.remove
  } # End of 'for(i_1 in 1:length(FUT_STK_IDX_Ami_Database_old_content))'
}else{
  print("No prior files found to delete")
} # End of 'if (length(FUT_STK_IDX_Ami_Database_old_content) > 0)'

#################################################################################
#################################################################################

#################################################################################
########################### Setting FUTSTK for Ami ##############################

#### Arranging 'Latest_File_Symbols' with Nifty50 first

Latest_NSE_Index_Composition_Database %>% setwd

list.files() -> Latest_NSE_Index_Composition_Database_Files

"Nifty50.csv" %>% grep(Latest_NSE_Index_Composition_Database_Files) %>% 
  Latest_NSE_Index_Composition_Database_Files[.] -> All_Nifty_50_candidates

All_Nifty_50_candidates %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>%
  as.Date(format="%Y%m%d") %>% which.max %>% All_Nifty_50_candidates[.] %>% 
  read.csv %>% select(Symbol) %>% unlist %>% as.character -> Latest_Nifty_50_tickers

#######################################################

FUTSTK_TS_Database %>% setwd

list.files() -> Latest_FUTSTK_TS_Database_content

Latest_Nifty_50_tickers %>% paste0(.,"_Futures.csv",sep="") %>% 
  c(.,Latest_FUTSTK_TS_Database_content) %>% unique -> Latest_FUTSTK_TS_Database_content

FUTSTK_TS_Database %>% setwd

if (length(Latest_FUTSTK_TS_Database_content) > 0)
{
  print(paste0("Found ", length(Latest_FUTSTK_TS_Database_content), 
               " files in FUTSTK_TS_Database", sep=''))
  # i_2 <- 1
  for (i_2 in 1:length(Latest_FUTSTK_TS_Database_content))
  {
    FUTSTK_TS_Database %>% setwd
    
    i_2 %>% Latest_FUTSTK_TS_Database_content[.] -> temp_raw_file
    
    temp_raw_file %>% strsplit(.,split='.csv') %>% unlist(.) %>% 
      paste0(., "_Ami_Upload.csv",sep='') -> temp_new_filename
    
    temp_raw_file %>% read.csv %>% 
      select(Date_YYYYMMDD,OPEN_NEAR,HIGH_NEAR,LOW_NEAR,CLOSE_NEAR,CONTRACTS_NEAR,OPEN_INT_NEAR) %>% 
      `colnames<-`((Desired_Colnames)) -> temp_new_df
    
    FUT_STK_IDX_Ami_Database %>% setwd
    
    temp_new_filename %>% paste0("Making '", .,"'", sep='') %>% print
    
    temp_new_df %>% write.csv(temp_new_filename, row.names = F, na='')
  }
}else{
  print("Found NO files in FUTSTK_TS_Database")
}

#################################################################################
#################################################################################

#################################################################################
########################### Setting FUTIDX for Ami ##############################

FUTIDX_TS_Database %>% setwd

list.files() -> Latest_FUTIDX_TS_Database_content

if (length(Latest_FUTIDX_TS_Database_content) > 0)
{
  print(paste0("Found ", length(Latest_FUTIDX_TS_Database_content), 
               " files in FUTIDX_TS_Database", sep=''))
  # i_3 <- 1
  for (i_3 in 1:length(Latest_FUTIDX_TS_Database_content))
  {
    FUTIDX_TS_Database %>% setwd
    
    i_3 %>% Latest_FUTIDX_TS_Database_content[.] -> temp_raw_file
    
    temp_raw_file %>% strsplit(.,split='.csv') %>% unlist(.) %>% 
      paste0(., "_Ami_Upload.csv",sep='') -> temp_new_filename
    
    temp_raw_file %>% read.csv %>% 
      select(Date_YYYYMMDD,OPEN_NEAR,HIGH_NEAR,LOW_NEAR,CLOSE_NEAR,CONTRACTS_NEAR,OPEN_INT_NEAR) %>% 
      `colnames<-`((Desired_Colnames)) -> temp_new_df
    
    FUT_STK_IDX_Ami_Database %>% setwd
    
    temp_new_filename %>% paste0("Making '", .,"'", sep='') %>% print
    
    temp_new_df %>% write.csv(temp_new_filename, row.names = F, na='')
  }
}else{
  print("Found NO files in FUTIDX_TS_Database")
}

#################################################################################
#################################################################################