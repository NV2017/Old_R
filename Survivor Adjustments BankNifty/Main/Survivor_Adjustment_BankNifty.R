#################################################################################
#################################### Goals ######################################

# Date: 2020-Aug-25
# Author: Arunabha Sarkar

# Goals: Survivor Adjustment BankNifty
# File Name: Survivor_Adjustment_BankNifty

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
################## Set Directories & other Hyperparameters ######################

Output_Col_Nums <- 51

Output_filename <- 'Survivor_Adjustment_BankNifty_Tickers.csv'

"C:/Users/Arunabha Sarkar/Desktop/Work From home/Central Database" -> Central_Database

Central_Database %>% paste0("/Nifty 50 Historical Data Investing dot com",
                            sep='') -> Nifty50_Database  # To find out which day is open & NIFTY Spot

Central_Database %>% paste0("/Survivorship Adjustment BankNifty",sep='') -> Survivorship_Adjustments_Database  

Central_Database %>% gsub("Central Database","",.) %>% 
  paste0(.,"Codes_Handling_Central_Database/Survivorship Adjustments Indexes/Raw Data Files",
         sep='') -> Survivorship_Adjustments_Raw_Database

Central_Database %>% gsub("Central Database","",.) %>% 
  paste0(.,"Codes_Handling_Central_Database/Survivorship Adjustments BankNifty/Resources",
         sep='') -> Survivorship_Adjustments_BankNifty_Raw_Database

Central_Database %>% setwd

if(!file.exists('Survivorship Adjustments Indexes'))
{
  print("Creating 'Survivorship Adjustments Indexes' directory.")
  dir.create(file.path(Central_Database, 'Survivorship Adjustments Indexes'))
}else{
  print("'Survivorship Adjustments Indexes' directory already exists.")
} # End of 'if(!file.exists('Survivorship Adjustments Indexes'))'

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
######################### Finding Company Ticker Maps ###########################

# Finding Company Ticker Maps
Survivorship_Adjustments_Raw_Database %>% setwd
# 'Prowess_Company_Names_Map_NSE_Tickers'
list.files() -> Survivorship_Adjustments_Raw_Database_Files

Survivorship_Adjustments_Raw_Database_Files %>% tolower %>% grep("prowess_company_names_map_nse_tickers.csv",.) %>% 
  Survivorship_Adjustments_Raw_Database_Files[.] %>% read.csv -> Prowess_Company_Names_Map_NSE_Tickers

Prowess_Company_Names_Map_NSE_Tickers$Company_Name_ProwessIQ %>% unlist %>% 
  as.character -> Prowess_Company_Names

Prowess_Company_Names_Map_NSE_Tickers$ProwessIQ_NSE %>% unlist %>% 
  as.character -> Prowess_Company_Tickers

#################################################################################
#################################################################################

#################################################################################
####################### Finding Latest BankNiftyList ##########################

Survivorship_Adjustments_BankNifty_Raw_Database %>% setwd

list.files() -> Survivorship_Adjustments_Raw_Database_Files

Survivorship_Adjustments_Raw_Database_Files %>% tolower %>% grep("banknifty",.) %>% 
  Survivorship_Adjustments_Raw_Database_Files[.] -> All_candidates

All_candidates %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>%
  as.Date(format="%Y%m%d") %>% which.max %>% All_candidates[.] %>% 
  read.xlsx(sheet = 1,detectDates = T,check.names = T,na.strings = "") -> Latest_All_candidates

if(exists("Latest_All_candidates"))
{
  'Latest_All_candidates' %>% paste0("Found '",.,"'",sep='') %>% print
  
  Latest_All_candidates[1,-1] %>% unlist %>% as.character %>% as.numeric %>% 
    as.Date(origin="1899-12-30") %>% format("%Y%M%d") %>% as.numeric %>% 
    as.character %>% paste0("Included On ",.,sep='') %>% 
    c("Company",.) -> colnames(Latest_All_candidates)
  
  which(Latest_All_candidates$Company == "Company Name") %>% seq(1,.,1) %>% {-(.)} %>% 
    Latest_All_candidates[.,] -> Latest_All_candidates
  
  Latest_All_candidates %>% colnames %>% {.[-1]} %>% strsplit(split = ' ') %>% 
    lapply(.,rev) %>% map(1) %>% unlist %>% as.character %>% as.numeric %>% sort %T>%
    {. ->> Latest_All_candidates_dates} %>% min -> Min_Latest_All_candidates
  
  'Latest_All_candidates' %>% paste0("Reformatted '",.,"'",sep='') %>% print
}else{
  # Couldn't find 'Latest_All_candidates'
  'Latest_All_candidates' %>% paste0("Couldn't find '",.,"'",sep='') %>% print
} # End of 'if(exists("Latest_All_candidates"))'

#################################################################################
#################################################################################

#################################################################################
####################### Making Survivorship Adjustments #########################

# If previous output doesn't exist, then make from start using 'Latest_All_candidates'

if(exists("Latest_All_candidates"))
{
  # Checking for previous output
  Survivorship_Adjustments_Database %>% setwd
  list.files() -> Survivorship_Adjustments_Database_files
  if (Output_filename %in% Survivorship_Adjustments_Database_files)
  {
    # Found existing banknifty survivorship adjusted file
    # Update/overwrite over last two prowessIQ updates
    
    "Found existing BankNifty survivorship adjusted file" %>% print
    
    # Finding latest 2 expiries from 'Latest_All_candidates'
    Latest_All_candidates %>% colnames %>% {.[-1]} %>% strsplit(split=" ") %>% lapply(.,rev) %>% 
      map(1) %>% unlist %>% as.character %>% as.numeric %>% sort %>% rev %>% .[2] -> Relevant_Min_Date
    
    Relevant_Min_Date %>% {which(N50_Dates_YYYYMMDD >= .)} %>% N50_Dates_YYYYMMDD[.] -> Relevant_Dates
    
    Survivorship_Adjustments_Database %>% setwd
    Output_filename %>% read.csv -> Old_file
    
    which(as.numeric(Old_file$Dates_YYYYMMDD) < Relevant_Min_Date) %>% Old_file[.,] -> temp_new
    
    temp_new %>% colnames %>% length %>% matrix(data = NA, nrow = length(Relevant_Dates), ncol = .) %>% 
      data.frame %>% `colnames<-`(colnames(temp_new)) -> temp_temp_new
    
    temp_temp_new$Dates_YYYYMMDD <- Relevant_Dates
    
    # i_1 <- 1
    for(i_1 in 1:length(Relevant_Dates))
    {
      "Making company ticker list for row number " %>% paste0(i_1,sep='') %>% 
        paste0(" out of ", length(Relevant_Dates),sep='') %>% print
      
      i_1 %>% Relevant_Dates[.] %>% as.numeric -> temp_date_YYYYMMDD
      
      # 'Latest_All_candidates'
      which(Latest_All_candidates_dates >= temp_date_YYYYMMDD) %>% 
        {if(length(.) == 0){return(length(Latest_All_candidates_dates))}else{return(.)}} %>% 
        {. + 1} %>% min -> Temp_correct_col_Latest_candidates
      
      Latest_All_candidates[,c(1,Temp_correct_col_Latest_candidates)] %>% 
        na.omit %>% .[,1] %>% unlist %>% as.character -> temp_company_names
      
      temp_company_names %>% gsub(" [Merged]","",.,fixed=TRUE) -> temp_company_names
      
      # 'Prowess_Company_Names' & 'Prowess_Company_Tickers'
      temp_company_names %>% {which(Prowess_Company_Names %in% .)} %>% 
        Prowess_Company_Tickers[.] -> temp_company_tickers
      
      temp_company_tickers %>% 
        c(.,rep(NA,(Output_Col_Nums-1-length(temp_company_tickers)))) -> temp_temp_new[i_1,-1]
    } # End of 'for(i_1 in 1:length(Relevant_Dates))'
    
    temp_new %>% rbind(.,temp_temp_new) -> temp_new
    
    # Save the file, delete the old as well
    Survivorship_Adjustments_Database %>% setwd
    print(paste0("Overwriting '",Output_filename,"'",sep=''))
    file.remove(Output_filename)
    temp_new %>% write.csv(.,file=Output_filename,na = "",row.names = FALSE)
    
  }else{
    "NOT found existing BankNifty survivorship adjusted file" %>% print
    
    # Find min starting date of Nifty 50
    which(N50_Dates_YYYYMMDD >= Min_Latest_Nifty_50_candidates) %>% 
      N50_Dates_YYYYMMDD[.] %>% as.numeric %>% as.character -> Relevant_Dates
    
    Relevant_Dates %>% length %>% matrix(data = NA, nrow = ., ncol = Output_Col_Nums) %>% 
      data.frame(stringsAsFactors = F) -> Survivorship_Adjustments_Company_Names_df
    
    Output_Col_Nums %>% {.-1} %>% seq(1,.,1) %>% paste0("Company_",.,sep='') %>% 
      c("Dates_YYYYMMDD",.) -> colnames(Survivorship_Adjustments_Company_Names_df)
    
    Survivorship_Adjustments_Company_Names_df$Dates_YYYYMMDD <- as.character(Relevant_Dates)
    
    # i_1 <- 1; i_1 <- 3326
    for(i_1 in 1:nrow(Survivorship_Adjustments_Company_Names_df))
    {
      #print(i_1)
      "Making company ticker list for row number " %>% paste0(i_1,sep='') %>% 
        paste0(" out of ", nrow(Survivorship_Adjustments_Company_Names_df),sep='') %>% print
      
      i_1 %>% Survivorship_Adjustments_Company_Names_df$Dates_YYYYMMDD[.] %>% as.numeric -> temp_date_YYYYMMDD
      # 'Latest_All_candidates'
      which(Latest_All_candidates_dates >= temp_date_YYYYMMDD) %>% 
        {if(length(.) == 0){return(length(Latest_All_candidates_dates))}else{return(.)}} %>% 
        {. + 1} %>% min -> Temp_correct_col_Latest_candidates
      
      Latest_All_candidates[,c(1,Temp_correct_col_Latest_candidates)] %>% 
        na.omit %>% .[,1] %>% unlist %>% as.character -> temp_company_names
      
      temp_company_names %>% gsub(" [Merged]","",.,fixed=TRUE) -> temp_company_names
      
      # 'Prowess_Company_Names' & 'Prowess_Company_Tickers'
      temp_company_names %>% {which(Prowess_Company_Names %in% .)} %>% 
        Prowess_Company_Tickers[.] -> temp_company_tickers
      
      temp_company_tickers %>% 
        c(.,rep(NA,(Output_Col_Nums-1-length(temp_company_tickers)))) -> Survivorship_Adjustments_Company_Names_df[i_1,-1]
    } # End of 'for(i_1 in 1:nrow(Survivorship_Adjustments_Company_Names_df))'
    
    # Save the file
    print(paste0("Making new '",Output_filename,"'",sep=''))
    Survivorship_Adjustments_Database %>% setwd
    Survivorship_Adjustments_Company_Names_df %>% 
      write.csv(.,file=Output_filename,na = "",row.names = FALSE)
    
  } # End of 'if (Output_filename %in% Survivorship_Adjustments_Database_files)'
}else{
  # Couldn't find 'Latest_All_candidates'
  'Latest_All_candidates' %>% paste0("Couldn't find '",.,"'",sep='') %>% print
} # End of 'if(exists("Latest_All_candidates"))'

#################################################################################
#################################################################################