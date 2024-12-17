#################################################################################
#################################### Goals ######################################

# Date: 2020-November-19
# Author: Arunabha Sarkar

# Goals: Get minute level NSE all FnO bhav copy data if possible
# File Name: Minute_Level_NSE_FnO_bhav_copy

#################################################################################
#################################################################################

#################################################################################
##################### Initializing and loading Libraries ########################

library(dplyr)
library(rvest)
library(stringr)
library(magrittr)

#################################################################################
#################################################################################

#################################################################################
################## Set Directories & other Hyperparameters ######################

Time_Break_Next_Page_Seconds <- 1

c("ASDF") -> Output_Colnames

Output_Folder_Name <- "Minute_Level_NSE_FnO_bhav_copy"

"C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% paste0("/NSE FnO Bhav Copies",sep='') -> FnO_Bhav_Database

Central_Database %>% setwd

if(!file.exists(Output_Folder_Name))
{
  Output_Folder_Name %>% paste0("Creating '",.,"' directory.",sep="") %>% print
  dir.create(file.path(Central_Database, Output_Folder_Name))
}else{
  Output_Folder_Name %>% paste0("Directory '",.,"' already exists.",sep="") %>% print
} # End of 'if(!file.exists(Output_Folder_Name))'

Central_Database %>% paste0(.,"/",Output_Folder_Name,sep='') -> Output_Folder

url_nifty <- "https://www.nseindia.com/live_market/dynaContent/live_watch/fomwatchsymbol.jsp?key=NIFTY&Fut_Opt=Futures"

Part1_Options_URL <- "https://www1.nseindia.com/live_market/dynaContent/live_watch/option_chain/optionKeys.jsp?segmentLink=17&instrument=OPTSTK&symbol="
Part1_Options_Index_URL <- "https://www1.nseindia.com/live_market/dynaContent/live_watch/option_chain/optionKeys.jsp?segmentLink=17&instrument=OPTIDX&symbol="
Part3_Options_URL <- "&date="

Market_Open_Time <- "0914"
Market_Close_Time <- "1555"
Muhurat_Market_Open_Time <- "1800"
Muhurat_Market_Close_Time <- "1955"

Muhurat_Exception_YYYYMMDD <- c("20201114")

While_Pause_Time_Seconds <- 1

Futures_Colnames <- c("Instrument","Underlying","Expiry_Date","Option_Type",
                      "Strike_Price","Open_Price","High_Price","Low_Price","Prev_Close",
                      "Last_Price","Volume","Turnover_lacs","Underlying_Value")

URL_latest_NSE_Index_composition <- "https://archives.nseindia.com/content/fo/fo_mktlots.csv"

Central_Database %>% paste0("/NSE FnO Bhav Copies",sep='') -> FnO_Bhav_Database

temp_FnO_colnames <- c("INSTRUMENT","SYMBOL","EXPIRY_DT","STRIKE_PR","OPTIONTYPE",
                       "OPEN","HIGH","LOW","CLOSE","SETTLE_PR","CONTRACTS","VAL_INLAKH",
                       "OPEN_INT","CHG_IN_OI","TIMESTAMP")

Output_Filename_Extention <- "_Live_FnO_"

Market_Lot_Foldername <- "NSE_Market_Lots"
Central_Database %>% setwd

if(!file.exists(Market_Lot_Foldername))
{
  Market_Lot_Foldername %>% paste0("Creating '",.,"' directory.",sep="") %>% print
  dir.create(file.path(Central_Database, Market_Lot_Foldername))
}else{
  Market_Lot_Foldername %>% paste0("Directory '",.,"' already exists.",sep="") %>% print
} # End of 'if(!file.exists(Market_Lot_Foldername))'

Central_Database %>% paste0("/",Market_Lot_Foldername,sep='') -> Market_Lot_Database

Market_Lot_URL <- "https://archives.nseindia.com/content/fo/fo_mktlots.csv"

#################################################################################
#################################################################################

#################################################################################
############################### Perpetual Loop ##################################

while(TRUE)
{
  # The exact market times are known, but Muhurat Trading is special
  # Muhurat Trading can thus be handled as an exception in code
  Sys.Date() %>% format("%Y%m%d") %>% as.character -> Todays_Date
  # Sys.time() %>% as.character %>% strsplit(split=" ") %>% .[[1]] %>% .[2] %>% 
  #   gsub(":","",.) -> Time_Now
  
  # Finding start and open time
  if(Todays_Date %in% Muhurat_Exception_YYYYMMDD)
  {
    temp_Market_Open_Time <- Muhurat_Market_Open_Time
    temp_Market_Close_Time <- Muhurat_Market_Close_Time
  }else{
    temp_Market_Open_Time <- Market_Open_Time
    temp_Market_Close_Time <- Market_Close_Time
  } # End of 'if(Todays_Date %in% Muhurat_Exception_YYYYMMDD)'
  
  # Pause for 1 day if this is Saturday or Sunday, but not Muhurat Trading day
  Sys.Date() %>% weekdays -> Day_Today
  if((Day_Today %in% c("Saturday","Sunday")) | (Todays_Date %in% Muhurat_Exception_YYYYMMDD))
  {
    # Pause till next morning start
    Sys.Date() %>% {.+1} -> Tomorrows_Date
    
    Tomorrows_Date %>% format("%Y%m%d") -> Tomorrows_Date_YYYYMMDD
    
    if(Tomorrows_Date_YYYYMMDD %in% Muhurat_Exception_YYYYMMDD)
    {
      Muhurat_Market_Open_Time -> tomowwor_Market_Open_Time
    }else{
      Market_Open_Time -> tomowwor_Market_Open_Time
    } # End of 'if(Tomorrows_Date_YYYYMMDD %in% Muhurat_Exception_YYYYMMDD)'
    
    Tomorrows_Date %>% as.character %>% paste0(.," ",tomowwor_Market_Open_Time,"00",sep="") %>% 
      as.POSIXct(format="%Y-%m-%d %H%M%S") -> temp_next_day_start_time
    
    difftime(temp_next_day_start_time,Sys.time(),units="secs") %>% 
      as.numeric %T>% {print(paste0("Sleeping ",round(.)," seconds",sep=""))} %>% Sys.sleep
  }else{
    # Start code
    # Pause till approx market start time
    temp_Market_Open_Time %>% as.character %>% paste0(Sys.Date()," ",.,"00",sep="") %>% 
      as.POSIXct(format="%Y-%m-%d %H%M%S") -> temp_today_day_start_time
    
    difftime(temp_today_day_start_time,Sys.time(),units="secs") %>% 
      as.numeric -> temp_seconds_to_sleep
    if(temp_seconds_to_sleep > 0)
    {
      temp_seconds_to_sleep %>% {.-10} %T>% {print(paste0("Sleeping ",.," seconds",sep=""))} %>% 
        Sys.sleep
    } # End of 'if(temp_seconds_to_sleep > 0)'
    
    # Pause till next day if market time is over
    temp_Market_Close_Time %>% as.character %>% paste0(Sys.Date()," ",.,"00",sep="") %>% 
      as.POSIXct(format="%Y-%m-%d %H%M%S") -> temp_today_day_close_time
    
    difftime(temp_today_day_close_time,Sys.time(),units="secs") %>% 
      as.numeric -> temp_seconds_to_close
    
    if(temp_seconds_to_close < 0)
    {
      # Sleep till 1 hour post midnight
      Sys.Date() %>% as.character %>% paste0(.," 23:59:59",sep="") %>% 
        as.POSIXct(format="%Y-%m-%d %H:%M:%S") %>% difftime(.,Sys.time(),units="secs") %>% 
        as.numeric %>% {.+3600} %T>% {print(paste0("Sleeping ",round(.)," seconds",sep=""))} %>% 
        Sys.sleep
      next
    } # End of 'if(temp_seconds_to_close < 0)'
    
    # Check for next three expiry dates
    url_nifty %>% read_html %>% html_nodes("#tab26Content > table:nth-child(1)") %>%
      html_table(header = NA, trim = TRUE, fill = FALSE, dec = ".") %>%
      as.data.frame %>% `colnames<-`(Futures_Colnames) %T>% {. ->> NIFTY_Futures} %>% 
      select(Expiry_Date) %>% unlist %>% as.character -> Expiry_Dates_NSE_Format
    
    # Check for all tickers in FnO from Market Lot csv file from NSE
    # Add reader code for latest date file
    Market_Lot_Database %>% setwd
    list.files() -> Market_Lot_Database_files
    Sys.Date() %>% format("%Y%m%d") %>% paste0(.,"_Market_Lot.csv",sep="") -> temp_mklot_filename
    if(temp_mklot_filename %in% Market_Lot_Database_files)
    {
      temp_mklot_filename %>% read.csv(check.names = F) %>% select(SYMBOL) %>%
        unlist %>% as.character %>% gsub("\ ","",.) %>% .[.!="Symbol"] -> All_Symbols_Today
    }else{
      Market_Lot_URL %>% read.csv(check.names = F) %T>% 
        {write.csv(.,file = temp_mklot_filename,na="",row.names = F)} %>% 
        select(SYMBOL) %>% 
        unlist %>% as.character %>% gsub("\ ","",.) %>% .[.!="Symbol"] -> All_Symbols_Today
    } # End of 'if(temp_mklot_filename %in% Market_Lot_Database_files)'
    # temp_file <- tempfile()
    # URL_latest_NSE_Index_composition %>% download.file(temp_file)
    # temp_file %>% read.csv(check.names = F) -> Raw_DF
    # Raw_DF %>% colnames %>% gsub("[.]","-",.) -> colnames(Raw_DF)
    # Raw_DF$SYMBOL %>% as.character %>% gsub("\ ","",.) %>% 
    #   {.[. != "Symbol"]} %>% sort -> All_Symbols_Today
    
    # Finding all Index expiries using 'Raw_DF'
    
    # Loop over 'Expiry_Dates_NSE_Format' & 'All_Symbols_Today' per minute
    # Then pause for appropriate time
    # Pause for 'While_Pause_Time_Seconds' between symbols in market hours
    # If market hours is over, pause till next day market open
    
    # i_All_Symbols_Today <- 1
    for(i_All_Symbols_Today in 1:length(All_Symbols_Today))
    {
      i_All_Symbols_Today %>% All_Symbols_Today[.] -> temp_Symbol
      
      # Selecting correct 'temp_Part1_Options_URL'; & Find correct expiry list as well
      if(grepl("NIFTY",temp_Symbol))
      {
        # Cutoff for BANKNIFTY & NIFTY is 10; 
        Part1_Options_Index_URL -> temp_Part1_Options_URL
        # Check exact expiry date from latest bhavcopy
        FnO_Bhav_Database %>% setwd
        list.files() -> FnO_Bhav_Database_Files
        FnO_Bhav_Database_Files %>% as.Date(format="fo%d%b%Ybhav.csv") -> Dates_Order
        Dates_Order %>% {which.max(Dates_Order)} %>% FnO_Bhav_Database_Files[.] %>% 
          read.csv %>% .[,(1:length(temp_FnO_colnames))] %>% `colnames<-`(temp_FnO_colnames) %>% 
          filter(SYMBOL == temp_Symbol) %>% select(EXPIRY_DT) %>% unlist %>% 
          as.character %>% unique %>% as.Date(format="%d-%b-%Y") %>% sort %>% .[1:10] %>% 
          format("%d%b%Y") %>% toupper -> temp_Expiry_Dates_NSE_Format
      }else{
        Part1_Options_URL -> temp_Part1_Options_URL
        Expiry_Dates_NSE_Format -> temp_Expiry_Dates_NSE_Format
      } # End of 'if(grepl("NIFTY",temp_Symbol))'
      
      # i_Expiry_Dates_NSE_Format <- 1
      for(i_Expiry_Dates_NSE_Format in 1:length(temp_Expiry_Dates_NSE_Format))
      {
        i_Expiry_Dates_NSE_Format %>% temp_Expiry_Dates_NSE_Format[.] -> temp_Expiry
        
        temp_Part1_Options_URL %>% paste0(.,temp_Symbol,sep="") %>% 
          paste0(.,Part3_Options_URL,temp_Expiry,sep="") -> temp_Symbol_Expiry_URL
        
        # Load page, check Date, check if it is already saved
        # To do: Wrap the read_html line in try catch system
        No_Problem_Getting_NSE_URL <- FALSE
        query_counter <- 1
        while(!No_Problem_Getting_NSE_URL)
        {
          query_counter <- 1 + query_counter
          if (query_counter > 10)
          {
            break
          } # End of 'if (query_counter > 10)'
          tryCatch(
            expr = {
              temp_Symbol_Expiry_URL %>% read_html -> temp_HTML
              No_Problem_Getting_NSE_URL <- TRUE
            },
            error = function(e){ 
              e %>% paste0("Error: ",.,sep="") %>% print
              No_Problem_Getting_NSE_URL <- FALSE
            },
            warning = function(w){
              w %>% paste0("Warning: ",.,sep="") %>% print
              No_Problem_Getting_NSE_URL <- FALSE
            }
          )
        } # End of 'while(!No_Problem_Getting_NSE_URL)'
        if (query_counter > 10)
        {
          next
        } # End of 'if (query_counter > 10)'
        
        # temp_Symbol_Expiry_URL %>% read_html -> temp_HTML
        temp_HTML %>% html_nodes("#wrapper_btm") %>% .[1] %>% as.character -> temp_Proto_Date_Time
        temp_Proto_Date_Time %>% gregexpr("As on ",.) %>% .[[1]] %>% .[1] -> temp_start
        temp_Proto_Date_Time %>% gregexpr(" IST<a> <img",.) %>% .[[1]] %>% .[1] -> temp_end
        temp_Proto_Date_Time %>% substr(.,temp_start+6,temp_end-1) -> temp_Date_Time
        
        # Making filename using 'temp_Date_Time' & ''
        temp_Date_Time %>% as.POSIXct(format="%b %d, %Y %H:%M:%S") %T>% {.->>temp_Date_Time_Z} %>% 
          as.character %>% gsub("-","",.) %>% gsub(" ","",.) %>% gsub(":","",.) %>% 
          paste0(.,Output_Filename_Extention,temp_Symbol,
                 "_Expiry_",temp_Expiry,".csv",sep="") -> temp_Output_filename
        
        temp_Date_Time_Z %>% as.character %>% paste0(.," IST",sep="") -> temp_Date_Time_Z
        
        Output_Folder %>% setwd
        list.files() -> Output_Folder_Files
        if(!temp_Output_filename %in% Output_Folder_Files)
        {
          # Make output DF, then save
          temp_HTML %>% html_table(., fill=TRUE) %>% .[[3]] -> temp_output_df
          if(nrow(temp_output_df) > 1)
          {
            "Construction/Saving live FnO, Symbol #" %>% 
              paste0(.,i_All_Symbols_Today," out of ",length(All_Symbols_Today),": ",sep="") %>% 
              paste0(.,temp_Symbol,";",sep="") %>% 
              paste0(.," Expiry: ",temp_Expiry," for date-time: ",sep="") %>% 
              paste0(.,temp_Date_Time,sep="") %>% print
            
            # Making Output DF
            temp_output_df %>% colnames -> temp_output_df_colnames
            temp_output_df[1,] %>% unlist %>% as.character %>% 
              paste(temp_output_df_colnames,.,sep=" ") %>% 
              gsub(" ","_",.) -> colnames(temp_output_df)
            
            temp_output_df %>% .[,-1] %>% rev %>% .[,-1] %>% rev -> temp_output_df
            temp_output_df %<>% .[-1,] 
            temp_output_df$SYMBOL <- temp_Symbol
            temp_output_df$EXPIRY <- temp_Expiry
            temp_output_df$DATE_TIME_ZONE <- temp_Date_Time_Z
            
            # Save
            temp_output_df %>% write.csv(.,file=temp_Output_filename,row.names = F,na="")
          } # End of 'if(nrow(temp_output_df) > 0)'
          
        }else{
          temp_Output_filename %>% paste0(.," already present.",sep="")
        } # End of 'if(!temp_Output_filename %in% Output_Folder_Files)'
      } # End of 'for(i_Expiry_Dates_NSE_Format in 1:length(temp_Expiry_Dates_NSE_Format))'
      
      # Some pause step here after each ticker
      While_Pause_Time_Seconds %>% Sys.sleep  
    } # End of 'for(i_All_Symbols_Today in 1:length(All_Symbols_Today))'
    
  } # End of 'if((Day_Today %in% c("Saturday","Sunday")) | (Todays_Date %in% Muhurat_Exception_YYYYMMDD))'
  
} # End of 'while(TRUE)'

#################################################################################
#################################################################################