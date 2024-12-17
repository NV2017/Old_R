#################################################################################
#################################### Goals ######################################

# Date: 2020-December-10
# Author: Arunabha Sarkar

# Goals: Get minute level NSE all live futures & Option Chain data if possible
# File Name: Minute_Level_NSE_Futures_Option_Chain

#################################################################################
#################################################################################

#################################################################################
##################### Initializing and loading Libraries ########################

library(XML)
library(dplyr)
library(rvest)
library(stringr)
library(magrittr)

#################################################################################
#################################################################################

#################################################################################
################## Set Directories & other Hyperparameters ######################

Time_Break_Next_Page_Seconds <- 1

Output_Folder_Futures <- "Minute_Level_NSE_Futures"

"C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% paste0("/NSE FnO Bhav Copies",sep='') -> FnO_Bhav_Database

Central_Database %>% setwd

if(!file.exists(Output_Folder_Futures))
{
  Output_Folder_Futures %>% paste0("Creating '",.,"' directory.",sep="") %>% print
  dir.create(file.path(Central_Database, Output_Folder_Futures))
}else{
  Output_Folder_Futures %>% paste0("Directory '",.,"' already exists.",sep="") %>% print
} # End of 'if(!file.exists(Output_Folder_Futures))'

Central_Database %>% paste0(.,"/",Output_Folder_Futures,sep='') -> Output_Folder_Futures

url_nifty <- "https://www.nseindia.com/live_market/dynaContent/live_watch/fomwatchsymbol.jsp?key=NIFTY&Fut_Opt=Futures"

Part1_Futures_URL <- "https://www1.nseindia.com/live_market/dynaContent/live_watch/fomwatchsymbol.jsp?key="
Part2_Futures_URL <- "&Fut_Opt=Futures"

Market_Open_Time <- "0845"
Market_Close_Time <- "1715"
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

Output_Filename_Extention_Futures <- "_Live_Futures_"

Market_Lot_Foldername <- "NSE_Market_Lots"

Market_Holiday_Foldername <- "NSE_Market_Holidays"

Central_Database %>% setwd

if(!file.exists(Market_Holiday_Foldername))
{
  Market_Holiday_Foldername %>% paste0("Creating '",.,"' directory.",sep="") %>% print
  dir.create(file.path(Central_Database, Market_Holiday_Foldername))
}else{
  Market_Holiday_Foldername %>% paste0("Directory '",.,"' already exists.",sep="") %>% print
} # End of 'if(!file.exists(Market_Holiday_Foldername))'

Central_Database %>% paste0("/",Market_Holiday_Foldername,sep='') -> Market_Holiday_Database

Market_Holiday_Filename_Extention <- "_Market_Holiday.csv"

Market_Lot_URL <- "https://archives.nseindia.com/content/fo/fo_mktlots.csv"

url_NSE_trading_holidays <- "https://www.nseindia.com/products-services/equity-derivatives-timings-holidays"

url_NSE_trading_holidays_xpath <- "/html/body/div[6]/div/section/div/div/div/div/div/div/div[1]/div[4]/div[2]/div[2]/div[1]/div/table[1]"

if(!file.exists(Market_Lot_Foldername))
{
  Market_Lot_Foldername %>% paste0("Creating '",.,"' directory.",sep="") %>% print
  dir.create(file.path(Central_Database, Market_Lot_Foldername))
}else{
  Market_Lot_Foldername %>% paste0("Directory '",.,"' already exists.",sep="") %>% print
} # End of 'if(!file.exists(Market_Lot_Foldername))'

Central_Database %>% paste0("/",Market_Lot_Foldername,sep='') -> Market_Lot_Database

Output_Folder_Name_Option_Chain <- "Minute_Level_NSE_Option_Chain"

Central_Database %>% setwd

if(!file.exists(Output_Folder_Name_Option_Chain))
{
  Output_Folder_Name_Option_Chain %>% paste0("Creating '",.,"' directory.",sep="") %>% print
  dir.create(file.path(Central_Database, Output_Folder_Name_Option_Chain))
}else{
  Output_Folder_Name_Option_Chain %>% paste0("Directory '",.,"' already exists.",sep="") %>% print
} # End of 'if(!file.exists(Output_Folder_Name_Option_Chain))'

Central_Database %>% paste0(.,"/",Output_Folder_Name_Option_Chain,sep='') -> Output_Folder_Option_Chain

Output_Filename_Extention_Option_Chain <- "_Live_Option_Chain_"

Part1_Option_Chain_URL <- "https://www1.nseindia.com/live_market/dynaContent/live_watch/option_chain/optionKeys.jsp?segmentLink=17&instrument=OPTSTK&symbol="
Part1_Option_Chain_Index_URL <- "https://www1.nseindia.com/live_market/dynaContent/live_watch/option_chain/optionKeys.jsp?segmentLink=17&instrument=OPTIDX&symbol="
Part3_Option_Chain_URL <- "&date="

#################################################################################
#################################################################################

#################################################################################
########################## Function to find holidays ############################

NSE_Trading_Holidays_DF <- function()
{
  temp_holidays_DF <- c()
  
  # Check for current holiday dates
  url_NSE_trading_holidays %>% read_html%>% 
    html_nodes(xpath = url_NSE_trading_holidays_xpath) %>% 
    html_table(header = NA, trim = TRUE, fill = FALSE, dec = ".") %>% 
    .[[1]] -> temp_holidays_DF
  
  return(temp_holidays_DF)
} # End of 'NSE_Trading_Holidays <- function()'

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
  # Also pause if trading holiday
  
  temp_NSE_Trading_Holidays <- c()
  
  Market_Holiday_Database %>% setwd
  list.files() -> Market_Holiday_Database_files
  Todays_Date %>% paste0(.,Market_Holiday_Filename_Extention,sep="") -> temp_Market_Holiday_file
  if(temp_Market_Holiday_file %in% Market_Holiday_Database_files)
  {
    temp_Market_Holiday_file %>% read.csv %>% select(Date) %>% unlist %>% 
      as.character %>% as.Date("%d-%b-%Y") %>% format("%Y%m%d") %>% 
      as.character -> temp_NSE_Trading_Holidays
  }else{
    temp_NSE_Trading_Holidays_DF <- NSE_Trading_Holidays_DF()
    
    temp_NSE_Trading_Holidays_DF %>% select(Date) %>% unlist %>% 
      as.character %>% as.Date("%d-%b-%Y") %>% format("%Y%m%d") %>% 
      as.character -> temp_NSE_Trading_Holidays
    
    Market_Holiday_Database %>% setwd
    temp_NSE_Trading_Holidays_DF %>% 
      write.csv(.,file = temp_Market_Holiday_file,row.names = F, na="")
  } # End of 'if(temp_Market_Holiday_file %in% Market_Holiday_Database_files)'
  
  Sys.Date() %>% weekdays -> Day_Today
  if((Day_Today %in% c("Saturday","Sunday") & (!Todays_Date %in% Muhurat_Exception_YYYYMMDD)) | 
     (Todays_Date %in% temp_NSE_Trading_Holidays))
  {
    # Pause till next morning start
    Sys.Date() %>% {.+1} -> Tomorrows_Date
    
    Tomorrows_Date %>% format("%Y%m%d") -> Tomorrows_Date_YYYYMMDD
    
    if(Tomorrows_Date_YYYYMMDD %in% Muhurat_Exception_YYYYMMDD)
    {
      Muhurat_Market_Open_Time -> tomorrow_Market_Open_Time
    }else{
      Market_Open_Time -> tomorrow_Market_Open_Time
    } # End of 'if(Tomorrows_Date_YYYYMMDD %in% Muhurat_Exception_YYYYMMDD)'
    
    Tomorrows_Date %>% as.character %>% paste0(.," ",tomorrow_Market_Open_Time,"00",sep="") %>% 
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
    
    # Check for all tickers in FnO from Market Lot csv file from NSE
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
    
    # i_All_Symbols_Today <- 1
    for(i_All_Symbols_Today in 1:length(All_Symbols_Today))
    {
      i_All_Symbols_Today %>% All_Symbols_Today[.] -> temp_Symbol
      print("Getting Live Futures data for temp Symbol")
      #### Getting Live Futures data for temp Symbol
      temp_Symbol %>% gsub("&","%26",.) %>% 
        paste0(Part1_Futures_URL,.,Part2_Futures_URL,sep="") -> temp_Symbol_URL_Futures
      
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
            print("Catching Live Futures data for temp Symbol")
            temp_Symbol_URL_Futures %>% read_html -> temp_HTML_Futures
            No_Problem_Getting_NSE_URL <- TRUE
          },
          error = function(e){ 
            print("Error Live Futures data for temp Symbol")
            e %>% paste0("Error: ",.,sep="") %>% print
            No_Problem_Getting_NSE_URL <- FALSE
          },
          warning = function(w){
            print("Warning Live Futures data for temp Symbol")
            w %>% paste0("Warning: ",.,sep="") %>% print
            No_Problem_Getting_NSE_URL <- FALSE
          }
        )
      } # End of 'while(!No_Problem_Getting_NSE_URL)'
      if (query_counter > 10)
      {
        next
      } # End of 'if (query_counter > 10)'
      
      # temp_Symbol_Expiry_URL %>% read_html -> temp_HTML_Futures
      temp_HTML_Futures %>% html_nodes("#wrapper_btm") %>% .[1] %>% 
        as.character -> temp_Proto_Date_Time_Futures
      temp_Proto_Date_Time_Futures %>% gregexpr("As on ",.) %>% 
        .[[1]] %>% .[1] -> temp_start_Futures
      temp_Proto_Date_Time_Futures %>% gregexpr(" IST",.) %>% 
        .[[1]] %>% .[1] -> temp_end_Futures
      temp_Proto_Date_Time_Futures %>% 
        substr(.,temp_start_Futures+6,temp_end_Futures-1) -> temp_Date_Time_Futures_Futures
      
      # Making filename using 'temp_Date_Time_Futures_Futures' & ''
      temp_Date_Time_Futures_Futures %>% as.POSIXct(format="%b, %d %Y %H:%M:%S") %T>% {.->>temp_Date_Time_Futures_Z} %>% 
        as.character %>% gsub("-","",.) %>% gsub(" ","",.) %>% gsub(":","",.) %>% 
        paste0(.,Output_Filename_Extention_Futures,temp_Symbol,".csv",sep="") -> temp_Output_filename_Futures
      
      temp_Date_Time_Futures_Z %>% as.character %>% paste0(.," IST",sep="") -> temp_Date_Time_Futures_Z
      
      Output_Folder_Futures %>% setwd
      list.files() -> Output_Folder_Futures_Files
      if(!temp_Output_filename_Futures %in% Output_Folder_Futures_Files)
      {
        # Make output DF, then save
        temp_HTML_Futures %>% html_table(., fill=TRUE) -> Patch_temp_1
        
        Patch_temp_1_Boolean <- FALSE
        
        if(length(Patch_temp_1) > 1)
        {
          Patch_temp_1_Boolean <- TRUE
          # Note: Can't skip here, as the entire ticker will be skipped
        } # End of 'if(length(Patch_temp_1) > 1)'
        
        if(Patch_temp_1_Boolean)
        {
          Patch_temp_1  %>% .[[2]] -> temp_output_df_Futures
          if(nrow(temp_output_df_Futures) > 0)
          {
            "Construction/Saving live Futures, Symbol #" %>% 
              paste0(.,i_All_Symbols_Today," out of ",length(All_Symbols_Today),": ",sep="") %>% 
              paste0(.,temp_Symbol,"; Date Time: ",sep="") %>% 
              paste0(.,temp_Date_Time_Futures_Futures,sep="") %>% print
            
            # Making Output DF
            Futures_Colnames -> colnames(temp_output_df_Futures)
            
            temp_output_df_Futures$SYMBOL <- temp_Symbol
            temp_output_df_Futures$DATE_TIME_ZONE <- temp_Date_Time_Futures_Z
            
            # Save
            Output_Folder_Futures %>% setwd
            temp_output_df_Futures %>% write.csv(.,file=temp_Output_filename_Futures,row.names = F,na="")
          } # End of 'if(nrow(temp_output_df_Futures) > 0)'
        } # End of 'if(Patch_temp_1_Boolean)'
      }else{
        temp_Output_filename_Futures %>% paste0(.," already present.",sep="")
      } # End of 'if(!temp_Output_filename_Futures %in% Output_Folder_Futures_Files)'
      #### End Getting Live Futures data for temp Symbol
      print("Getting Live Option Chain data for temp Symbol")
      #### Getting Live Option Chain data for temp Symbol
      # Check for next three expiry dates
      # This should be in try catch
      url_nifty %>% read_html %>% html_nodes("#tab26Content > table:nth-child(1)") %>%
        html_table(header = NA, trim = TRUE, fill = FALSE, dec = ".") %>%
        as.data.frame %>% `colnames<-`(Futures_Colnames) %T>% {. ->> NIFTY_Futures} %>% 
        select(Expiry_Date) %>% unlist %>% as.character -> Expiry_Dates_NSE_Format
      
      # Selecting correct 'temp_Part1_Option_Chain_URL'; & Find correct expiry list as well
      if(grepl("NIFTY",temp_Symbol))
      {
        # Cutoff for BANKNIFTY & NIFTY is 10; 
        Part1_Option_Chain_Index_URL -> temp_Part1_Option_Chain_URL
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
        Part1_Option_Chain_URL -> temp_Part1_Option_Chain_URL
        Expiry_Dates_NSE_Format -> temp_Expiry_Dates_NSE_Format
      } # End of 'if(grepl("NIFTY",temp_Symbol))'
      
      ################## TAB
      
      # i_Expiry_Dates_NSE_Format <- 1
      for(i_Expiry_Dates_NSE_Format in 1:length(temp_Expiry_Dates_NSE_Format))
      {
        i_Expiry_Dates_NSE_Format %>% temp_Expiry_Dates_NSE_Format[.] -> temp_Expiry
        
        temp_Part1_Option_Chain_URL %>% paste0(.,temp_Symbol,sep="") %>% 
          paste0(.,Part3_Option_Chain_URL,temp_Expiry,sep="") -> temp_Symbol_Expiry_URL
        
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
              print("Catching Live Options data for temp Symbol")
              temp_Symbol_Expiry_URL %>% read_html -> temp_HTML_Option_Chain
              No_Problem_Getting_NSE_URL <- TRUE
            },
            error = function(e){ 
              print("Error Live Options data for temp Symbol")
              e %>% paste0("Error: ",.,sep="") %>% print
              No_Problem_Getting_NSE_URL <- FALSE
            },
            warning = function(w){
              print("Warning Live Options data for temp Symbol")
              w %>% paste0("Warning: ",.,sep="") %>% print
              No_Problem_Getting_NSE_URL <- FALSE
            }
          )
        } # End of 'while(!No_Problem_Getting_NSE_URL)'
        if (query_counter > 10)
        {
          next
        } # End of 'if (query_counter > 10)'
        
        # temp_Symbol_Expiry_URL %>% read_html -> temp_HTML_Option_Chain
        temp_HTML_Option_Chain %>% html_nodes("#wrapper_btm") %>% .[1] %>% 
          as.character -> temp_Proto_Date_Time_Option_Chain
        
        # Patch
        if(length(temp_Proto_Date_Time_Option_Chain) == 0)
        {
          next
        } # End of 'if(length(temp_Proto_Date_Time_Option_Chain) == 0)'
        #
        
        temp_Proto_Date_Time_Option_Chain %>% gregexpr("As on ",.) %>% 
          .[[1]] %>% .[1] -> temp_start_Option_Chain
        temp_Proto_Date_Time_Option_Chain %>% gregexpr(" IST<a> <img",.) %>% 
          .[[1]] %>% .[1] -> temp_end_Option_Chain
        temp_Proto_Date_Time_Option_Chain %>% 
          substr(.,temp_start_Option_Chain+6,temp_end_Option_Chain-1) -> temp_Date_Time_Option_Chain
        
        # Making filename using 'temp_Date_Time_Option_Chain', 'temp_Expiry' & 'temp_Symbol'
        temp_Date_Time_Option_Chain %>% as.POSIXct(format="%b %d, %Y %H:%M:%S") %T>% {.->>temp_Date_Time_Option_Chain_Z} %>% 
          as.character %>% gsub("-","",.) %>% gsub(" ","",.) %>% gsub(":","",.) %>% 
          paste0(.,Output_Filename_Extention_Option_Chain,temp_Symbol,
                 "_Expiry_",temp_Expiry,".csv",sep="") -> temp_Output_filename_Option_Chain
        
        temp_Date_Time_Option_Chain_Z %>% as.character %>% paste0(.," IST",sep="") -> temp_Date_Time_Option_Chain_Z
        
        Output_Folder_Option_Chain %>% setwd
        list.files() -> Output_Folder_Option_Chain_Files
        if(!temp_Output_filename_Option_Chain %in% Output_Folder_Option_Chain_Files)
        {
          # Make output DF, then save
          temp_HTML_Option_Chain %>% html_table(., fill=TRUE) %>% .[[3]] -> temp_output_df_Option_Chain
          if(nrow(temp_output_df_Option_Chain) > 1)
          {
            "Construction/Saving live Option Chain, Symbol #" %>% 
              paste0(.,i_All_Symbols_Today," out of ",length(All_Symbols_Today),": ",sep="") %>% 
              paste0(.,temp_Symbol,";",sep="") %>% 
              paste0(.," Expiry: ",temp_Expiry," for date-time: ",sep="") %>% 
              paste0(.,temp_Date_Time_Option_Chain,sep="") %>% print
            
            # Making Output DF
            temp_output_df_Option_Chain %>% colnames -> temp_output_df_colnames_Option_Chain
            temp_output_df_Option_Chain[1,] %>% unlist %>% as.character %>% 
              paste(temp_output_df_colnames_Option_Chain,.,sep=" ") %>% 
              gsub(" ","_",.) -> colnames(temp_output_df_Option_Chain)
            
            temp_output_df_Option_Chain %>% .[,-1] %>% rev %>% .[,-1] %>% rev -> temp_output_df_Option_Chain
            temp_output_df_Option_Chain %<>% .[-1,] 
            temp_output_df_Option_Chain$SYMBOL <- temp_Symbol
            temp_output_df_Option_Chain$EXPIRY <- temp_Expiry
            temp_output_df_Option_Chain$DATE_TIME_ZONE <- temp_Date_Time_Option_Chain_Z
            
            # Save
            Output_Folder_Option_Chain %>% setwd
            temp_output_df_Option_Chain %>% write.csv(.,file=temp_Output_filename_Option_Chain,row.names = F,na="")
          } # End of 'if(nrow(temp_output_df_Option_Chain) > 0)'
        }else{
          temp_Output_filename_Option_Chain %>% paste0(.," already present.",sep="")
        } # End of 'if(!temp_Output_filename_Option_Chain %in% Output_Folder_Option_Chain_Files)'
      } # End of 'for(i_Expiry_Dates_NSE_Format in 1:length(temp_Expiry_Dates_NSE_Format))'
      #### End Getting Live Option Chain data for temp Symbol
      
      # Some pause step here after each ticker
      While_Pause_Time_Seconds %>% Sys.sleep  
    } # End of 'for(i_All_Symbols_Today in 1:length(All_Symbols_Today))'
    
  } # End of 'if((Day_Today %in% c("Saturday","Sunday") & (!Todays_Date %in% Muhurat_Exception_YYYYMMDD)) | 
    #            (Todays_Date %in% temp_NSE_Trading_Holidays))'
  
} # End of 'while(TRUE)'

#################################################################################
#################################################################################

#################################################################################
################################### Future ######################################

# Auto read Muhurat day and time

#################################################################################
#################################################################################