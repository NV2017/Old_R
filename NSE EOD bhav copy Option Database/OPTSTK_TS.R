#################################################################################
#################################### Goals ######################################

# Date: 2020-June-06
# Author: Arunabha Sarkar

# Goals: Make Options Index Time Series
# File Name: OPTSTK_TS

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

Threshold_Year <- 2011 # For IDX, this has to be 2007

Max_Number_of_Expiries_in_Database <- 3 # This is only for stocks, for index, '10' !

Percentages_from_spot <- c(-25,-20,-15,-10,-5,-4,-3,-2,2,3,4,5,10,15,20,25) # In addition to SPOT prices; 16 stikes max

"C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% paste0("/Latest_NSE_Index_Composition",
                            sep='') -> Latest_NSE_Index_Composition_Database

Central_Database %>% paste0("/Nifty 50 Historical Data Investing dot com",
                            sep='') -> Nifty50_Database  # To find out which day is open & NIFTY Spot

Central_Database %>% paste0("/NSE EQ Bhav Copies",sep='') -> EQ_Bhav_Database # To add spot value

Central_Database %>% paste0("/NSE FnO Bhav Copies",sep='') -> FnO_Bhav_Database # To get the raw data

Central_Database %>% paste0("/Symbols in EnFnO",sep='') -> Symbols_in_EnFnO_Database # To get the symbols

Central_Database %>% paste0("/OPTSTK_TS",sep='') -> OPTSTK_TS_Database

Central_Database %>% setwd()

if(!file.exists('OPTSTK_TS'))
{
  print("Creating 'OPTSTK_TS' directory.")
  dir.create(file.path(Central_Database, 'OPTSTK_TS'))
}else{
  print("'OPTSTK_TS' directory already exists.")
} # End of 'if(!file.exists('OPTSTK_TS'))'

#################################################################################
#################################################################################

#################################################################################
##################### 'OPTSTK_TS' database column names #########################

Super_df_colnames_CE <- c("Date_YYYYMMDD_C","INSTRUMENT_C","SYMBOL_C", "OPEN_SPOT_C","HIGH_SPOT_C",
                          "LOW_SPOT_C","CLOSE_SPOT_C","LAST_SPOT_C","TOTTRDVAL_SPOT_C","STRIKE_CE_ATM",
                          "OPEN_CE_ATM","HIGH_CE_ATM","LOW_CE_ATM","CLOSE_CE_ATM","SETTLE_PR_CE_ATM",
                          "CONTRACTS_CE_ATM","VAL_INLAKH_CE_ATM","OPEN_INT_CE_ATM")
# Making colnames for CE, expiry
# i_3 <- 1
for (i_3 in 1:Max_Number_of_Expiries_in_Database)
{
  i_3 %>% paste0("CE_Expiry_",.,sep="") -> temp_Expiry_column
  
  # Looping over different CE strikes
  # i_4 <- 16
  for (i_4 in 1:length(Percentages_from_spot))
  {
    i_4 %>% Percentages_from_spot[.] %>% abs -> temp_percentage
    type <- c()
    Plus_Minus <- c()
    if(i_4 %>% Percentages_from_spot[.] >= 0)
    {
      Plus_Minus <- "Plus"
    }else{
      Plus_Minus <- "Minus"
    }# End of 'if(i_4 %>% Percentages_from_spot[.] >= 0)'
    type <- paste0("CE_Expiry_",i_3,"_Strike_", temp_percentage,"p_",Plus_Minus,"_Spot",sep='')
    
    c("STRIKE_PR","OPEN","HIGH","LOW","CLOSE","SETTLE_PR","CONTRACTS","VAL_INLAKH","OPEN_INT") %>% 
      paste0("_CE_Expiry_",i_3,"_Strike_", temp_percentage,"p_",Plus_Minus,"_Spot",sep='') -> temp_names
    
    temp_names %>% c(temp_Expiry_column,.) -> temp_Expiry_column
  } # End of for loop over 'Percentages_from_spot'
  temp_Expiry_column %>% c(Super_df_colnames_CE,.) -> Super_df_colnames_CE
} # End of for loop over 'expiries'

Super_df_colnames_PE <- c("Date_YYYYMMDD_P","INSTRUMENT_P","SYMBOL_P","OPEN_SPOT_P","HIGH_SPOT_P",
                          "LOW_SPOT_P","CLOSE_SPOT_P","LAST_SPOT_P","TOTTRDVAL_SPOT_P","STRIKE_PE_ATM",
                          "OPEN_PE_ATM","HIGH_PE_ATM","LOW_PE_ATM","CLOSE_PE_ATM","SETTLE_PR_PE_ATM",
                          "CONTRACTS_PE_ATM","VAL_INLAKH_PE_ATM","OPEN_INT_PE_ATM")
# Making colnames for PE, expiry
# i_5 <- 1
for (i_5 in 1:Max_Number_of_Expiries_in_Database)
{
  i_5 %>% paste0("PE_Expiry_",.,sep="") -> temp_Expiry_column
  
  # Looping over different PE strikes
  # i_6 <- 1
  for (i_6 in 1:length(Percentages_from_spot))
  {
    i_6 %>% Percentages_from_spot[.] %>% abs -> temp_percentage
    type <- c()
    Plus_Minus <- c()
    if(i_6 %>% Percentages_from_spot[.] >= 0)
    {
      Plus_Minus <- "Plus"
    }else{
      Plus_Minus <- "Minus"
    }# End of 'if(i_6 %>% Percentages_from_spot[.] >= 0)'
    type <- paste0("PE_Expiry_",i_5,"_Strike_", temp_percentage,"p_",Plus_Minus,"_Spot",sep='')
    
    c("STRIKE_PR","OPEN","HIGH","LOW","CLOSE","SETTLE_PR","CONTRACTS","VAL_INLAKH","OPEN_INT") %>% 
      paste0("_PE_Expiry_",i_5,"_Strike_", temp_percentage,"p_",Plus_Minus,"_Spot",sep='') -> temp_names
    
    temp_names %>% c(temp_Expiry_column,.) -> temp_Expiry_column
  } # End of for loop over 'Percentages_from_spot'
  temp_Expiry_column %>% c(Super_df_colnames_PE,.) -> Super_df_colnames_PE
} # End of for loop over 'expiries'

# Combining 'Super_df_colnames_CE' & 'Super_df_colnames_CE'

Super_df_colnames_CE %>% c(.,Super_df_colnames_PE) -> Super_df_colnames

#################################################################################
#################################################################################

#################################################################################
######################## Preferred 'temp_FnO' colanmes ##########################

temp_FnO_colnames <- c("INSTRUMENT","SYMBOL","EXPIRY_DT","STRIKE_PR","OPTIONTYPE",
                       "OPEN","HIGH","LOW","CLOSE","SETTLE_PR","CONTRACTS","VAL_INLAKH",
                       "OPEN_INT","CHG_IN_OI","TIMESTAMP")

temp_EQ_colnames <- c("SYMBOL","SERIES","OPEN","HIGH","LOW","CLOSE","LAST","PREVCLOSE",
                      "TOTTRDQTY","TOTTRDVAL","TIMESTAMP")

#################################################################################
#################################################################################

#################################################################################
############################ Finding/Making OPTSTK ##############################

# Find latest date of NIFTY50
Nifty50_Database %>% setwd

list.files() %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>% 
  as.Date(format="%Y%m%d") %>% which.max %>% list.files()[.] %>% 
  read.csv %>% {. ->> Nifty_Data} %>% select(Date) %>% unlist %>% as.character %>% 
  as.Date(format="%b %d, %Y") %>% max %>% format("%Y%m%d") -> Max_Nifty_Date
colnames(Nifty_Data) <- c("Date","Last","Open","High","Low","Volume","Change")
Nifty_Data %>% select(Date) %>% unlist %>% as.character %>% as.Date(format("%b %d, %Y")) %>% 
  format("%Y%m%d") -> Nifty_Data$Date

# Finding the list of OPTSTK, same as FUTSTK
Symbols_in_EnFnO_Database %>% setwd
Symbols_in_EnFnO_Database_File_List <- list.files(pattern = "FUTSTK")

if(length(Symbols_in_EnFnO_Database_File_List) > 0)
{
  Symbols_in_EnFnO_Database_File_List %>% strsplit(.,split = ' ') %>% map(1) %>% 
    unlist %>% as.Date(format="%Y%m%d") %>% which.max %>% 
    Symbols_in_EnFnO_Database_File_List[.] %T>% {. ->> Latest_File} %>% 
    read.csv %>% select(Unique_FUTSTK_Symbols) %>% unlist %>% 
    as.character -> Latest_File_Symbols
  
  Latest_File %>% strsplit(split = '_') %>% .[[1]] %>% .[1] -> Latest_File_Date
}else{
  Latest_File_Symbols <- c()
  Latest_File_Date <- c()
  Latest_File <- c()
} # End of 'if(length(Symbols_in_EnFnO_Database_File_List) > 0)'

# Latest_N50 %>% c(.,Latest_File_Symbols) %>% unique -> Latest_File_Symbols

#### Arranging 'Latest_File_Symbols' with Nifty50 first

Latest_NSE_Index_Composition_Database %>% setwd

list.files() -> Latest_NSE_Index_Composition_Database_Files

"Nifty50.csv" %>% grep(Latest_NSE_Index_Composition_Database_Files) %>% 
  Latest_NSE_Index_Composition_Database_Files[.] -> All_Nifty_50_candidates

All_Nifty_50_candidates %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>%
  as.Date(format="%Y%m%d") %>% which.max %>% All_Nifty_50_candidates[.] %>% 
  read.csv %>% select(Symbol) %>% unlist %>% as.character -> Latest_Nifty_50_tickers

#######################################################

if(length(Latest_File_Symbols) > 0)
{
  Latest_Nifty_50_tickers %>% c(.,Latest_File_Symbols) %>% unique -> Latest_File_Symbols
  # Looping over each Symbol
  # i_1 <- 1; i_1 <- 2
  for(i_1 in 1:length(Latest_File_Symbols))
  {
    i_1 %>% Latest_File_Symbols[.] %>% {. ->> temp_Symbol} %>% 
      paste0("Checking ",.,sep='') %>% print
    
    # Checking if this Symbol exists in the OPTSTK_TS folder
    # Returning required 'Relevant_files'
    OPTSTK_TS_Database %>% setwd
    
    temp_Symbol %>% paste0(.,"_Options.csv",sep='') -> temp_filename
    
    Prior_temp_Symbol_file_found <- FALSE
    Overwrite_Required <- FALSE
    
    if(!(temp_filename %in% list.files()))
    {
      print(paste0("No prior file found for: ", temp_Symbol, sep=''))
      
      # Finding the relevant FnO for this symbol
      FnO_Bhav_Database %>% setwd
      list.files() -> All_FnO_Bhav_Copy_Files
      
      All_FnO_Bhav_Copy_Files %>% as.Date(format="fo%d%b%Ybhav.csv") -> All_FnO_Bhav_Copy_Dates
      
      All_FnO_Bhav_Copy_Dates %>% as.Date(format="%Y%m%d") %>% format("%Y") %>% 
        as.integer %>% {.>=Threshold_Year} %>% All_FnO_Bhav_Copy_Files[.] %>%
        as.Date(format="fo%d%b%Ybhav.csv") %>% sort %>% 
        format("fo%d%b%Ybhav.csv") -> Relevant_files
      
      # Finding optimal start points for 'Relevant_files'
      Relevant_files %>% length %>% seq(1,.,252) %>% c(.,length(Relevant_files)) %>% 
        unique -> Trial_points_index_Relevant_files
      
      # Finding correct starting point of 'Relevant_files'
      if(length(Trial_points_index_Relevant_files) > 1)
      {
        Starting_Index_Relevant_files <- 1
        # i_r <- 1
        for (i_r in 1:length(Trial_points_index_Relevant_files))
        {
          # print(i_r)
          FnO_Bhav_Database %>% setwd
          
          i_r %>% Trial_points_index_Relevant_files[.] %>% Relevant_files[.] %>% 
            read.csv %>% select(SYMBOL) %>% unlist %>% as.character %>% 
            unique -> temp_relevant_symbols
          
          if (temp_Symbol %in% temp_relevant_symbols)
          {
            if(i_r == 1)
            {
              break
            }else{
              i_r %>% {.-1} %>% 
                Trial_points_index_Relevant_files[.] -> Starting_Index_Relevant_files
              
              break
            } # End of 'if(i_r == 1)'
          } # End of 'if (temp_Symbol %in% temp_relevant_symbols)'
        } # End of 'for (i_r in 1:length(Trial_points_index_Relevant_files))'
      }# End of Finding correct starting point of 'Relevant_files'
      
      # Finding correct ending point of 'Relevant_files'
      if(length(Trial_points_index_Relevant_files) > 1)
      {
        Ending_Index_Relevant_files <- length(Relevant_files)
        # i_r <- length(Trial_points_index_Relevant_files) - 10
        for (i_r in length(Trial_points_index_Relevant_files):1)
        {
          # print(i_r)
          FnO_Bhav_Database %>% setwd
          
          i_r %>% Trial_points_index_Relevant_files[.] %>% Relevant_files[.] %>% 
            read.csv %>% select(SYMBOL) %>% unlist %>% as.character %>% 
            unique -> temp_relevant_symbols
          
          if (temp_Symbol %in% temp_relevant_symbols)
          {
            if(i_r == length(Trial_points_index_Relevant_files))
            {
              break
            }else{
              
              i_r %>% {.+1} %>% 
                Trial_points_index_Relevant_files[.] -> Ending_Index_Relevant_files
              break
            } # End of 'if(i_r == length(Trial_points_index_Relevant_files))'
          } # End of 'if (temp_Symbol %in% temp_relevant_symbols)'
        } # End of 'for (i_r in length(Trial_points_index_Relevant_files):1)'
      }# End of Finding correct ending point of 'Relevant_files'
      (Starting_Index_Relevant_files:Ending_Index_Relevant_files) %>% 
        Relevant_files[.] -> Relevant_files
    }else{
      # When prior file is found
      print(paste0("Prior file found for: ", temp_Symbol, sep=''))
      Prior_temp_Symbol_file_found <- TRUE
      OPTSTK_TS_Database %>% setwd
      
      temp_Symbol %>% paste0(.,"_Options.csv",sep='') -> temp_filename
      
      if(length(temp_filename) > 0)
      {
        temp_filename %>% read.csv -> temp_old_file
        temp_old_file %>% select(Date_YYYYMMDD_C) %>% unlist %>% as.character %>% 
          as.numeric %>% max -> max_date_in_temp_old_file
        
        if(max_date_in_temp_old_file < Max_Nifty_Date)
        {
          # Requires update by overwriting
          Overwrite_Required <- TRUE
          # Finding all relevant files
          FnO_Bhav_Database %>% setwd
          list.files() -> All_FnO_Bhav_Copy_Files
          
          All_FnO_Bhav_Copy_Files %>% as.Date(format="fo%d%b%Ybhav.csv") -> All_FnO_Bhav_Copy_Dates
          
          All_FnO_Bhav_Copy_Dates %>% format("%Y%m%d") %>% as.numeric %>% 
            { . > max_date_in_temp_old_file} %>% All_FnO_Bhav_Copy_Dates[.] %>% 
            as.Date(format="%Y-%m-%d") %>% format("fo%d%b%Ybhav.csv") -> Relevant_files
          
          Relevant_files %>% length %>% seq(1,.,252) %>% c(.,length(Relevant_files)) %>% 
            unique -> Trial_points_index_Relevant_files
          
          # Check approx max fo bhav copy with this ticker
          # Finding correct ending point of 'Relevant_files'
          if(length(Trial_points_index_Relevant_files) > 0)
          {
            Ending_Index_Relevant_files <- length(Relevant_files)
            Atleast_Found_One <- FALSE
            # i_r_2 <- length(Trial_points_index_Relevant_files) - 10
            for (i_r_2 in length(Trial_points_index_Relevant_files):1)
            {
              # print(i_r)
              FnO_Bhav_Database %>% setwd
              
              i_r_2 %>% Trial_points_index_Relevant_files[.] %>% Relevant_files[.] %>% 
                read.csv %>% select(SYMBOL) %>% unlist %>% as.character %>% 
                unique -> temp_relevant_symbols
              
              if (temp_Symbol %in% temp_relevant_symbols)
              {
                if(i_r_2 == 1)
                {
                  Atleast_Found_One <- TRUE
                  break
                }else{
                  i_r_2 %>% {.+1} %>% 
                    Trial_points_index_Relevant_files[.] -> Ending_Index_Relevant_files
                  Atleast_Found_One <- TRUE
                  break
                } # End of 'if(i_r_2 == 1)'
              } # End of 'if (temp_Symbol %in% temp_relevant_symbols)'
            } # End of 'for (i_r_2 in length(Trial_points_index_Relevant_files):1)'
            
            if(!Atleast_Found_One)
            {
              Overwrite_Required <- FALSE
              Relevant_files <- c()
              print(paste0("Prior file for ", temp_Symbol, " is upto date", sep=''))
              next
            } # End of 'if(!Atleast_Found_One)'
          }# End of Finding correct ending point of 'Relevant_files'
        }else{
          Overwrite_Required <- FALSE
          Relevant_files <- c()
          print(paste0("Prior file for ", temp_Symbol, " is upto date", sep=''))
          next
        } # End of 'if(max_date_in_temp_old_file < Max_Nifty_Date)'
      }# End of 'Checking temp_filename existance'
    }# End of 'if(!(temp_filename %in% list.files()))
    
    # If old data is present, get it. If not, prepare the following dataframe
    if(Overwrite_Required)
    {
      OPTSTK_TS_Database %>% setwd
      
      temp_Symbol %>% paste0(.,"_Options.csv",sep='') -> temp_filename
      temp_filename %>% read.csv -> temp_Symbol_Data
    }else{ # When over write is not required, i.e. starting from scratch
      Super_df_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% 
        data.frame(stringsAsFactors = F) %>% 
        `colnames<-`(Super_df_colnames) -> temp_Symbol_Data
    } # End of checking for overwrite
    
    # Now we have 'Relevant_files'. Looping over this for 'temp_Symbol'
    if (length(Relevant_files) > 0)
    {
      # i_2 <- 1; i_2 <- 1330; i_2 <- 1320; i_2 <- 2079; i_2 <- length(Relevant_files) - 1
      for (i_2 in 1:length(Relevant_files))
      {
        print(paste0("Processing #",i_2, " out of ", length(Relevant_files)," for '", 
                     temp_Symbol,"' #",i_1, " of ",length(Latest_File_Symbols),
                     " symbols. Present rows for this symbol: ",
                     nrow(temp_Symbol_Data),sep=''))
        
        Super_df_colnames %>% length %>% matrix(data = NA,nrow = 1, ncol = .) %>% 
          data.frame(stringsAsFactors = F) %>% `colnames<-`(Super_df_colnames) -> temp_df
        
        FnO_Bhav_Database %>% setwd
        i_2 %>% Relevant_files[.] %>% read.csv -> temp_FnO
        i_2 %>% Relevant_files[.] %>% as.Date(format="fo%d%b%Ybhav.csv") -> temp_relevant_file_date
        
        temp_FnO %>% colnames %>% {which(.=="TIMESTAMP")} %>% seq(1,.,1) %>% 
          temp_FnO[,.] -> temp_FnO
        
        temp_FnO_colnames -> colnames(temp_FnO)
        
        # Correcting date format for 'temp_FnO' as in case of 14th May 2012
        if (dim(temp_FnO)[1] > 0)
        {
          temp_FnO %>% select(EXPIRY_DT) %>% unlist %>% as.character %>% 
            .[1] -> temp_first_date_entry
          if(nchar(temp_first_date_entry[1]) == 9)
          {
            temp_FnO %>% select(EXPIRY_DT) %>% unlist %>% as.character %>% 
              as.Date(format="%d-%b-%y") %>% format("%d-%b-%Y") -> temp_FnO$EXPIRY_DT
            
            temp_FnO %>% select(TIMESTAMP) %>% unlist %>% as.character %>% 
              as.Date(format="%d-%b-%y") %>% format("%d-%b-%Y") -> temp_FnO$TIMESTAMP
            
          }# End of 'if(nchar(temp_first_date_entry[1]) == 9)'
          
        } # End of 'if (dim(temp_FnO)[1] > 0)'
        
        temp_FnO %>% filter(INSTRUMENT == "OPTSTK") %>% 
          filter(SYMBOL == temp_Symbol) %>% 
          filter(OPTIONTYPE == "CE") -> temp_CE_FnO_of_temp_Symbol
        
        temp_FnO %>% filter(INSTRUMENT == "OPTSTK") %>% 
          filter(SYMBOL == temp_Symbol) %>% 
          filter(OPTIONTYPE == "PE") -> temp_PE_FnO_of_temp_Symbol
        
        if(nrow(temp_CE_FnO_of_temp_Symbol) > 0 &
           nrow(temp_PE_FnO_of_temp_Symbol) > 0)
        {
          ############################################################################################
          # Entry from EQ files into 'temp_df' for SPOT, temp date from 'temp_relevant_file_date'
          
          EQ_Bhav_Database %>% setwd
          list.files() -> All_EQ_filenames
          
          All_EQ_filenames %>% as.Date(format="cm%d%b%Ybhav.csv") %>% 
            {which(. == temp_relevant_file_date)} %>% All_EQ_filenames[.] -> temp_Spot_filename
          
          if (length(temp_Spot_filename) > 0)
          {
            temp_Spot_filename %>% read.csv -> temp_Spot_File
            
            temp_Spot_File %>% colnames %>% {which(.=="TIMESTAMP")} %>% seq(1,.,1) %>% 
              temp_Spot_File[,.] -> temp_Spot_File
            
            temp_Spot_File %>% `colnames<-`(temp_EQ_colnames) -> temp_Spot_File
          }else{
            print(paste0("Didn't find correct Spot EQ file named: ", temp_Spot_filename,sep=''))
            c() -> temp_Spot_File
          } # End of 'if (length(temp_Spot_filename) > 0)'

          if(length(temp_Spot_File) > 1)
          {
            temp_Spot_File %>% filter(SYMBOL == temp_Symbol) %>% filter(SERIES == "EQ") -> temp_EQ_relevant
            if(nrow(temp_EQ_relevant) > 0)
            {
              temp_EQ_relevant$TIMESTAMP %>% unlist %>% as.character %>% 
                as.Date(format="%d-%b-%Y") %>% format("%Y%m%d") -> temp_DATE_spot
              temp_DATE_spot_columns <- grep("Date_YYYYMMDD", colnames(temp_df))
              for (i_DATE in 1:length(temp_DATE_spot_columns))
              {
                temp_df[1,temp_DATE_spot_columns[i_DATE]] <- temp_DATE_spot
              } # End of 'for (i_DATE in 1:length(temp_DATE_spot_columns))'
              
              temp_EQ_relevant$SERIES %>% unlist %>% as.character -> temp_SERIES_spot
              temp_SERIES_columns <- grep("INSTRUMENT", colnames(temp_df))
              for (i_SERIES in 1:length(temp_SERIES_columns))
              {
                temp_df[1,temp_SERIES_columns[i_SERIES]] <- temp_SERIES_spot
              } # End of 'for (i_SERIES in 1:length(temp_SERIES_columns))'
              
              temp_EQ_relevant$SYMBOL %>% unlist %>% as.character -> temp_SYMBOL_spot
              temp_SYMBOL_columns <- grep("SYMBOL", colnames(temp_df))
              for (i_SYMBOL in 1:length(temp_SYMBOL_columns))
              {
                temp_df[1,temp_SYMBOL_columns[i_SYMBOL]] <- temp_SYMBOL_spot
              } # End of 'for (i_SYMBOL in 1:length(temp_SYMBOL_columns))'
              
              temp_EQ_relevant$OPEN -> temp_OPEN_spot
              temp_OPEN_columns <- grep("OPEN_SPOT", colnames(temp_df))
              for (i_OPEN in 1:length(temp_OPEN_columns))
              {
                temp_df[1,temp_OPEN_columns[i_OPEN]] <- temp_OPEN_spot
              } # End of 'for (i_OPEN in 1:length(temp_OPEN_columns))'
              
              temp_EQ_relevant$HIGH -> temp_HIGH_spot
              temp_HIGH_columns <- grep("HIGH_SPOT", colnames(temp_df))
              for (i_HIGH in 1:length(temp_HIGH_columns))
              {
                temp_df[1,temp_HIGH_columns[i_HIGH]] <- temp_HIGH_spot
              } # End of 'for (i_HIGH in 1:length(temp_HIGH_columns))'
              
              temp_EQ_relevant$LOW -> temp_LOW_spot
              temp_LOW_columns <- grep("LOW_SPOT", colnames(temp_df))
              for (i_LOW in 1:length(temp_LOW_columns))
              {
                temp_df[1,temp_LOW_columns[i_LOW]] <- temp_LOW_spot
              } # End of 'for (i_LOW in 1:length(temp_LOW_columns))'
              
              temp_EQ_relevant$CLOSE -> temp_CLOSE_spot
              temp_CLOSE_columns <- grep("CLOSE_SPOT", colnames(temp_df))
              for (i_CLOSE in 1:length(temp_CLOSE_columns))
              {
                temp_df[1,temp_CLOSE_columns[i_CLOSE]] <- temp_CLOSE_spot
              } # End of 'for (i_CLOSE in 1:length(temp_CLOSE_columns))'
              
              temp_EQ_relevant$LOW -> temp_LAST_spot
              temp_LAST_columns <- grep("LAST_SPOT", colnames(temp_df))
              for (i_LAST in 1:length(temp_LAST_columns))
              {
                temp_df[1,temp_LAST_columns[i_LAST]] <- temp_LAST_spot
              } # End of 'for (i_LAST in 1:length(temp_LAST_columns))'
              
              temp_EQ_relevant$TOTTRDVAL -> temp_TOTTRDVAL_spot
              temp_TOTTRDVAL_columns <- grep("TOTTRDVAL_SPOT", colnames(temp_df))
              for (i_TOTTRDVAL in 1:length(temp_TOTTRDVAL_columns))
              {
                temp_df[1,temp_TOTTRDVAL_columns[i_TOTTRDVAL]] <- temp_TOTTRDVAL_spot
              } # End of 'for (i_TOTTRDVAL in 1:length(temp_TOTTRDVAL_columns))'

            } # End of 'if(nrow(temp_EQ_relevant) > 0)'
          } # End of 'if(length(temp_Spot_File) > 1)'
          
          ############################################################################################
          
          ############################################################################################
          # CE ATM stuff, SPOT
          temp_relevant_file_date %>% as.Date(format="%Y-%m-%d") %>% format("%Y%m%d") -> temp_date
          
          if(!is.na(temp_CLOSE_spot) & is.numeric(temp_CLOSE_spot))
          {
            # Most recent expiry is always as top, but still, we need to still find it for ATM
            
            temp_FnO %>% filter(INSTRUMENT == "OPTSTK") %>% 
              filter(SYMBOL == temp_Symbol) %>% 
              filter(OPTIONTYPE == "CE") -> temp_CE_FnO_of_temp_Symbol
            
            temp_CE_FnO_of_temp_Symbol %>% select(EXPIRY_DT) %>% unlist %>% as.character %>% 
              as.Date(format="%d-%b-%Y") -> temp_CE_FnO_of_temp_Symbol_Expiry_Dates
            
            temp_CE_FnO_of_temp_Symbol_Expiry_Dates -> temp_CE_FnO_of_temp_Symbol$EXPIRY_DT
            
            temp_CE_FnO_of_temp_Symbol_Expiry_Dates %>% min -> temp_CE_FnO_of_temp_Symbol_Min_Expiry_Date
            
            temp_CE_FnO_of_temp_Symbol %>% 
              filter(EXPIRY_DT==temp_CE_FnO_of_temp_Symbol_Min_Expiry_Date) -> temp_CE_FnO_of_temp_Symbol_Nearest_Expiry
            
            temp_CE_FnO_of_temp_Symbol_Nearest_Expiry %>% 
              filter(STRIKE_PR >= temp_CLOSE_spot) %>%
              arrange_at("STRIKE_PR") -> temp_CE_FnO_of_temp_Symbol_Nearest_Expiry_ATM_Candidates
            
            if(nrow(temp_CE_FnO_of_temp_Symbol_Nearest_Expiry_ATM_Candidates) > 0)
            {
              # Fixing CE ATM here
              which(colnames(temp_df) == "STRIKE_CE_ATM") -> STRIKE_CE_ATM_col_num
              temp_CE_FnO_of_temp_Symbol_Nearest_Expiry_ATM_Candidates[1,"STRIKE_PR"] -> temp_df[1,STRIKE_CE_ATM_col_num]
              
              which(colnames(temp_df) == "OPEN_CE_ATM") -> OPEN_CE_ATM_col_num
              temp_CE_FnO_of_temp_Symbol_Nearest_Expiry_ATM_Candidates[1,"OPEN"] -> temp_df[1,OPEN_CE_ATM_col_num]
              
              which(colnames(temp_df) == "HIGH_CE_ATM") -> HIGH_CE_ATM_col_num
              temp_CE_FnO_of_temp_Symbol_Nearest_Expiry_ATM_Candidates[1,"HIGH"] -> temp_df[1,HIGH_CE_ATM_col_num]
              
              which(colnames(temp_df) == "LOW_CE_ATM") -> LOW_CE_ATM_col_num
              temp_CE_FnO_of_temp_Symbol_Nearest_Expiry_ATM_Candidates[1,"LOW"] -> temp_df[1,LOW_CE_ATM_col_num]
              
              which(colnames(temp_df) == "CLOSE_CE_ATM") -> CLOSE_CE_ATM_col_num
              temp_CE_FnO_of_temp_Symbol_Nearest_Expiry_ATM_Candidates[1,"CLOSE"] -> temp_df[1,CLOSE_CE_ATM_col_num]
              
              which(colnames(temp_df) == "SETTLE_PR_CE_ATM") -> SETTLE_PR_CE_ATM_col_num
              temp_CE_FnO_of_temp_Symbol_Nearest_Expiry_ATM_Candidates[1,"SETTLE_PR"] -> temp_df[1,SETTLE_PR_CE_ATM_col_num]
              
              which(colnames(temp_df) == "CONTRACTS_CE_ATM") -> CONTRACTS_CE_ATM_col_num
              temp_CE_FnO_of_temp_Symbol_Nearest_Expiry_ATM_Candidates[1,"CONTRACTS"] -> temp_df[1,CONTRACTS_CE_ATM_col_num]
              
              which(colnames(temp_df) == "VAL_INLAKH_CE_ATM") -> VAL_INLAKH_CE_ATM_col_num
              temp_CE_FnO_of_temp_Symbol_Nearest_Expiry_ATM_Candidates[1,"VAL_INLAKH"] -> temp_df[1,VAL_INLAKH_CE_ATM_col_num]
              
              which(colnames(temp_df) == "OPEN_INT_CE_ATM") -> OPEN_INT_CE_ATM_col_num
              temp_CE_FnO_of_temp_Symbol_Nearest_Expiry_ATM_Candidates[1,"OPEN_INT"] -> temp_df[1,OPEN_INT_CE_ATM_col_num]
              
            }# End of 'if(nrow(temp_CE_FnO_of_temp_Symbol_Nearest_Expiry_ATM_Candidates) > 0)'
          }# End of 'if(!is.na(temp_CLOSE_spot) & is.numeric(temp_CLOSE_spot))'
          
          ############################################################################################
          # PE ATM stuff, SPOT: temp_NIFTY_CLOSE
          temp_relevant_file_date %>% as.Date(format="%Y-%m-%d") %>% format("%Y%m%d") -> temp_date
          
          if(!is.na(temp_CLOSE_spot) & is.numeric(temp_CLOSE_spot))
          {
            # Most recent expiry is always as top, but still, we need to still find it for ATM
            temp_FnO %>% filter(INSTRUMENT == "OPTSTK") %>% 
              filter(SYMBOL == temp_Symbol) %>% 
              filter(OPTIONTYPE == "PE") -> temp_PE_FnO_of_temp_Symbol
            
            temp_PE_FnO_of_temp_Symbol %>% select(EXPIRY_DT) %>% unlist %>% as.character %>% 
              as.Date(format="%d-%b-%Y") -> temp_PE_FnO_of_temp_Symbol_Expiry_Dates
            
            temp_PE_FnO_of_temp_Symbol_Expiry_Dates -> temp_PE_FnO_of_temp_Symbol$EXPIRY_DT
            
            temp_PE_FnO_of_temp_Symbol_Expiry_Dates %>% min -> temp_PE_FnO_of_temp_Symbol_Min_Expiry_Date
            
            temp_PE_FnO_of_temp_Symbol %>% 
              filter(EXPIRY_DT==temp_PE_FnO_of_temp_Symbol_Min_Expiry_Date) -> temp_PE_FnO_of_temp_Symbol_Nearest_Expiry
            
            temp_PE_FnO_of_temp_Symbol_Nearest_Expiry %>% 
              filter(STRIKE_PR <= temp_CLOSE_spot) %>%
              arrange_at("STRIKE_PR", desc) -> temp_PE_FnO_of_temp_Symbol_Nearest_Expiry_ATM_Candidates
            
            if(nrow(temp_PE_FnO_of_temp_Symbol_Nearest_Expiry_ATM_Candidates) > 0)
            {
              # Fixing PE ATM here
              which(colnames(temp_df) == "STRIKE_PE_ATM") -> STRIKE_PE_ATM_col_num
              temp_PE_FnO_of_temp_Symbol_Nearest_Expiry_ATM_Candidates[1,"STRIKE_PR"] -> temp_df[1,STRIKE_PE_ATM_col_num]
              
              which(colnames(temp_df) == "OPEN_PE_ATM") -> OPEN_PE_ATM_col_num
              temp_PE_FnO_of_temp_Symbol_Nearest_Expiry_ATM_Candidates[1,"OPEN"] -> temp_df[1,OPEN_PE_ATM_col_num]
              
              which(colnames(temp_df) == "HIGH_PE_ATM") -> HIGH_PE_ATM_col_num
              temp_PE_FnO_of_temp_Symbol_Nearest_Expiry_ATM_Candidates[1,"HIGH"] -> temp_df[1,HIGH_PE_ATM_col_num]
              
              which(colnames(temp_df) == "LOW_PE_ATM") -> LOW_PE_ATM_col_num
              temp_PE_FnO_of_temp_Symbol_Nearest_Expiry_ATM_Candidates[1,"LOW"] -> temp_df[1,LOW_PE_ATM_col_num]
              
              which(colnames(temp_df) == "CLOSE_PE_ATM") -> CLOSE_PE_ATM_col_num
              temp_PE_FnO_of_temp_Symbol_Nearest_Expiry_ATM_Candidates[1,"CLOSE"] -> temp_df[1,CLOSE_PE_ATM_col_num]
              
              which(colnames(temp_df) == "SETTLE_PR_PE_ATM") -> SETTLE_PR_PE_ATM_col_num
              temp_PE_FnO_of_temp_Symbol_Nearest_Expiry_ATM_Candidates[1,"SETTLE_PR"] -> temp_df[1,SETTLE_PR_PE_ATM_col_num]
              
              which(colnames(temp_df) == "CONTRACTS_PE_ATM") -> CONTRACTS_PE_ATM_col_num
              temp_PE_FnO_of_temp_Symbol_Nearest_Expiry_ATM_Candidates[1,"CONTRACTS"] -> temp_df[1,CONTRACTS_PE_ATM_col_num]
              
              which(colnames(temp_df) == "VAL_INLAKH_PE_ATM") -> VAL_INLAKH_PE_ATM_col_num
              temp_PE_FnO_of_temp_Symbol_Nearest_Expiry_ATM_Candidates[1,"VAL_INLAKH"] -> temp_df[1,VAL_INLAKH_PE_ATM_col_num]
              
              which(colnames(temp_df) == "OPEN_INT_PE_ATM") -> OPEN_INT_PE_ATM_col_num
              temp_PE_FnO_of_temp_Symbol_Nearest_Expiry_ATM_Candidates[1,"OPEN_INT"] -> temp_df[1,OPEN_INT_PE_ATM_col_num]
              
            }# End of 'if(nrow(temp_CE_FnO_of_temp_Symbol_Nearest_Expiry_ATM_Candidates) > 0)'
          }# End of 'if(!is.na(temp_CLOSE_spot) & is.numeric(temp_CLOSE_spot))'
          
          ############################################################################################
          
          ############################################################################################
          # Entry from 'temp_CE_FnO_of_temp_Symbol' into 'temp_df' for different expiries and strikes
          # # Making 'temp_CE_FnO_of_temp_Symbol_Unique_Expiry' in ascending order
          temp_FnO %>% filter(INSTRUMENT == "OPTSTK") %>% 
            filter(SYMBOL == temp_Symbol) %>% 
            filter(OPTIONTYPE == "CE") -> temp_CE_FnO_of_temp_Symbol
          
          temp_CE_FnO_of_temp_Symbol %>% select(EXPIRY_DT) %>% unlist %>% as.character %>% 
            as.Date(format="%d-%b-%Y") %>% sort %>% unique -> temp_CE_FnO_of_temp_Symbol_Unique_Expiry
          
          if (length(temp_CE_FnO_of_temp_Symbol_Unique_Expiry) > Max_Number_of_Expiries_in_Database)
          {
            temp_CE_FnO_of_temp_Symbol_Unique_Expiry[1:Max_Number_of_Expiries_in_Database] -> temp_CE_FnO_of_temp_Symbol_Unique_Expiry
          } # End of 'if (length(temp_CE_FnO_of_temp_Symbol_Unique_Expiry) > Max_Number_of_Expiries_in_Database)'
          
          First_CE_Expiry_Col_Num <- which(colnames(temp_df)=="CE_Expiry_1")[1]
          # Looping over each temp_CE_FnO_of_temp_Symbol_Unique_Expiry expiry
          # i_7 <- 1
          for (i_7 in 1: length(temp_CE_FnO_of_temp_Symbol_Unique_Expiry))
          {
            if (i_7 > Max_Number_of_Expiries_in_Database)
            {
              break
            }# End of 'if (i_7 > Max_Number_of_Expiries_in_Database)'
            
            # Every strike has 9 colums to it.
            # Max 16 stikes.
            # So thats 9 x 16 columns = 144 columns for ALL strikes for a expiry
            # Add 1 more for the expiry date, we have 145 columns for expiry + strike
            # This 145 set starts from 'which(colnames(temp_df)=="CE_Expiry_1")[1]'
            # Ends at '144 + which(colnames(temp_df)=="CE_Expiry_1")[1]'
            i_7 %>% temp_CE_FnO_of_temp_Symbol_Unique_Expiry[.] %>% 
              format("%Y%m%d") -> temp_expiry_date
            
            expiry_date_col_index <- which(colnames(temp_df)==paste0("CE_Expiry_",i_7,sep=''))[1]
            
            temp_df[1,expiry_date_col_index] <- temp_expiry_date
            
            # Looping over each strike
            # i_8 <- 1; i_8 <- 2; i_8 <- 10
            for (i_8 in 1: length(Percentages_from_spot))
            {
              i_8 %>% Percentages_from_spot[.] -> temp_percentage
              
              temp_Start_Col_Index <- expiry_date_col_index + 9*(i_8-1) + 1
              temp_End_Col_Index <- temp_Start_Col_Index + 9 - 1
              
              temp_CLOSE_spot %>% {.*(1+temp_percentage/100)} -> temp_theoretical_strike
              
              temp_CE_FnO_of_temp_Symbol %>% filter(STRIKE_PR <= 1.01*temp_theoretical_strike) %>% 
                filter(STRIKE_PR >= 0.99*temp_theoretical_strike) -> temp_relevant_strike_candidates
              
              temp_relevant_strike_candidates %>% select(EXPIRY_DT) %>% unlist %>% as.character %>% 
                as.Date(format="%d-%b-%Y") %>% format("%Y%m%d") %>% {which(. == temp_expiry_date)} %>%  
                temp_relevant_strike_candidates[.,] -> temp_relevant_strike_candidates
              
              if(nrow(temp_relevant_strike_candidates) > 0)
              {
                temp_relevant_strike_candidates %>% select(STRIKE_PR) %>% unlist %>% as.numeric %>% 
                  {.-temp_theoretical_strike} %>% abs -> temp_spreads
                
                which.min(temp_spreads) %>% temp_relevant_strike_candidates[.,] -> temp_relevant_strike_candidates
                
                temp_percentage %>% abs -> temp_percentage_abs
                
                temp_colname <- c()
                Plus_Minus <- c()
                if(temp_percentage >= 0)
                {
                  Plus_Minus <- "Plus"
                }else{
                  Plus_Minus <- "Minus"
                }# End of 'if(temp_percentage >= 0)'
                
                temp_relevant_strike_candidates %>% arrange_at("STRIKE_PR") -> temp_relevant_strike_candidates
                
                temp_relevant_strike_candidates[1,] -> temp_relevant_strike_candidates
                
                temp_colname <- paste0("STRIKE_PR_CE_Expiry_",i_7,"_Strike_", temp_percentage_abs,"p_",Plus_Minus,"_Spot",sep='')
                temp_colnum <- which(colnames(temp_df) == temp_colname)
                temp_relevant_strike_candidates$STRIKE_PR -> temp_df[1,temp_colnum]       #1 Strike
                
                temp_colname <- paste0("OPEN_CE_Expiry_",i_7,"_Strike_", temp_percentage_abs,"p_",Plus_Minus,"_Spot",sep='')
                temp_colnum <- which(colnames(temp_df) == temp_colname)
                temp_relevant_strike_candidates$OPEN -> temp_df[1,temp_colnum]            #2 Open
                
                temp_colname <- paste0("HIGH_CE_Expiry_",i_7,"_Strike_", temp_percentage_abs,"p_",Plus_Minus,"_Spot",sep='')
                temp_colnum <- which(colnames(temp_df) == temp_colname)
                temp_relevant_strike_candidates$HIGH -> temp_df[1,temp_colname]           #3 High
                
                temp_colname <- paste0("LOW_CE_Expiry_",i_7,"_Strike_", temp_percentage_abs,"p_",Plus_Minus,"_Spot",sep='')
                temp_colnum <- which(colnames(temp_df) == temp_colname)
                temp_relevant_strike_candidates$LOW -> temp_df[1,temp_colnum]             #4 Low
                
                temp_colname <- paste0("CLOSE_CE_Expiry_",i_7,"_Strike_", temp_percentage_abs,"p_",Plus_Minus,"_Spot",sep='')
                temp_colnum <- which(colnames(temp_df) == temp_colname)
                temp_relevant_strike_candidates$CLOSE -> temp_df[1,temp_colnum]           #5 Close
                
                temp_colname <- paste0("SETTLE_PR_CE_Expiry_",i_7,"_Strike_", temp_percentage_abs,"p_",Plus_Minus,"_Spot",sep='')
                temp_colnum <- which(colnames(temp_df) == temp_colname)
                temp_relevant_strike_candidates$SETTLE_PR -> temp_df[1,temp_colnum]       #6 SETTLE_PR
                
                temp_colname <- paste0("CONTRACTS_CE_Expiry_",i_7,"_Strike_", temp_percentage_abs,"p_",Plus_Minus,"_Spot",sep='')
                temp_colnum <- which(colnames(temp_df) == temp_colname)
                temp_relevant_strike_candidates$CONTRACTS -> temp_df[1,temp_colnum]       #7 CONTRACTS
                
                temp_colname <- paste0("VAL_INLAKH_CE_Expiry_",i_7,"_Strike_", temp_percentage_abs,"p_",Plus_Minus,"_Spot",sep='')
                temp_colnum <- which(colnames(temp_df) == temp_colname)
                temp_relevant_strike_candidates$VAL_INLAKH -> temp_df[1,temp_colname]     #8 VAL_INLAKH
                
                temp_colname <- paste0("OPEN_INT_CE_Expiry_",i_7,"_Strike_", temp_percentage_abs,"p_",Plus_Minus,"_Spot",sep='')
                temp_colnum <- which(colnames(temp_df) == temp_colname)
                temp_relevant_strike_candidates$OPEN_INT -> temp_df[1,temp_colnum]        #9 OPEN_INT
                
              } # End of 'if(nrow(temp_relevant_strike_candidates) > 0)'
              
            } # End of 'for (i_8 in 1: length(Percentages_from_spot))'
            
          }# End of 'for (i_7 in 1: length(temp_CE_FnO_of_temp_Symbol_Unique_Expiry))'
          ############################################################################################
          
          ############################################################################################
          # Entry from 'temp_PE_FnO_of_temp_Symbol' into 'temp_df' for different expiries and strikes
          # # Making 'temp_PE_FnO_of_temp_Symbol_Unique_Expiry' in ascending order
          temp_FnO %>% filter(INSTRUMENT == "OPTSTK") %>% 
            filter(SYMBOL == temp_Symbol) %>% 
            filter(OPTIONTYPE == "PE") -> temp_PE_FnO_of_temp_Symbol
          
          temp_PE_FnO_of_temp_Symbol %>% select(EXPIRY_DT) %>% unlist %>% as.character %>% 
            as.Date(format="%d-%b-%Y") %>% sort %>% unique -> temp_PE_FnO_of_temp_Symbol_Unique_Expiry
          
          if (length(temp_PE_FnO_of_temp_Symbol_Unique_Expiry) > Max_Number_of_Expiries_in_Database)
          {
            temp_PE_FnO_of_temp_Symbol_Unique_Expiry[1:Max_Number_of_Expiries_in_Database] -> temp_PE_FnO_of_temp_Symbol_Unique_Expiry
          } # End of 'if (length(temp_PE_FnO_of_temp_Symbol_Unique_Expiry) > Max_Number_of_Expiries_in_Database)'
          
          First_PE_Expiry_Col_Num <- which(colnames(temp_df)=="PE_Expiry_1")[1]
          # Looping over each temp_PE_FnO_of_temp_Symbol_Unique_Expiry expiry
          # i_9 <- 1
          for (i_9 in 1:length(temp_PE_FnO_of_temp_Symbol_Unique_Expiry))
          {
            if (i_9 > Max_Number_of_Expiries_in_Database)
            {
              break
            }# End of 'if (i_9 > Max_Number_of_Expiries_in_Database)'
            
            # Every strike has 9 colums to it.
            # Max 16 stikes.
            # So thats 9 x 16 columns = 144 columns for ALL strikes for a expiry
            # Add 1 more for the expiry date, we have 145 columns for expiry + strike
            # This 145 set starts from 'which(colnames(temp_df)=="CE_Expiry_1")[1]'
            # Ends at '144 + which(colnames(temp_df)=="PE_Expiry_1")[1]'
            i_9 %>% temp_PE_FnO_of_temp_Symbol_Unique_Expiry[.] %>% 
              format("%Y%m%d") -> temp_expiry_date
            
            expiry_date_col_index <- which(colnames(temp_df)==paste0("PE_Expiry_",i_9,sep=''))[1]
            
            temp_df[1,expiry_date_col_index] <- temp_expiry_date
            
            # Looping over each strike
            # i_10 <- 2; i_10 <- 1; i_10 <- 9
            for (i_10 in 1: length(Percentages_from_spot))
            {
              i_10 %>% Percentages_from_spot[.] -> temp_percentage
              
              temp_Start_Col_Index <- expiry_date_col_index + 9*(i_10-1) + 1
              temp_End_Col_Index <- temp_Start_Col_Index + 9 - 1
              
              temp_CLOSE_spot %>% {.*(1+temp_percentage/100)} -> temp_theoretical_strike
              
              temp_PE_FnO_of_temp_Symbol %>% filter(STRIKE_PR <= 1.01*temp_theoretical_strike) %>% 
                filter(STRIKE_PR >= 0.99*temp_theoretical_strike) -> temp_relevant_strike_candidates
              
              temp_relevant_strike_candidates %>% select(EXPIRY_DT) %>% unlist %>% as.character %>% 
                as.Date(format="%d-%b-%Y") %>% format("%Y%m%d") %>% {which(. == temp_expiry_date)} %>%  
                temp_relevant_strike_candidates[.,] -> temp_relevant_strike_candidates
              
              temp_relevant_strike_candidates %>% arrange_at("STRIKE_PR",desc) -> temp_relevant_strike_candidates
              
              if(nrow(temp_relevant_strike_candidates) > 0)
              {
                temp_relevant_strike_candidates %>% select(STRIKE_PR) %>% unlist %>% as.numeric %>% 
                  {.-temp_theoretical_strike} %>% abs -> temp_spreads
                
                which.min(temp_spreads) %>% temp_relevant_strike_candidates[.,] -> temp_relevant_strike_candidates
                
                temp_relevant_strike_candidates[1,] -> temp_relevant_strike_candidates
                
                temp_percentage %>% abs -> temp_percentage_abs
                
                temp_colname <- c()
                Plus_Minus <- c()
                if(temp_percentage >= 0)
                {
                  Plus_Minus <- "Plus"
                }else{
                  Plus_Minus <- "Minus"
                }# End of 'if(temp_percentage >= 0)'
                
                temp_colname <- paste0("STRIKE_PR_PE_Expiry_",i_9,"_Strike_", temp_percentage_abs,"p_",Plus_Minus,"_Spot",sep='')
                temp_colnum <- which(colnames(temp_df) == temp_colname)
                temp_relevant_strike_candidates$STRIKE_PR -> temp_df[1,temp_colnum]       #1 Strike
                
                temp_colname <- paste0("OPEN_PE_Expiry_",i_9,"_Strike_", temp_percentage_abs,"p_",Plus_Minus,"_Spot",sep='')
                temp_colnum <- which(colnames(temp_df) == temp_colname)
                temp_relevant_strike_candidates$OPEN -> temp_df[1,temp_colname]           #2 Open
                
                temp_colname <- paste0("HIGH_PE_Expiry_",i_9,"_Strike_", temp_percentage_abs,"p_",Plus_Minus,"_Spot",sep='')
                temp_colnum <- which(colnames(temp_df) == temp_colname)
                temp_relevant_strike_candidates$HIGH -> temp_df[1,temp_colnum]            #3 High
                
                temp_colname <- paste0("LOW_PE_Expiry_",i_9,"_Strike_", temp_percentage_abs,"p_",Plus_Minus,"_Spot",sep='')
                temp_colnum <- which(colnames(temp_df) == temp_colname)
                temp_relevant_strike_candidates$LOW -> temp_df[1,temp_colnum]             #4 Low
                
                temp_colname <- paste0("CLOSE_PE_Expiry_",i_9,"_Strike_", temp_percentage_abs,"p_",Plus_Minus,"_Spot",sep='')
                temp_colnum <- which(colnames(temp_df) == temp_colname)
                temp_relevant_strike_candidates$CLOSE -> temp_df[1,temp_colnum]           #5 Close
                
                temp_colname <- paste0("SETTLE_PR_PE_Expiry_",i_9,"_Strike_", temp_percentage_abs,"p_",Plus_Minus,"_Spot",sep='')
                temp_colnum <- which(colnames(temp_df) == temp_colname)
                temp_relevant_strike_candidates$SETTLE_PR -> temp_df[1,temp_colnum]       #6 SETTLE_PR
                
                temp_colname <- paste0("CONTRACTS_PE_Expiry_",i_9,"_Strike_", temp_percentage_abs,"p_",Plus_Minus,"_Spot",sep='')
                temp_colnum <- which(colnames(temp_df) == temp_colname)
                temp_relevant_strike_candidates$CONTRACTS -> temp_df[1,temp_colnum]       #7 CONTRACTS
                
                temp_colname <- paste0("VAL_INLAKH_PE_Expiry_",i_9,"_Strike_", temp_percentage_abs,"p_",Plus_Minus,"_Spot",sep='')
                temp_colnum <- which(colnames(temp_df) == temp_colname)
                temp_relevant_strike_candidates$VAL_INLAKH -> temp_df[1,temp_colnum]      #8 VAL_INLAKH
                
                temp_colname <- paste0("OPEN_INT_PE_Expiry_",i_9,"_Strike_", temp_percentage_abs,"p_",Plus_Minus,"_Spot",sep='')
                temp_colnum <- which(colnames(temp_df) == temp_colname)
                temp_relevant_strike_candidates$OPEN_INT -> temp_df[1,temp_colnum]        #9 OPEN_INT
                
              } # End of 'if(nrow(temp_relevant_strike_candidates) > 0)'
              
            } # End of 'for (i_10 in 1: length(Percentages_from_spot))'
            
          }# End of 'for (i_9 in 1: length(temp_PE_FnO_of_temp_Symbol_Unique_Expiry))'
          ############################################################################################
        }else{
          print(paste0("Error! Check !! nrows of 'temp_CE_FnO_of_temp_Symbol',",
                       " 'temp_PE_FnO_of_temp_Symbol' are: ",
                       nrow(temp_CE_FnO_of_temp_Symbol),", ",
                       nrow(temp_PE_FnO_of_temp_Symbol)," respectively",sep=''))
        }# End of 'if(nrow(temp_CE_FnO_of_temp_Symbol) > 0 &'
        
        # Adding 'temp_df' to 
        if(!is.na(temp_df$Date_YYYYMMDD_C))
        {
          temp_df %>% rbind(temp_Symbol_Data,.) -> temp_Symbol_Data
        } # End of 'if(!is.na(temp_df$Date_YYYYMMDD_C))'
        
      }# End of 'for (i_2 in 1:length(Relevant_files))'
      if(nrow(temp_Symbol_Data) > 0)
      {
        # Make output file
        OPTSTK_TS_Database %>% setwd
        if(Overwrite_Required)
        {
          # Writing when overwrite is required
          print(paste0("Overwriting ", temp_filename, sep=''))
          
          file.remove(temp_filename)
          
          temp_Symbol_Data %>% as.data.frame %>% 
            write.csv(file=temp_filename,row.names = F,na = "")
        }else{
          # Writing when overwrite is NOT required
          temp_Symbol_Data %>% write.csv(file = temp_filename,row.names = F, na = "")
        } # Writing when overwrite is NOT required
      }# End of 'if(nrow(temp_Symbol_Data) > 0)'
    }# End of 'if(length(Relevant_files) > 0)'
  }# End of 'for(i_1 in 1:length(Latest_File_Symbols))' 
}else{
  print("Couldn't find a FUTIDX list in the Symbols folder.")
} # End of 'if(length(Latest_File_Symbols) > 0)'
#################################################################################
#################################################################################