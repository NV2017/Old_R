#################################################################################
#################################### Goals ######################################

# Date: 2020-May-28
# Author: Arunabha Sarkar

# Goals: Make Futures Stocks Time Series
# File Name: FUTSTK_TS

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

Threshold_Year <- 2007

"C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% paste0("/Latest_NSE_Index_Composition",
                            sep='') -> Latest_NSE_Index_Composition_Database

Central_Database %>% paste0("/Nifty 50 Historical Data Investing dot com",
                            sep='') -> Nifty50_Database

Central_Database %>% paste0("/NSE EQ Bhav Copies",sep='') -> EQ_Bhav_Database

Central_Database %>% paste0("/NSE FnO Bhav Copies",sep='') -> FnO_Bhav_Database

Central_Database %>% paste0("/Symbols in EnFnO",sep='') -> Symbols_in_EnFnO_Database

Central_Database %>% paste0("/FUTSTK_TS",sep='') -> FUTSTK_TS_Database

Central_Database %>% setwd()

if(!file.exists('FUTSTK_TS'))
{
  print("Creating 'FUTSTK_TS' directory.")
  dir.create(file.path(Central_Database, 'FUTSTK_TS'))
}else{
  print("'FUTSTK_TS' directory already exists.")
} # End of 'if(!file.exists('FUTSTK_TS'))'

#################################################################################
#################################################################################

#################################################################################
############################ Finding/Making FUTSTK ##############################

# Find latest date of NIFTY50

Nifty50_Database %>% setwd

list.files() %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>% 
  as.Date(format="%Y%m%d") %>% which.max %>% list.files()[.] %>% 
  read.csv %>% select(Date) %>% unlist %>% as.character %>% 
  as.Date(format="%b %d, %Y") %>% max %>% format("%Y%m%d") -> Max_Nifty_Date

# Finding the list of FUTSTK
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
  # i_1 <- 1
  for(i_1 in 1:length(Latest_File_Symbols))
  {
    i_1 %>% Latest_File_Symbols[.] %>% {. ->> temp_Symbol} %>% 
      paste0("Checking ",.,sep='') %>% print
    
    # Checking if this Symbol exists in the FUTSTK_TS folder
    # Returning required 'Relevant_files'
    FUTSTK_TS_Database %>% setwd
    
    temp_Symbol %>% paste0(.,"_Futures.csv",sep='') -> temp_filename
    
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
        # i_r <- 2
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
      FUTSTK_TS_Database %>% setwd
      
      temp_Symbol %>% paste0(.,"_Futures.csv",sep='') -> temp_filename
      
      if(length(temp_filename) > 0)
      {
        temp_filename %>% read.csv -> temp_old_file
        temp_old_file %>% select(Date_YYYYMMDD) %>% unlist %>% as.character %>% 
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
          if(length(Trial_points_index_Relevant_files) > 1)
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
      }# End of  'Checking temp_filename existance'
    } # End of 'if(!(temp_filename %in% list.files()))'
    
    # If old data is present, get it. If not, prepare the following dataframe
    if(Overwrite_Required)
    {
      FUTSTK_TS_Database %>% setwd
      
      temp_Symbol %>% paste0(.,"_Futures.csv",sep='') -> temp_filename
      temp_filename %>% read.csv -> temp_Symbol_Data
    }else{ # When over write is not required, i.e. starting from scratch
      data.frame(Date_YYYYMMDD = as.Date(character()),
                 INSTRUMENT = character(),
                 SYMBOL = character(),
                 
                 EXPIRY_NEAR = as.Date(character()),
                 OPEN_NEAR = as.double(),
                 HIGH_NEAR = as.double(),
                 LOW_NEAR = as.double(),
                 CLOSE_NEAR = as.double(),
                 SETTLE_PR_NEAR = as.double(),
                 CONTRACTS_NEAR = as.double(),
                 VAL_INLAKH_NEAR = as.double(),
                 OPEN_INT_NEAR = as.double(),
                 CHG_IN_OI_NEAR = as.double(),
                 
                 EXPIRY_MID = as.Date(character()),
                 OPEN_MID = as.double(),
                 HIGH_MID = as.double(),
                 LOW_MID = as.double(),
                 CLOSE_MID = as.double(),
                 SETTLE_PR_MID = as.double(),
                 CONTRACTS_MID = as.double(),
                 VAL_INLAKH_MID = as.double(),
                 OPEN_INT_MID = as.double(),
                 CHG_IN_OI_MID = as.double(),
                 
                 EXPIRY_FAR = as.Date(character()),
                 OPEN_FAR = as.double(),
                 HIGH_FAR = as.double(),
                 LOW_FAR = as.double(),
                 CLOSE_FAR = as.double(),
                 SETTLE_PR_FAR = as.double(),
                 CONTRACTS_FAR = as.double(),
                 VAL_INLAKH_FAR = as.double(),
                 OPEN_INT_FAR = as.double(),
                 CHG_IN_OI_FAR = as.double(),
                 
                 OPEN_SPOT_NIFTY = as.double(),
                 HIGH_SPOT_NIFTY = as.double(),
                 LOW_SPOT_NIFTY = as.double(),
                 CLOSE_SPOT_NIFTY = as.double(),
                 LAST_SPOT_NIFTY = as.double(),
                 TOTTRDVAL_SPOT_NIFTY = as.double(),
                 
                 OPEN_SPOT_STOCK = as.double(),
                 HIGH_SPOT_STOCK = as.double(),
                 LOW_SPOT_STOCK = as.double(),
                 CLOSE_SPOT_STOCK = as.double(),
                 LAST_SPOT_STOCK = as.double(),
                 TOTTRDVAL_SPOT_STOCK = as.double(),
                 
                 stringsAsFactors = F) -> temp_Symbol_Data
    } # End of checking for overwrite
    
    
    # Now we have 'Relevant_files'. Looping over this for 'temp_Symbol'
    if (length(Relevant_files) > 0)
    {
      # i_2 <- 1
      for (i_2 in 1:length(Relevant_files))
      {
        print(paste0("Processing #",i_2, " out of ", length(Relevant_files)," for '", 
                     temp_Symbol,"' #",i_1, " of ",length(Latest_File_Symbols),
                     " symbols. Present rows for this symbol: ",
                     nrow(temp_Symbol_Data),sep=''))
        FnO_Bhav_Database %>% setwd
        i_2 %>% Relevant_files[.] %>% read.csv -> temp_FnO
        
        temp_FnO %>% colnames %>% {which(.=="TIMESTAMP")} %>% seq(1,.,1) %>% 
          temp_FnO[,.] -> temp_FnO
        
        temp_FnO %>% filter(INSTRUMENT == "FUTSTK") %>% 
          filter(SYMBOL == temp_Symbol) -> temp_FnO_of_temp_Symbol
        
        temp_Symbol_Data %>% colnames %>% length %>% 
          matrix(data = NA,nrow = 1, ncol = .) %>% 
          data.frame(stringsAsFactors = F) %>% 
          `colnames<-`(colnames(temp_Symbol_Data)) -> temp_df
        
        # Solve 14th May 2012 date format right now
        
        if(nrow(temp_FnO_of_temp_Symbol) > 0)
        {
          # Tackling for first row of temp_FnO_of_temp_Symbol
          temp_FnO_of_temp_Symbol %>% select(TIMESTAMP) %>% 
            unlist %>% as.character %>% unique -> Pro_Date
          
          if(nchar(Pro_Date[1]) == 9)
          {
            Pro_Date %>% .[1] %>% as.Date(format="%d-%b-%y") -> temp_date
          }else{
            Pro_Date %>% .[1] %>% as.Date(format="%d-%b-%Y") -> temp_date
          } # End of 'if(nchar(Pro_Date[1]) == 9)'
          
          temp_date %>% as.Date(format="%Y-%m-%d") %>% 
            format("%Y%m%d") %>% as.character -> temp_df$Date_YYYYMMDD[1]
          
          temp_FnO_of_temp_Symbol %>% select(INSTRUMENT) %>% unlist %>% 
            as.character %>% unique %>% .[1] -> temp_df$INSTRUMENT[1]
          
          temp_Symbol -> temp_df$SYMBOL[1]
          
          temp_FnO_of_temp_Symbol %>% select(EXPIRY_DT) %>% 
            unlist %>% as.character %>% unique %>% .[1] %>% 
            as.Date(format="%d-%b-%Y") %>% format("%Y%m%d") -> temp_df$EXPIRY_NEAR[1]
          
          temp_FnO_of_temp_Symbol[1,6:14] -> temp_df[,5:13]
          
          # Tackling for second row of temp_FnO_of_temp_Symbol
          if (nrow(temp_FnO_of_temp_Symbol) > 1)
          {
            temp_FnO_of_temp_Symbol %>% select(EXPIRY_DT) %>% 
              unlist %>% as.character %>% unique %>% .[2] %>% 
              as.Date(format="%d-%b-%Y") %>% format("%Y%m%d") -> temp_df$EXPIRY_MID[1]
            
            temp_FnO_of_temp_Symbol[2,6:14] -> temp_df[,15:23]
          } # End of 'if (nrow(temp_FnO_of_temp_Symbol) > 1)'
          
          # Tackling for third row of temp_FnO_of_temp_Symbol
          if (nrow(temp_FnO_of_temp_Symbol) > 2)
          {
            temp_FnO_of_temp_Symbol %>% select(EXPIRY_DT) %>% 
              unlist %>% as.character %>% unique %>% .[3] %>% 
              as.Date(format="%d-%b-%Y") %>% format("%Y%m%d") -> temp_df$EXPIRY_FAR[1]
            
            temp_FnO_of_temp_Symbol[3,6:14] -> temp_df[,25:33]
          } # End of 'if (nrow(temp_FnO_of_temp_Symbol) > 2)'
          
          # Tackling NIFTY for SPOT values for temp_df
          Nifty50_Database %>% setwd
          
          list.files() %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>% 
            as.Date(format="%Y%m%d") %>% which.max %>% list.files()[.] %>% 
            read.csv -> Nifty_Index
          
          Nifty_Index %>% select(Date) %>% unlist %>% as.character %>% 
            as.Date(format="%b %d, %Y") %>% format("%Y-%m-%d") -> Nifty_Index$Date
          
          which(Nifty_Index$Date == temp_date) %>% 
            Nifty_Index[.,2:5] -> temp_df[,c(37,34,35,36)]
          
          # Tackling STK for SPOT values for temp_df
          EQ_Bhav_Database %>% setwd
          
          list.files() -> EQ_bhav_copy_files
          
          EQ_bhav_copy_files %>% as.Date(format="cm%d%b%Ybhav.csv") %>% 
            format("%Y%m%d")-> EQ_bhav_copy_dates
          
          which(EQ_bhav_copy_dates == temp_df$Date_YYYYMMDD[1]) -> Correct_EQ_Filename
          
          if(length(Correct_EQ_Filename) > 0)
          {
            Correct_EQ_Filename[1] %>% EQ_bhav_copy_files[.] %>% read.csv %>% 
              filter(SYMBOL==temp_Symbol) %>% .[1,c(3,4,5,6,7,10)] %>% 
              unlist -> temp_df[,c(40,41,42,43,44,45)]
          }# End of 'if(length(Correct_EQ_File) > 0)'
        } # End of 'if(nrow(temp_FnO_of_temp_Symbol) > 0)'
        
        if(!is.na(temp_df$Date_YYYYMMDD))
        {
          temp_df %>% rbind(temp_Symbol_Data,.) -> temp_Symbol_Data
        } # End of 'if(!is.na(temp_df$Date_YYYYMMDD))'
      }
      if(nrow(temp_Symbol_Data) > 0)
      {
        # Make output file
        FUTSTK_TS_Database %>% setwd
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
  print("Couldn't find a FUTSTK list in the Symbols folder.")
} # End of 'if(length(Latest_File_Symbols) > 0)'

#################################################################################
#################################################################################