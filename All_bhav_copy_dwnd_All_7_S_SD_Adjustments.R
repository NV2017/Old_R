#################################################################################
#################################### Goals ######################################

# Date: 2020-Jun-19
# Author: Arunabha Sarkar

# Goals: Download latest NIFTY 50 composition
# File Name: Dwnld_latest_NIFTY50_composition

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

Sys.Date() %>% format("%Y%m%d") %>% as.character -> Todays_Date

61 -> Standard_Waiting_Time_Seconds

c("Nifty50","NiftyNext50","Nifty100","Nifty200","Nifty500","NiftyMidcap150","NiftyMidcap50",
  "NiftyMidcap100","NiftySmallcap250","NiftySmallcap50","NiftySmallcap100","NiftyLargeMidcap250",
  "NiftyMidSmallcap400","BankNifty") -> NSE_Index_Types

NSE_Index_Types %>% paste0(Todays_Date," ",.,".csv",sep="") -> Output_filenames

c("https://www1.nseindia.com/content/indices/ind_nifty50list.csv", # Nifty50
  "https://www1.nseindia.com/content/indices/ind_niftynext50list.csv", # NiftyNext50
  "https://www1.nseindia.com/content/indices/ind_nifty100list.csv", # Nifty100
  "https://www1.nseindia.com/content/indices/ind_nifty200list.csv", # Nifty200
  "https://www1.nseindia.com/content/indices/ind_nifty500list.csv", # Nifty500
  "https://www1.nseindia.com/content/indices/ind_niftymidcap150list.csv", # NiftyMidcap150
  "https://www1.nseindia.com/content/indices/ind_niftymidcap50list.csv", # NiftyMidcap50
  "https://www1.nseindia.com/content/indices/ind_niftymidcap100list.csv", # NiftyMidcap100
  "https://www1.nseindia.com/content/indices/ind_niftysmallcap250list.csv", # NiftySmallcap250
  "https://www1.nseindia.com/content/indices/ind_niftysmallcap50list.csv", # NiftySmallcap50
  "https://www1.nseindia.com/content/indices/ind_niftysmallcap100list.csv", # NiftySmallcap100,
  "https://www1.nseindia.com/content/indices/ind_niftylargemidcap250list.csv", # NiftyLargeMidcap250
  "https://www1.nseindia.com/content/indices/ind_niftymidsmallcap400list.csv", # NiftyMidSmallcap400
  "https://archives.nseindia.com/content/indices/ind_niftybanklist.csv" # BankNifty
) -> URL_latest_NSE_Index_composition

"C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% setwd()

if(!file.exists('Latest_NSE_Index_Composition'))
{
  print("Creating 'Latest_NSE_Index_Composition' directory.")
  dir.create(file.path(Central_Database, 'Latest_NSE_Index_Composition'))
}else{
  print("'Latest_NSE_Index_Composition' directory already exists.")
} # End of 'if(!file.exists('Latest_NSE_Index_Composition'))'

'Latest_NSE_Index_Composition' %>% paste0(Central_Database,"/",.,sep="") -> Latest_NSE_Index_Composition_Database

#################################################################################
#################################################################################

#################################################################################
################ Making latest NIFTY 50 composition, if needed ##################

# i_1 <- 1
for (i_1 in 1:length(Output_filenames))
{
  To_Make <- FALSE
  
  Latest_NSE_Index_Composition_Database %>% setwd
  
  list.files() -> Latest_NSE_Index_Composition_Database_files
  
  if(length(Latest_NSE_Index_Composition_Database_files) == 0)
  {
    "No existing file: '" %>% paste0(.,Output_filenames[i_1],"'",sep="") %>% print
    To_Make <- TRUE
  }else{
    # Check
    if(Output_filenames[i_1] %in% Latest_NSE_Index_Composition_Database_files)
    {
      "Existing file: '" %>% paste0(.,Output_filenames[i_1],"' is upto date",sep="") %>% print
      To_Make <- FALSE
    }else{
      "Making new file: '" %>% paste0(.,Output_filenames[i_1],"'",sep="") %>% print
      To_Make <- TRUE
    }
  } # End of 'if(length(Latest_NSE_Index_Composition_Database_files) == 0)'
  
  if(To_Make)
  {
    Latest_NSE_Index_Composition_Database %>% setwd
    
    temp_file <- tempfile()
    
    i_1 %>% URL_latest_NSE_Index_composition[.] %>% download.file(temp_file)
    
    temp_file %>% read.csv %>% write.csv(.,file=Output_filenames[i_1],row.names = F)
    
    Standard_Waiting_Time_Seconds %T>% {print(paste0("Sleeping ", .," seconds",sep=''))} %>% Sys.sleep
    
  } # End of 'if(To_Make)'
  
} # End of 'for (i_1 in 1:length(Output_filenames))'

#################################################################################
#################################################################################

#################################################################################
################# All in one Download bhav copy and FnO stuff ###################

# Date: 2020-July-10
# Author: Arunabha Sarkar

# Goals: All in one Download bhav copy and FnO stuff
# File Name: All_Download_bhavcopy_FnO

#################################################################################
#################################################################################

#################################################################################
#################################### Goals ######################################

# Date: 2020-May-27
# Author: Arunabha Sarkar

# Goals: Downloading missing NSE EQ Bhav Copy for central database
# File Name: Dwnld_missing_EQ_Bhav_Copy

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

"C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% paste0("/Nifty 50 Historical Data Investing dot com",
                            sep='') -> Nifty50_Database

Central_Database %>% paste0("/NSE EQ Bhav Copies",sep='') -> EQ_Bhav_Database

Central_Database %>% setwd()

if(!file.exists('NSE EQ Bhav Copies'))
{
  print("Creating 'NSE EQ Bhav Copies' directory.")
  dir.create(file.path(Central_Database, 'NSE EQ Bhav Copies'))
}else{
  print("'NSE EQ Bhav Copies' directory already exists.")
} # End of 'if(!file.exists('NSE EQ Bhav Copies'))'

#################################################################################
#################################################################################

#################################################################################
################################ All NIFTY Dates ################################

Nifty50_Database %>% setwd

list.files() %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>% 
  as.Date(format="%Y%m%d") %>% which.max %>% list.files()[.] %>% 
  read.csv %>% select(Date) %>% unlist %>% as.character %>% 
  as.Date(format="%b %d, %Y") -> All_Nifty_Dates

#################################################################################
#################################################################################

#################################################################################
########################### All EQ Bhav Copy Dates #############################

EQ_Bhav_Database %>% setwd

list.files() %>% as.Date(format="cm%d%b%Ybhav.csv") -> All_EQ_Bhav_Copy_Dates

#################################################################################
#################################################################################

#################################################################################
################## Filtering Dates above a thereshold year ######################

Threshold_Year <- 2007

All_Nifty_Dates %>% format("%Y") %>% as.integer %>% {.>=Threshold_Year} %>%
  All_Nifty_Dates[.] -> All_Nifty_Dates_above_threshold

All_EQ_Bhav_Copy_Dates %>% format("%Y") %>% as.integer %>% {.>=Threshold_Year} %>%
  All_EQ_Bhav_Copy_Dates[.] -> All_EQ_Bhav_Copy_Dates_above_threshold

#################################################################################
#################################################################################

#################################################################################
########################## Missing Bhav Copy Dates ##############################

(!(All_EQ_Bhav_Copy_Dates_above_threshold %in% All_Nifty_Dates_above_threshold)) %>% 
  which(.==TRUE) %>% All_EQ_Bhav_Copy_Dates_above_threshold[.] -> Missing_Date_in_NIFTY50

(!(All_Nifty_Dates_above_threshold %in% All_EQ_Bhav_Copy_Dates_above_threshold)) %>% 
  which(.==TRUE) %>% All_Nifty_Dates_above_threshold[.] -> Missing_Date_in_Bhav_Copy

#################################################################################
#################################################################################

#################################################################################
########################## Missing Bhav Copy Dates ##############################

# Sample
# https://www1.nseindia.com/content/historical/EQUITIES/2020/MAY/cm26MAY2020bhav.csv.zip

Part_1 <- "https://www1.nseindia.com/content/historical/EQUITIES/"
Part_3 <- "bhav.csv.zip"

if (length(Missing_Date_in_Bhav_Copy) > 0)
{
  print(paste0("To download ", length(Missing_Date_in_Bhav_Copy), " files.", sep=''))
  
  EQ_Bhav_Database %>% setwd
  
  # i_1 <- 1
  for (i_1 in 1:length(Missing_Date_in_Bhav_Copy))
  {
    i_1 %>% Missing_Date_in_Bhav_Copy[.] -> temp_date
    temp_date %>% format("%Y") %>% as.integer -> temp_year
    temp_date %>% format("%b") %>% toupper -> temp_month
    temp_date %>% format("%d%b%Y") %>% toupper %>% paste0("cm",.,sep='') %>% 
      paste0(temp_year,"/",temp_month,"/",.,sep='') -> Part_2
    
    Part_1 %>% paste0(.,Part_2,Part_3,sep='') -> temp_nse_EQ_bhav_link
    temp_file <- tempfile()
    
    temp_date %>% format("%d%b%Y") %>% toupper %>% 
      paste0("cm",.,"bhav.csv",sep='') -> temp_filename
    
    print(paste0("Downloading NSE EQ bhav copy for: ", temp_date,
                 ". Number: ",i_1, " out of ", length(Missing_Date_in_Bhav_Copy), sep=''))
    
    temp_nse_EQ_bhav_link %>% download.file(temp_file)
    
    unz(temp_file, temp_filename) %>% read.csv -> temp_df
    
    which(colnames(temp_df) == "ISIN") %>% {1:.} %>% 
      temp_df[,.] %>% write.csv(file = temp_filename,row.names = FALSE)
    
    181 %T>% {print(paste0("Sleeping ", .," seconds",sep=''))} %>% Sys.sleep
  } # End of 'for (i_1 in 1:length(Missing_Date_in_Bhav_Copy))'
}else{
  print("All the files for EQ bhav copy exist.")
} # End of 'if (length(Missing_Date_in_Bhav_Copy) > 0)'

#################################################################################
#################################################################################

#################################################################################
#################################### Goals ######################################

# Date: 2020-May-27
# Author: Arunabha Sarkar

# Goals: Downloading missing NSE FnO Bhav Copy for central database
# File Name: Dwnld_missing_FnO_Bhav_Copy

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

# "C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% paste0("/Nifty 50 Historical Data Investing dot com",
                            sep='') -> Nifty50_Database

Central_Database %>% paste0("/NSE FnO Bhav Copies",sep='') -> FnO_Bhav_Database

Central_Database %>% setwd()

if(!file.exists('NSE FnO Bhav Copies'))
{
  print("Creating 'NSE FnO Bhav Copies' directory.")
  dir.create(file.path(Base_Directory, 'NSE FnO Bhav Copies'))
}else{
  print("'NSE FnO Bhav Copies' directory already exists.")
} # End of 'if(!file.exists('NSE FnO Bhav Copies'))'

#################################################################################
#################################################################################

#################################################################################
################################ All NIFTY Dates ################################

Nifty50_Database %>% setwd

list.files() %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>% 
  as.Date(format="%Y%m%d") %>% which.max %>% list.files()[.] %>% 
  read.csv %>% select(Date) %>% unlist %>% as.character %>% 
  as.Date(format="%b %d, %Y") -> All_Nifty_Dates

#################################################################################
#################################################################################

#################################################################################
########################### All FnO Bhav Copy Dates #############################

FnO_Bhav_Database %>% setwd

list.files() %>% as.Date(format="fo%d%b%Ybhav.csv") -> All_FnO_Bhav_Copy_Dates

#################################################################################
#################################################################################

#################################################################################
################## Filtering Dates above a thereshold year ######################

Threshold_Year <- 2007

All_Nifty_Dates %>% format("%Y") %>% as.integer %>% {.>=Threshold_Year} %>%
  All_Nifty_Dates[.] -> All_Nifty_Dates_above_threshold

All_FnO_Bhav_Copy_Dates %>% format("%Y") %>% as.integer %>% {.>=Threshold_Year} %>%
  All_FnO_Bhav_Copy_Dates[.] -> All_FnO_Bhav_Copy_Dates_above_threshold

#################################################################################
#################################################################################

#################################################################################
########################## Missing Bhav Copy Dates ##############################

(!(All_FnO_Bhav_Copy_Dates_above_threshold %in% All_Nifty_Dates_above_threshold)) %>% 
  which(.==TRUE) %>% All_FnO_Bhav_Copy_Dates_above_threshold[.] -> Missing_Date_in_NIFTY50

(!(All_Nifty_Dates_above_threshold %in% All_FnO_Bhav_Copy_Dates_above_threshold)) %>% 
  which(.==TRUE) %>% All_Nifty_Dates_above_threshold[.] -> Missing_Date_in_Bhav_Copy

#################################################################################
#################################################################################

#################################################################################
########################## Missing Bhav Copy Dates ##############################

# Sample
# https://www1.nseindia.com/content/historical/DERIVATIVES/2020/MAY/fo26MAY2020bhav.csv.zip

Part_1 <- "https://www1.nseindia.com/content/historical/DERIVATIVES/"
Part_3 <- "bhav.csv.zip"

if (length(Missing_Date_in_Bhav_Copy) > 0)
{
  print(paste0("To download ", length(Missing_Date_in_Bhav_Copy), " files.", sep=''))
  
  FnO_Bhav_Database %>% setwd
  
  # i_1 <- 1
  for (i_1 in 1:length(Missing_Date_in_Bhav_Copy))
  {
    i_1 %>% Missing_Date_in_Bhav_Copy[.] -> temp_date
    temp_date %>% format("%Y") %>% as.integer -> temp_year
    temp_date %>% format("%b") %>% toupper -> temp_month
    temp_date %>% format("%d%b%Y") %>% toupper %>% paste0("fo",.,sep='') %>% 
      paste0(temp_year,"/",temp_month,"/",.,sep='') -> Part_2
    
    Part_1 %>% paste0(.,Part_2,Part_3,sep='') -> temp_nse_FnO_bhav_link
    temp_file <- tempfile()
    
    temp_date %>% format("%d%b%Y") %>% toupper %>% 
      paste0("fo",.,"bhav.csv",sep='') -> temp_filename
    
    print(paste0("Downloading NSE FnO bhav copy for: ", temp_date,
                 ". Number: ",i_1, " out of ", length(Missing_Date_in_Bhav_Copy), sep=''))
    
    temp_nse_FnO_bhav_link %>% download.file(temp_file)
    
    unz(temp_file, temp_filename) %>% read.csv -> temp_df
    
    which(colnames(temp_df) == "TIMESTAMP") %>% {1:.} %>% 
      temp_df[,.] %>% write.csv(file = temp_filename,row.names = FALSE)
    
    181 %T>% {print(paste0("Sleeping ", .," seconds",sep=''))} %>% Sys.sleep
  } # End of 'for (i_1 in 1:length(Missing_Date_in_Bhav_Copy))'
}else{
  print("All the files for FnO bhav copy exist.")
} # End of 'if (length(Missing_Date_in_Bhav_Copy) > 0)'

#################################################################################
#################################################################################

#################################################################################
#################################### Goals ######################################

# Date: 2020-May-28
# Author: Arunabha Sarkar

# Goals: Finding all tickers in EnFnO
# File Name: Futures_Symbols_in_EnFnO

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

Threshold_Year <- 2011

# "C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% paste0("/Nifty 50 Historical Data Investing dot com",
                            sep='') -> Nifty50_Database

Central_Database %>% paste0("/NSE EQ Bhav Copies",sep='') -> EQ_Bhav_Database

Central_Database %>% paste0("/NSE FnO Bhav Copies",sep='') -> FnO_Bhav_Database

Central_Database %>% paste0("/Symbols in EnFnO",sep='') -> Symbols_in_EnFnO_Database

Central_Database %>% setwd()

if(!file.exists('Symbols in EnFnO'))
{
  print("Creating 'Symbols in EnFnO' directory.")
  dir.create(file.path(Central_Database, 'Symbols in EnFnO'))
}else{
  print("'Symbols in EnFnO' directory already exists.")
} # End of 'if(!file.exists('Symbols in EnFnO'))'

#################################################################################
#################################################################################

#################################################################################
############################ Finding FUTIDX Symbols #############################

# Find latest date of NIFTY50

Nifty50_Database %>% setwd

list.files() %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>% 
  as.Date(format="%Y%m%d") %>% which.max %>% list.files()[.] %>% 
  read.csv %>% select(Date) %>% unlist %>% as.character %>% 
  as.Date(format="%b %d, %Y") %>% max %>% format("%Y%m%d") -> Max_Nifty_Date

Max_Nifty_Date %>% as.Date(format = "%Y%m%d") %>% format("%Y%m%d") %>% 
  paste0("_FUTIDX_Symbols.csv",sep = '') -> temp_FUTIDX_symbols_filename

Symbols_in_EnFnO_Database %>% setwd
Symbols_in_EnFnO_Database_File_List <- list.files(pattern = "FUTIDX")

if(length(Symbols_in_EnFnO_Database_File_List) > 0)
{
  Symbols_in_EnFnO_Database_File_List %>% strsplit(.,split = ' ') %>% map(1) %>% 
    unlist %>% as.Date(format="%Y%m%d") %>% which.max %>% 
    Symbols_in_EnFnO_Database_File_List[.] %T>% {. ->> Latest_File} %>% 
    read.csv %>% select(Unique_FUTIDX_Symbols) %>% unlist %>% 
    as.character -> Latest_File_Symbols
  
  Latest_File %>% strsplit(split = '_') %>% .[[1]] %>% .[1] -> Latest_File_Date
}else{
  Latest_File_Symbols <- c()
  Latest_File_Date <- c()
  Latest_File <- c()
} # End of 'if(length(Symbols_in_EnFnO_Database_File_List) > 0)'

if (!(temp_FUTIDX_symbols_filename %in% list.files()))
{
  # Find latest FUTIDX if possible
  FnO_Bhav_Database %>% setwd
  
  list.files() -> All_FnO_Bhav_Copy_Files
  
  All_FnO_Bhav_Copy_Files %>% as.Date(format="fo%d%b%Ybhav.csv") -> All_FnO_Bhav_Copy_Dates
  
  All_FnO_Bhav_Copy_Dates %>% max %>% format("%Y%m%d") -> Max_FnO_Bhav_Copy_Date
  
  if(length(Latest_File_Date) > 0)
  {
    if(Max_Nifty_Date > Latest_File_Date)
    {
      # Find the minimal set of FnO bhav copies
      All_FnO_Bhav_Copy_Dates %>% format("%Y%m%d") %>% {.>Latest_File_Date} %>%
        All_FnO_Bhav_Copy_Files[.] -> Relevant_FnO_files
      
      Relevant_FnO_files %>% as.Date(format="fo%d%b%Ybhav.csv") %>% format("%Y") %>% 
        as.integer %>% {.>=Threshold_Year} %>%
        Relevant_FnO_files[.] -> Relevant_FnO_files
    } # End of 'if(Max_Nifty_Date > Latest_File_Date)'
  }else{
    All_FnO_Bhav_Copy_Dates %>% as.Date(format="fo%d%b%Ybhav.csv") %>% format("%Y") %>% 
      as.integer %>% {.>=Threshold_Year} %>%
      All_FnO_Bhav_Copy_Files[.]  -> Relevant_FnO_files
  } # End of 'if(length(Latest_File_Date) > 0)'
  
  Total_New_Symbols <- c()
  
  FnO_Bhav_Database %>% setwd
  
  if(length(Relevant_FnO_files) > 0)
  {
    # i_1 <- 1
    for (i_1 in 1:length(Relevant_FnO_files))
    {
      print(paste0("Processing ",i_1, " out off ",
                   length(Relevant_FnO_files),sep=''))
      
      i_1 %>% Relevant_FnO_files[.] %>% read.csv -> temp_FnO_file
      
      temp_FnO_file %>% filter(INSTRUMENT == "FUTIDX") %>% 
        select("SYMBOL") %>% unlist %>% as.character %>% 
        unique %>% c(Total_New_Symbols,.) -> Total_New_Symbols
      
      Total_New_Symbols <- Total_New_Symbols[Total_New_Symbols!=""]
    } # End of 'for (i_1 in 1:length(Relevant_FnO_files))'
  } # End of 'if(length(Relevant_FnO_files) > 0)'
  
  if(length(Total_New_Symbols) > 0)
  {
    Total_New_Symbols %>% c(.,Latest_File_Symbols) %>% unique -> Total_New_Symbols
  } # End of 'if(length(Total_New_Symbols) > 0)'
  
  if(length(Total_New_Symbols) >= length(Latest_File_Symbols))
  {
    Total_New_Symbols %>% unique -> Total_Symbols_Unique
    
    Total_Symbols_Unique %>% c(.,Latest_File_Symbols) %>% unique -> Total_Symbols_Unique
    
    Total_Symbols_Unique <- Total_Symbols_Unique[Total_Symbols_Unique!=""]
    
    Symbols_in_EnFnO_Database %>% setwd
    
    if (!(temp_FUTIDX_symbols_filename %in% list.files()))
    {
      print(paste0("Making new ", temp_FUTIDX_symbols_filename, sep=''))
      
      Total_Symbols_Unique %>% as.data.frame %>% 
        `colnames<-`("Unique_FUTIDX_Symbols") %>% 
        write.csv(file=temp_FUTIDX_symbols_filename,row.names = F)
    }else{
      print(paste0("Overwriting ", temp_FUTIDX_symbols_filename, sep=''))
      
      file.remove(temp_FUTIDX_symbols_filename)
      
      Total_Symbols_Unique %>% as.data.frame %>% 
        `colnames<-`("Unique_FUTIDX_Symbols") %>% 
        write.csv(file=temp_FUTIDX_symbols_filename,row.names = F)
    } # End of 'if (!(temp_FUTIDX_symbols_filename %in% list.files()))'
  }else{
    print(paste0("No new symbols found, latest: ", Max_Nifty_Date,sep=''))
  } # End of 'if(length(Total_New_Symbols) >= length(Latest_File_Symbols))'
}else{
  print("Latest FUTIDX file already present")
} # End of 'if (!(temp_FUTIDX_symbols_filename %in% list.files()))'

#################################################################################
#################################################################################

#################################################################################
############################ Finding FUTSTK Symbols #############################

# Find latest date of NIFTY50

Nifty50_Database %>% setwd

list.files() %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>% 
  as.Date(format="%Y%m%d") %>% which.max %>% list.files()[.] %>% 
  read.csv %>% select(Date) %>% unlist %>% as.character %>% 
  as.Date(format="%b %d, %Y") %>% max %>% format("%Y%m%d") -> Max_Nifty_Date

Max_Nifty_Date %>% as.Date(format = "%Y%m%d") %>% format("%Y%m%d") %>% 
  paste0("_FUTSTK_Symbols.csv",sep = '') -> temp_FUTSTK_symbols_filename

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

if (!(temp_FUTSTK_symbols_filename %in% list.files()))
{
  # Find latest FUTSTK if possible
  FnO_Bhav_Database %>% setwd
  
  list.files() -> All_FnO_Bhav_Copy_Files
  
  All_FnO_Bhav_Copy_Files %>% as.Date(format="fo%d%b%Ybhav.csv") -> All_FnO_Bhav_Copy_Dates
  
  All_FnO_Bhav_Copy_Dates %>% max %>% format("%Y%m%d") -> Max_FnO_Bhav_Copy_Date
  
  if(length(Latest_File_Date) > 0)
  {
    if(Max_Nifty_Date > Latest_File_Date)
    {
      # Find the minimal set of FnO bhav copies
      All_FnO_Bhav_Copy_Dates %>% format("%Y%m%d") %>% {.>Latest_File_Date} %>%
        All_FnO_Bhav_Copy_Files[.] -> Relevant_FnO_files
      
      Relevant_FnO_files %>% as.Date(format="fo%d%b%Ybhav.csv") %>% format("%Y") %>% 
        as.integer %>% {.>=Threshold_Year} %>%
        Relevant_FnO_files[.] -> Relevant_FnO_files
    } # End of 'if(Max_Nifty_Date > Latest_File_Date)'
  }else{
    All_FnO_Bhav_Copy_Dates %>% as.Date(format="fo%d%b%Ybhav.csv") %>% format("%Y") %>% 
      as.integer %>% {.>=Threshold_Year} %>%
      All_FnO_Bhav_Copy_Files[.]  -> Relevant_FnO_files
  } # End of 'if(length(Latest_File_Date) > 0)'
  
  Total_New_Symbols <- c()
  
  FnO_Bhav_Database %>% setwd
  
  if(length(Relevant_FnO_files) > 0)
  {
    # i_1 <- 1
    for (i_1 in 1:length(Relevant_FnO_files))
    {
      print(paste0("Processing ",i_1, " out off ",
                   length(Relevant_FnO_files),sep=''))
      
      i_1 %>% Relevant_FnO_files[.] %>% read.csv -> temp_FnO_file
      
      temp_FnO_file %>% filter(INSTRUMENT == "FUTSTK") %>% 
        select("SYMBOL") %>% unlist %>% as.character %>% 
        unique %>% c(Total_New_Symbols,.) -> Total_New_Symbols
      
      Total_New_Symbols <- Total_New_Symbols[Total_New_Symbols!=""]
    } # End of 'for (i_1 in 1:length(Relevant_FnO_files))'
  } # End of 'if(length(Relevant_FnO_files) > 0)'
  
  if(length(Total_New_Symbols) > 0)
  {
    Total_New_Symbols %>% c(.,Latest_File_Symbols) %>% unique -> Total_New_Symbols
  } # End of 'if(length(Total_New_Symbols) > 0)'
  
  if(length(Total_New_Symbols) >= length(Latest_File_Symbols))
  {
    Total_New_Symbols %>% unique -> Total_Symbols_Unique
    
    Total_Symbols_Unique %>% c(.,Latest_File_Symbols) %>% unique -> Total_Symbols_Unique
    
    Total_Symbols_Unique <- Total_Symbols_Unique[Total_Symbols_Unique!=""]
    
    Symbols_in_EnFnO_Database %>% setwd
    
    if (!(temp_FUTSTK_symbols_filename %in% list.files()))
    {
      print(paste0("Making new ", temp_FUTSTK_symbols_filename, sep=''))
      
      Total_Symbols_Unique %>% as.data.frame %>% 
        `colnames<-`("Unique_FUTSTK_Symbols") %>% 
        write.csv(file=temp_FUTSTK_symbols_filename,row.names = F)
    }else{
      print(paste0("Overwriting ", temp_FUTSTK_symbols_filename, sep=''))
      
      file.remove(temp_FUTSTK_symbols_filename)
      
      Total_Symbols_Unique %>% as.data.frame %>% 
        `colnames<-`("Unique_FUTSTK_Symbols") %>% 
        write.csv(file=temp_FUTSTK_symbols_filename,row.names = F)
    } # End of 'if (!(temp_FUTSTK_symbols_filename %in% list.files()))'
  }else{
    print(paste0("No new symbols found, latest: ", Max_Nifty_Date,sep=''))
  } # End of 'if(length(Total_New_Symbols) >= length(Latest_File_Symbols))'
}else{
  print("Latest FUTSTK file already present")
} # End of 'if (!(temp_FUTSTK_symbols_filename %in% list.files()))'

#################################################################################
#################################################################################

#################################################################################
#################################### Goals ######################################

# Date: 2020-May-28
# Author: Arunabha Sarkar

# Goals: Finding all symbols in EnFnO
# File Name: Symbols_in_EnFnO

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

# "C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% paste0("/Nifty 50 Historical Data Investing dot com",
                            sep='') -> Nifty50_Database

Central_Database %>% paste0("/NSE EQ Bhav Copies",sep='') -> EQ_Bhav_Database

Central_Database %>% paste0("/NSE FnO Bhav Copies",sep='') -> FnO_Bhav_Database

Central_Database %>% paste0("/Symbols in EnFnO",sep='') -> Symbols_in_EnFnO_Database

Central_Database %>% setwd()

if(!file.exists('Symbols in EnFnO'))
{
  print("Creating 'Symbols in EnFnO' directory.")
  dir.create(file.path(Central_Database, 'Symbols in EnFnO'))
}else{
  print("'Symbols in EnFnO' directory already exists.")
} # End of 'if(!file.exists('Symbols in EnFnO'))

#################################################################################
#################################################################################

#################################################################################
############################# Finding FnO Symbols ###############################

# Find latest date of NIFTY50

Nifty50_Database %>% setwd

list.files() %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>% 
  as.Date(format="%Y%m%d") %>% which.max %>% list.files()[.] %>% 
  read.csv %>% select(Date) %>% unlist %>% as.character %>% 
  as.Date(format="%b %d, %Y") %>% max %>% format("%Y%m%d") -> Max_Nifty_Date

Max_Nifty_Date %>% as.Date(format = "%Y%m%d") %>% format("%Y%m%d") %>% 
  paste0("_All_FnO_Symbols.csv",sep = '') -> temp_FnO_symbols_filename

Symbols_in_EnFnO_Database %>% setwd
Symbols_in_EnFnO_Database_File_List <- list.files(pattern = "FnO")

if(length(Symbols_in_EnFnO_Database_File_List) > 0)
{
  Symbols_in_EnFnO_Database_File_List %>% strsplit(.,split = ' ') %>% map(1) %>% 
    unlist %>% as.Date(format="%Y%m%d") %>% which.max %>% 
    Symbols_in_EnFnO_Database_File_List[.] %T>% {. ->> Latest_File} %>% 
    read.csv %>% select(Unique_FnO_Symbols) %>% unlist %>% 
    as.character -> Latest_File_Symbols
  
  Latest_File %>% strsplit(split = '_') %>% .[[1]] %>% .[1] -> Latest_File_Date
}else{
  Latest_File_Symbols <- c()
  Latest_File_Date <- c()
  Latest_File <- c()
} # Endo of 'if(length(Symbols_in_EnFnO_Database_File_List) > 0)'

if (!(temp_FnO_symbols_filename %in% list.files()))
{
  # Find latest FnO if possible
  FnO_Bhav_Database %>% setwd
  
  list.files() -> All_FnO_Bhav_Copy_Files
  
  All_FnO_Bhav_Copy_Files %>% as.Date(format="fo%d%b%Ybhav.csv") -> All_FnO_Bhav_Copy_Dates
  
  All_FnO_Bhav_Copy_Dates %>% max %>% format("%Y%m%d") -> Max_FnO_Bhav_Copy_Date
  
  if(length(Latest_File_Date) > 0)
  {
    if(Max_Nifty_Date > Latest_File_Date)
    {
      # Find the minimal set of FnO bhav copies
      All_FnO_Bhav_Copy_Dates %>% format("%Y%m%d") %>% {.>Latest_File_Date} %>%
        All_FnO_Bhav_Copy_Files[.] -> Relevant_FnO_files
      
      Relevant_FnO_files %>% as.Date(format="fo%d%b%Ybhav.csv") %>% format("%Y") %>% 
        as.integer %>% {.>=Threshold_Year} %>%
        Relevant_FnO_files[.] -> Relevant_FnO_files
    }
  }else{
    All_FnO_Bhav_Copy_Dates %>% as.Date(format="fo%d%b%Ybhav.csv") %>% format("%Y") %>% 
      as.integer %>% {.>=Threshold_Year} %>%
      All_FnO_Bhav_Copy_Files[.]  -> Relevant_FnO_files
  } # End of 'if(length(Latest_File_Date) > 0)
  
  Total_New_Symbols <- c()
  
  FnO_Bhav_Database %>% setwd
  
  if(length(Relevant_FnO_files) > 0)
  {
    # i_1 <- 1
    for (i_1 in 1:length(Relevant_FnO_files))
    {
      print(paste0("Processing ",i_1, " out off ",
                   length(Relevant_FnO_files),sep=''))
      
      i_1 %>% Relevant_FnO_files[.] %>% read.csv -> temp_FnO_file
      
      temp_FnO_file %>% select("SYMBOL") %>% unlist %>% as.character %>% 
        unique %>% c(Total_New_Symbols,.) -> Total_New_Symbols
      
      Total_New_Symbols <- Total_New_Symbols[Total_New_Symbols!=""]
    } # End of 'for (i_1 in 1:length(Relevant_FnO_files))'
  } # End of 'if(length(Relevant_FnO_files) > 0)'
  
  if(length(Total_New_Symbols) > 0)
  {
    Total_New_Symbols %>% unique -> Total_New_Symbols
  } # End of 'if(length(Total_New_Symbols) > 0)'
  
  if(length(Total_New_Symbols) >= length(Latest_File_Symbols))
  {
    Total_New_Symbols %>% unique -> Total_Symbols_Unique
    
    Total_Symbols_Unique %>% c(.,Latest_File_Symbols) %>% unique -> Total_Symbols_Unique
    
    Total_Symbols_Unique <- Total_Symbols_Unique[Total_Symbols_Unique!=""]
    
    Symbols_in_EnFnO_Database %>% setwd
    
    if (!(temp_FnO_symbols_filename %in% list.files()))
    {
      print(paste0("Making new ", temp_FnO_symbols_filename, sep=''))
      
      Total_Symbols_Unique %>% as.data.frame %>% 
        `colnames<-`("Unique_FnO_Symbols") %>% 
        write.csv(file=temp_FnO_symbols_filename,row.names = F)
    }else{
      print(paste0("Overwriting ", temp_FnO_symbols_filename, sep=''))
      
      file.remove(temp_FnO_symbols_filename)
      
      Total_Symbols_Unique %>% as.data.frame %>% 
        `colnames<-`("Unique_FnO_Symbols") %>% 
        write.csv(file=temp_FnO_symbols_filename,row.names = F)
    } # End of 'if (!(temp_FnO_symbols_filename %in% list.files()))'
  }else{
    print(paste0("No new symbols found, latest: ", Max_Nifty_Date,sep=''))
  } # End of 'if(length(Total_New_Symbols) >= length(Latest_File_Symbols))'
}else{
  print("Latest FnO file already present")
} # End of # 'if (!(temp_FnO_symbols_filename %in% list.files()))'

#################################################################################
#################################################################################

#################################################################################
############################## Finding EQ Symbols ###############################

# Find latest date of NIFTY50

Nifty50_Database %>% setwd

list.files() %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>% 
  as.Date(format="%Y%m%d") %>% which.max %>% list.files()[.] %>% 
  read.csv %>% select(Date) %>% unlist %>% as.character %>% 
  as.Date(format="%b %d, %Y") %>% max %>% format("%Y%m%d") -> Max_Nifty_Date

Max_Nifty_Date %>% as.Date(format = "%Y%m%d") %>% format("%Y%m%d") %>% 
  paste0("_All_EQ_Symbols.csv",sep = '') -> temp_EQ_symbols_filename

Symbols_in_EnFnO_Database %>% setwd
Symbols_in_EnFnO_Database_File_List <- list.files(pattern = "EQ")

if(length(Symbols_in_EnFnO_Database_File_List) > 0)
{
  Symbols_in_EnFnO_Database_File_List %>% strsplit(.,split = ' ') %>% map(1) %>% 
    unlist %>% as.Date(format="%Y%m%d") %>% which.max %>% 
    Symbols_in_EnFnO_Database_File_List[.] %T>% {. ->> Latest_File} %>% 
    read.csv %>% select(Unique_EQ_Symbols) %>% unlist %>% 
    as.character -> Latest_File_Symbols
  
  Latest_File %>% strsplit(split = '_') %>% .[[1]] %>% .[1] -> Latest_File_Date
}else{
  Latest_File_Symbols <- c()
  Latest_File_Date <- c()
  Latest_File <- c()
} # End of 'if(length(Symbols_in_EnFnO_Database_File_List) > 0)'

if (!(temp_EQ_symbols_filename %in% list.files()))
{
  # Find latest FnO if possible
  EQ_Bhav_Database %>% setwd
  
  list.files() -> All_EQ_Bhav_Copy_Files
  
  All_EQ_Bhav_Copy_Files %>% as.Date(format="cm%d%b%Ybhav.csv") -> All_EQ_Bhav_Copy_Dates
  
  All_EQ_Bhav_Copy_Dates %>% max %>% format("%Y%m%d") -> Max_EQ_Bhav_Copy_Date
  
  if(length(Latest_File_Date) > 0)
  {
    if(Max_Nifty_Date > Latest_File_Date)
    {
      # Find the minimal set of FnO bhav copies
      All_EQ_Bhav_Copy_Dates %>% format("%Y%m%d") %>% {.>Latest_File_Date} %>%
        All_EQ_Bhav_Copy_Files[.] -> Relevant_EQ_files
      
      Relevant_EQ_files %>% as.Date(format="cm%d%b%Ybhav.csv") %>% format("%Y") %>% 
        as.integer %>% {.>=Threshold_Year} %>%
        Relevant_EQ_files[.] -> Relevant_EQ_files
    } # End of 'if(Max_Nifty_Date > Latest_File_Date)'
  }else{
    All_EQ_Bhav_Copy_Dates %>% as.Date(format="cm%d%b%Ybhav.csv") %>% format("%Y") %>% 
      as.integer %>% {.>=Threshold_Year} %>%
      All_EQ_Bhav_Copy_Files[.]  -> Relevant_EQ_files
  } # End of 'if(length(Latest_File_Date) > 0)'
  
  Total_New_Symbols <- c()
  
  EQ_Bhav_Database %>% setwd
  
  if(length(Relevant_EQ_files) > 0)
  {
    # i_1 <- 1
    for (i_1 in 1:length(Relevant_EQ_files))
    {
      print(paste0("Processing ",i_1, " out off ",
                   length(Relevant_EQ_files),sep=''))
      
      i_1 %>% Relevant_EQ_files[.] %>% read.csv -> temp_EQ_file
      
      temp_EQ_file %>% select("SYMBOL") %>% unlist %>% as.character %>% 
        unique %>% c(Total_New_Symbols,.) -> Total_New_Symbols
      
      Total_New_Symbols <- Total_New_Symbols[Total_New_Symbols!=""]
    } # End of 'for (i_1 in 1:length(Relevant_EQ_files))'
  } # End of 'if(length(Relevant_EQ_files) > 0)'
  
  if(length(Total_New_Symbols) > 0)
  {
    Total_New_Symbols %>% unique -> Total_New_Symbols
  } # End of 'if(length(Total_New_Symbols) > 0)'
  
  if(length(Total_New_Symbols) >= length(Latest_File_Symbols))
  {
    Total_New_Symbols %>% unique -> Total_Symbols_Unique
    
    Total_Symbols_Unique %>% c(.,Latest_File_Symbols) %>% unique -> Total_Symbols_Unique
    
    Total_Symbols_Unique <- Total_Symbols_Unique[Total_Symbols_Unique!=""]
    
    Symbols_in_EnFnO_Database %>% setwd
    
    if (!(temp_EQ_symbols_filename %in% list.files()))
    {
      print(paste0("Making new ", temp_EQ_symbols_filename, sep=''))
      
      Total_Symbols_Unique %>% as.data.frame %>% 
        `colnames<-`("Unique_EQ_Symbols") %>% 
        write.csv(file=temp_EQ_symbols_filename,row.names = F)
    }else{
      print(paste0("Overwriting ", temp_EQ_symbols_filename, sep=''))
      
      file.remove(temp_EQ_symbols_filename)
      
      Total_Symbols_Unique %>% as.data.frame %>% 
        `colnames<-`("Unique_EQ_Symbols") %>% 
        write.csv(file=temp_EQ_symbols_filename,row.names = F)
    } # End of 'if (!(temp_EQ_symbols_filename %in% list.files()))'
  }else{
    print(paste0("No new symbols found, latest: ", Max_Nifty_Date,sep=''))
  } # End of 'if(length(Total_New_Symbols) >= length(Latest_File_Symbols))'
}else{
  print("Latest EQ file already present")
} # End of 'if (!(temp_EQ_symbols_filename %in% list.files()))'

#################################################################################
#################################################################################

#################################################################################
#################################### Goals ######################################

# Date: 2020-May-28
# Author: Arunabha Sarkar

# Goals: Make Futures Index Time Series
# File Name: FUTIDX_TS

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

# "C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% paste0("/Nifty 50 Historical Data Investing dot com",
                            sep='') -> Nifty50_Database

Central_Database %>% paste0("/NSE EQ Bhav Copies",sep='') -> EQ_Bhav_Database

Central_Database %>% paste0("/NSE FnO Bhav Copies",sep='') -> FnO_Bhav_Database

Central_Database %>% paste0("/Symbols in EnFnO",sep='') -> Symbols_in_EnFnO_Database

Central_Database %>% paste0("/FUTIDX_TS",sep='') -> FUTIDX_TS_Database

Central_Database %>% setwd()

if(!file.exists('FUTIDX_TS'))
{
  print("Creating 'FUTIDX_TS' directory.")
  dir.create(file.path(Central_Database, 'FUTIDX_TS'))
}else{
  print("'FUTIDX_TS' directory already exists.")
} # End of 'if(!file.exists('FUTIDX_TS'))'

#################################################################################
#################################################################################

#################################################################################
############################ Finding/Making FUTIDX ##############################

# Find latest date of NIFTY50

Nifty50_Database %>% setwd

list.files() %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>% 
  as.Date(format="%Y%m%d") %>% which.max %>% list.files()[.] %>% 
  read.csv %>% select(Date) %>% unlist %>% as.character %>% 
  as.Date(format="%b %d, %Y") %>% max %>% format("%Y%m%d") -> Max_Nifty_Date

# Finding the list of FUTIDX
Symbols_in_EnFnO_Database %>% setwd
Symbols_in_EnFnO_Database_File_List <- list.files(pattern = "FUTIDX")

if(length(Symbols_in_EnFnO_Database_File_List) > 0)
{
  Symbols_in_EnFnO_Database_File_List %>% strsplit(.,split = ' ') %>% map(1) %>% 
    unlist %>% as.Date(format="%Y%m%d") %>% which.max %>% 
    Symbols_in_EnFnO_Database_File_List[.] %T>% {. ->> Latest_File} %>% 
    read.csv %>% select(Unique_FUTIDX_Symbols) %>% unlist %>% 
    as.character -> Latest_File_Symbols
  
  Latest_File %>% strsplit(split = '_') %>% .[[1]] %>% .[1] -> Latest_File_Date
}else{
  Latest_File_Symbols <- c()
  Latest_File_Date <- c()
  Latest_File <- c()
} # End of 'if(length(Symbols_in_EnFnO_Database_File_List) > 0)'

if(length(Latest_File_Symbols) > 0)
{
  # Looping over each Symbol
  # i_1 <- 4
  for(i_1 in 1:length(Latest_File_Symbols))
  {
    i_1 %>% Latest_File_Symbols[.] %>% {. ->> temp_Symbol} %>% 
      paste0("Checking ",.,sep='') %>% print
    
    # Checking if this Symbol exists in the FUTIDX_TS folder
    # Returning required 'Relevant_files'
    FUTIDX_TS_Database %>% setwd
    
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
        } # End of 'if (temp_Symbol %in% temp_relevant_symbols)'
      }# End of Finding correct ending point of 'Relevant_files'
      (Starting_Index_Relevant_files:Ending_Index_Relevant_files) %>% 
        Relevant_files[.] -> Relevant_files
    }else{
      # When prior file is found
      print(paste0("Prior file found for: ", temp_Symbol, sep=''))
      Prior_temp_Symbol_file_found <- TRUE
      FUTIDX_TS_Database %>% setwd
      
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
      FUTIDX_TS_Database %>% setwd
      
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
                 
                 stringsAsFactors = F) -> temp_Symbol_Data
    } # End of checking for overwrite
    
    
    # Now we have 'Relevant_files'. Looping over this for 'temp_Symbol'
    if (length(Relevant_files) > 0)
    {
      # i_2 <- 1330
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
        
        temp_FnO %>% filter(INSTRUMENT == "FUTIDX") %>% 
          filter(SYMBOL == temp_Symbol) -> temp_FnO_of_temp_Symbol
        
        temp_Symbol_Data %>% colnames %>% length %>% 
          matrix(data = NA,nrow = 1, ncol = .) %>% 
          data.frame(stringsAsFactors = F) %>% 
          `colnames<-`(colnames(temp_Symbol_Data)) -> temp_df
        
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
          
          # Tackling for SPOT values for temp_df
          Nifty50_Database %>% setwd
          
          list.files() %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>% 
            as.Date(format="%Y%m%d") %>% which.max %>% list.files()[.] %>% 
            read.csv -> Nifty_Index
          
          Nifty_Index %>% select(Date) %>% unlist %>% as.character %>% 
            as.Date(format="%b %d, %Y") %>% format("%Y-%m-%d") -> Nifty_Index$Date
          
          which(Nifty_Index$Date == temp_date) %>% 
            Nifty_Index[.,2:5] -> temp_df[,c(37,34,35,36)]
        } # End of 'if(nrow(temp_FnO_of_temp_Symbol) > 0)'
        
        if(!is.na(temp_df$Date_YYYYMMDD))
        {
          temp_df %>% rbind(temp_Symbol_Data,.) -> temp_Symbol_Data
        } # End of 'if(!is.na(temp_df$Date_YYYYMMDD))'
      } # End of 'for (i_2 in 1:length(Relevant_files))'
      if(nrow(temp_Symbol_Data) > 0)
      {
        # Make output file
        FUTIDX_TS_Database %>% setwd
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

# "C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

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

# "C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

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

#################################################################################
#################################### Goals ######################################

# Date: 2020-June-03
# Author: Arunabha Sarkar

# Goals: Make Options Index Time Series
# File Name: OPTIDX_TS

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

Threshold_Year <- 2007 # For STK, this has to be 2011

Max_Number_of_Expiries_in_Database <- 10 # This is only for indexes, the limit of NIFTYIT !

Percentages_from_spot <- c(-25,-20,-15,-10,-5,-4,-3,-2,2,3,4,5,10,15,20,25) # In addition to SPOT prices; 16 stikes max

# "C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% paste0("/Nifty 50 Historical Data Investing dot com",
                            sep='') -> Nifty50_Database  # To find out which day is open & NIFTY Spot

Central_Database %>% paste0("/BANKNIFTY 50 Historical Data Investing dot com",
                            sep='') -> BankNifty_Database  # To find out BANKNIFTY Spot

Central_Database %>% paste0("/NIFTYIT 50 Historical Data Investing dot com",
                            sep='') -> NiftyIT_Database  # To find out NIFTYIT Spot

Central_Database %>% paste0("/NSE EQ Bhav Copies",sep='') -> EQ_Bhav_Database # To add spot value

Central_Database %>% paste0("/NSE FnO Bhav Copies",sep='') -> FnO_Bhav_Database # To get the raw data

Central_Database %>% paste0("/Symbols in EnFnO",sep='') -> Symbols_in_EnFnO_Database # To get the symbols

Central_Database %>% paste0("/OPTIDX_TS",sep='') -> OPTIDX_TS_Database

Central_Database %>% setwd()

if(!file.exists('OPTIDX_TS'))
{
  print("Creating 'OPTIDX_TS' directory.")
  dir.create(file.path(Central_Database, 'OPTIDX_TS'))
}else{
  print("'OPTIDX_TS' directory already exists.")
} # End of 'if(!file.exists('OPTIDX_TS'))'

#################################################################################
#################################################################################

#################################################################################
##################### 'OPTIDX_TS' database column names #########################

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
  # i_4 <- 8
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

#################################################################################
#################################################################################

#################################################################################
############################ Finding/Making OPTIDX ##############################

# Find latest date of NIFTY50
Nifty50_Database %>% setwd

list.files() %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>% 
  as.Date(format="%Y%m%d") %>% which.max %>% list.files()[.] %>% 
  read.csv %>% {. ->> Nifty_Data} %>% select(Date) %>% unlist %>% as.character %>% 
  as.Date(format="%b %d, %Y") %>% max %>% format("%Y%m%d") -> Max_Nifty_Date
colnames(Nifty_Data) <- c("Date","Last","Open","High","Low","Volume","Change")
Nifty_Data %>% select(Date) %>% unlist %>% as.character %>% as.Date(format("%b %d, %Y")) %>% 
  format("%Y%m%d") -> Nifty_Data$Date

# Find latest date of BANKNIFTY
BankNifty_Database %>% setwd

list.files() %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>% 
  as.Date(format="%Y%m%d") %>% which.max %>% list.files()[.] %>% 
  read.csv -> BankNifty_Data
colnames(BankNifty_Data) <- c("Date","Last","Open","High","Low","Volume","Change")
BankNifty_Data %>% select(Date) %>% unlist %>% as.character %>% as.Date(format("%b %d, %Y")) %>% 
  format("%Y%m%d") -> BankNifty_Data$Date

# Find latest date of NIFTYIT
NiftyIT_Database %>% setwd

list.files() %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>% 
  as.Date(format="%Y%m%d") %>% which.max %>% list.files()[.] %>% 
  read.csv -> NiftyIT_Data
colnames(NiftyIT_Data) <- c("Date","Last","Open","High","Low","Volume","Change")
NiftyIT_Data %>% select(Date) %>% unlist %>% as.character %>% as.Date(format("%b %d, %Y")) %>% 
  format("%Y%m%d") -> NiftyIT_Data$Date

# Finding the list of OPTIDX, same as FUTIDX
Symbols_in_EnFnO_Database %>% setwd
Symbols_in_EnFnO_Database_File_List <- list.files(pattern = "FUTIDX")

if(length(Symbols_in_EnFnO_Database_File_List) > 0)
{
  Symbols_in_EnFnO_Database_File_List %>% strsplit(.,split = ' ') %>% map(1) %>% 
    unlist %>% as.Date(format="%Y%m%d") %>% which.max %>% 
    Symbols_in_EnFnO_Database_File_List[.] %T>% {. ->> Latest_File} %>% 
    read.csv %>% select(Unique_FUTIDX_Symbols) %>% unlist %>% 
    as.character -> Latest_File_Symbols
  
  Latest_File %>% strsplit(split = '_') %>% .[[1]] %>% .[1] -> Latest_File_Date
}else{
  Latest_File_Symbols <- c()
  Latest_File_Date <- c()
  Latest_File <- c()
} # End of 'if(length(Symbols_in_EnFnO_Database_File_List) > 0)'

if(length(Latest_File_Symbols) > 0)
{
  # Looping over each Symbol
  # i_1 <- 1; i_1 <- 2
  for(i_1 in 1:length(Latest_File_Symbols))
  {
    if(i_1 > 2)
    {
      print("Not going beyond NIFTY, BANKNIFTY")
      break
    }# End of 'if(i_1 > 2)'
    
    if (Latest_File_Symbols[i_1] == "NIFTY")
    {
      temp_SPOT_Data <- Nifty_Data
    }# End of 'if (Latest_File_Symbols[i_1] == "NIFTY")'
    
    if (Latest_File_Symbols[i_1] == "BANKNIFTY")
    {
      temp_SPOT_Data <- BankNifty_Data
    }# End of 'if (Latest_File_Symbols[i_1] == "BANKNIFTY")'
    
    if (Latest_File_Symbols[i_1] == "NIFTYIT")
    {
      temp_SPOT_Data <- NiftyIT_Data
    }# End of 'if (Latest_File_Symbols[i_1] == "NIFTYIT")'
    
    if (!(Latest_File_Symbols[i_1] %in% c("NIFTYIT","BANKNIFTY","NIFTY")))
    {
      temp_SPOT_Data <- data.frame(stringsAsFactors = F)
    }# End of 'if (Latest_File_Symbols[i_1] %in% c("NIFTYIT","BANKNIFTY","NIFTY"))'
    
    i_1 %>% Latest_File_Symbols[.] %>% {. ->> temp_Symbol} %>% 
      paste0("Checking ",.,sep='') %>% print
    
    # Checking if this Symbol exists in the OPTIDX_TS folder
    # Returning required 'Relevant_files'
    OPTIDX_TS_Database %>% setwd
    
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
            }
          }
        }
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
            }
          }
        }
      }# End of Finding correct ending point of 'Relevant_files'
      (Starting_Index_Relevant_files:Ending_Index_Relevant_files) %>% 
        Relevant_files[.] -> Relevant_files
    }else{
      # When prior file is found
      print(paste0("Prior file found for: ", temp_Symbol, sep=''))
      Prior_temp_Symbol_file_found <- TRUE
      OPTIDX_TS_Database %>% setwd
      
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
                }
              }
            } # End of 'for (i_r_2 in length(Trial_points_index_Relevant_files):1)'
            
            if(!Atleast_Found_One)
            {
              Overwrite_Required <- FALSE
              Relevant_files <- c()
              print(paste0("Prior file for ", temp_Symbol, " is upto date", sep=''))
              next
            }
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
      OPTIDX_TS_Database %>% setwd
      
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
      # i_2 <- 1; i_2 <- 1330; i_2 <- 1320; i_2 <- 3172; i_2 <- length(Relevant_files) - 1
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
        
        temp_FnO %>% filter(INSTRUMENT == "OPTIDX") %>% 
          filter(SYMBOL == temp_Symbol) %>% 
          filter(OPTIONTYPE == "CE") -> temp_CE_FnO_of_temp_Symbol
        
        temp_FnO %>% filter(INSTRUMENT == "OPTIDX") %>% 
          filter(SYMBOL == temp_Symbol) %>% 
          filter(OPTIONTYPE == "PE") -> temp_PE_FnO_of_temp_Symbol
        
        # EQ_Bhav_Database %>% setwd
        # i_2 %>% Relevant_files[.] %>% as.Date(format="fo%d%b%Ybhav.csv") -> temp_file_date
        # 
        # temp_file_date %>% format("%b") -> temp_file_date_month
        # temp_file_date %>% format("%d") -> temp_file_date_day
        # temp_file_date %>% format("%Y") -> temp_file_date_year
        # 
        # paste0("cm",temp_file_date_day,toupper(temp_file_date_month),temp_file_date_year,
        #        "bhav.csv") %>% read.csv -> temp_EQ_of_temp_Symbol
        
        temp_SPOT_Data %>% select(Date) %>% unlist %>% as.character %>% as.Date(format="%Y%m%d") %>% 
          format("%Y-%m-%d") %>% 
          {which(. == temp_relevant_file_date)} %>% temp_SPOT_Data[.,]  -> Relevant_SPOT_Data
        
        if(nrow(Relevant_SPOT_Data) > 0 & 
           nrow(temp_CE_FnO_of_temp_Symbol) > 0 &
           nrow(temp_PE_FnO_of_temp_Symbol) > 0)
        {
          colnames(Relevant_SPOT_Data) <- c("Date","Close","Open","High","Low","Volume","Change")
          ############################################################################################
          # Entry from 'temp_SPOT_Data' into 'temp_df'
          Relevant_SPOT_Data$Date %>% as.numeric -> temp_df[1,grep("Date_YYYYMMDD", colnames(temp_df))]
          "OPTIDX" -> temp_df[1,grep("INSTRUMENT", colnames(temp_df))]
          temp_Symbol -> temp_df[1,grep("SYMBOL", colnames(temp_df))]
          
          Relevant_SPOT_Data$Open %>% as.character %>% gsub(",","",.) %>% as.numeric -> temp_SPOT_OPEN
          temp_SPOT_OPEN_columns <- grep("OPEN_SPOT", colnames(temp_df))
          # i_open <- 1
          for (i_open in 1:length(temp_SPOT_OPEN_columns))
          {
            temp_df[1,temp_SPOT_OPEN_columns[i_open]] <- temp_SPOT_OPEN
          } # End of 'for (i_open in 1:length(temp_SPOT_OPEN_columns))'
          
          Relevant_SPOT_Data$Close %>% as.character %>% gsub(",","",.) %>% as.numeric -> temp_SPOT_CLOSE
          temp_SPOT_CLOSE_columns <- grep("CLOSE_SPOT", colnames(temp_df))
          # i_close <- 1
          for (i_close in 1: length(temp_SPOT_CLOSE_columns))
          {
            temp_df[1,temp_SPOT_CLOSE_columns[i_close]] <- temp_SPOT_CLOSE
          } # End of 'for (i_close in 1: length(temp_SPOT_CLOSE_columns))'
          
          Relevant_SPOT_Data$High %>% as.character %>% gsub(",","",.) %>% as.numeric -> temp_SPOT_HIGH
          temp_SPOT_HIGH_columns <- grep("HIGH_SPOT", colnames(temp_df))
          # i_high <- 1
          for (i_high in 1: length(temp_SPOT_HIGH_columns))
          {
            temp_df[1,temp_SPOT_HIGH_columns[i_high]] <- temp_SPOT_HIGH
          } # End of 'for (i_high in 1: length(temp_SPOT_HIGH_columns))'
          
          Relevant_SPOT_Data$Low %>% as.character %>% gsub(",","",.) %>% as.numeric -> temp_SPOT_LOW
          temp_SPOT_LOW_columns <- grep("LOW_SPOT", colnames(temp_df))
          # i_low <- 2
          for (i_low in 1: length(temp_SPOT_LOW_columns))
          {
            temp_df[1,temp_SPOT_LOW_columns[i_low]] <- temp_SPOT_LOW
          } # End of 'for (i_low in 1: length(temp_SPOT_LOW_columns))'
          
          Relevant_SPOT_Data$Volume %>% as.character -> temp_SPOT_VOL
          temp_SPOT_VOL %>% gsub(pattern = "K", replacement = "*1000") %>% 
            gsub(pattern = "M", replacement = "*1000000") %>% 
            gsub(pattern = "B", replacement = "*1000000000") %>% 
            gsub(pattern = "T", replacement = "*1000000000000") %>% 
            gsub(pattern = "-", replacement = "") %>% 
            gsub(pattern = " ", replacement = "") -> temp_SPOT_VOL
          
          if(nchar(temp_SPOT_VOL) == 0)
          {
            temp_SPOT_VOL <- NA
          }# End of 'if(!is.numeric(temp_SPOT_VOL))'
          
          if(!is.numeric(temp_SPOT_VOL))
          {
            eval(parse(text=temp_SPOT_VOL)) -> temp_SPOT_VOL
            if(length(temp_SPOT_VOL) == 0)
            {
              temp_SPOT_VOL <- NA
            }# End of 'if(length(temp_SPOT_VOL) == 0)'
          }# End of 'if(is.numeric(temp_SPOT_VOL))'
          
          temp_SPOT_VOL_columns <- grep("TOTTRDVAL_SPOT", colnames(temp_df))
          # i_vol <- 1
          for (i_vol in 1: length(temp_SPOT_VOL_columns))
          {
            temp_df[1,temp_SPOT_VOL_columns[i_vol]] <- temp_SPOT_VOL
          } # End of 'for (i_vol in 1: length(temp_SPOT_VOL_columns))'
          ############################################################################################
          
          ############################################################################################
          # CE ATM stuff, SPOT
          temp_relevant_file_date %>% as.Date(format="%Y-%m-%d") %>% format("%Y%m%d") -> temp_date
          
          if(!is.na(temp_SPOT_CLOSE) & is.numeric(temp_SPOT_CLOSE))
          {
            # Most recent expiry is always as top, but still, we need to still find it for ATM
            
            temp_FnO %>% filter(INSTRUMENT == "OPTIDX") %>% 
              filter(SYMBOL == temp_Symbol) %>% 
              filter(OPTIONTYPE == "CE") -> temp_CE_FnO_of_temp_Symbol
            
            temp_CE_FnO_of_temp_Symbol %>% select(EXPIRY_DT) %>% unlist %>% as.character %>% 
              as.Date(format="%d-%b-%Y") -> temp_CE_FnO_of_temp_Symbol_Expiry_Dates
            
            temp_CE_FnO_of_temp_Symbol_Expiry_Dates -> temp_CE_FnO_of_temp_Symbol$EXPIRY_DT
            
            temp_CE_FnO_of_temp_Symbol_Expiry_Dates %>% min -> temp_CE_FnO_of_temp_Symbol_Min_Expiry_Date
            
            temp_CE_FnO_of_temp_Symbol %>% 
              filter(EXPIRY_DT==temp_CE_FnO_of_temp_Symbol_Min_Expiry_Date) -> temp_CE_FnO_of_temp_Symbol_Nearest_Expiry
            
            temp_CE_FnO_of_temp_Symbol_Nearest_Expiry %>% 
              filter(STRIKE_PR >= temp_SPOT_CLOSE) %>%
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
          }# End of 'if(!is.na(temp_SPOT_CLOSE) & is.numeric(temp_SPOT_CLOSE))'
          
          ############################################################################################
          # PE ATM stuff, SPOT: temp_NIFTY_CLOSE
          temp_relevant_file_date %>% as.Date(format="%Y-%m-%d") %>% format("%Y%m%d") -> temp_date
          
          if(!is.na(temp_SPOT_CLOSE) & is.numeric(temp_SPOT_CLOSE))
          {
            # Most recent expiry is always as top, but still, we need to still find it for ATM
            temp_FnO %>% filter(INSTRUMENT == "OPTIDX") %>% 
              filter(SYMBOL == temp_Symbol) %>% 
              filter(OPTIONTYPE == "PE") -> temp_PE_FnO_of_temp_Symbol
            
            temp_PE_FnO_of_temp_Symbol %>% select(EXPIRY_DT) %>% unlist %>% as.character %>% 
              as.Date(format="%d-%b-%Y") -> temp_PE_FnO_of_temp_Symbol_Expiry_Dates
            
            temp_PE_FnO_of_temp_Symbol_Expiry_Dates -> temp_PE_FnO_of_temp_Symbol$EXPIRY_DT
            
            temp_PE_FnO_of_temp_Symbol_Expiry_Dates %>% min -> temp_PE_FnO_of_temp_Symbol_Min_Expiry_Date
            
            temp_PE_FnO_of_temp_Symbol %>% 
              filter(EXPIRY_DT==temp_PE_FnO_of_temp_Symbol_Min_Expiry_Date) -> temp_PE_FnO_of_temp_Symbol_Nearest_Expiry
            
            temp_PE_FnO_of_temp_Symbol_Nearest_Expiry %>% 
              filter(STRIKE_PR <= temp_SPOT_CLOSE) %>%
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
          }# End of 'if(!is.na(temp_SPOT_CLOSE) & is.numeric(temp_SPOT_CLOSE))'
          
          ############################################################################################
          
          ############################################################################################
          # Entry from 'temp_CE_FnO_of_temp_Symbol' into 'temp_df' for different expiries and strikes
          # # Making 'temp_CE_FnO_of_temp_Symbol_Unique_Expiry' in ascending order
          temp_FnO %>% filter(INSTRUMENT == "OPTIDX") %>% 
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
              
              temp_SPOT_CLOSE %>% {.*(1+temp_percentage/100)} -> temp_theoretical_strike
              
              temp_CE_FnO_of_temp_Symbol %>% filter(STRIKE_PR <= 1.01*temp_theoretical_strike) %>% 
                filter(STRIKE_PR >= 0.99*temp_theoretical_strike) -> temp_relevant_strike_candidates
              
              temp_relevant_strike_candidates %>% select(EXPIRY_DT) %>% unlist %>% as.character %>% 
                as.Date(format="%d-%b-%Y") %>% format("%Y%m%d") %>% {which(. == temp_expiry_date)} %>%  
                temp_relevant_strike_candidates[.,] -> temp_relevant_strike_candidates
              
              if(nrow(temp_relevant_strike_candidates) > 0)
              {
                temp_relevant_strike_candidates %>% select(STRIKE_PR) %>% unlist %>% as.numeric %>% 
                  {.-temp_theoretical_strike} %>% abs -> temp_spreads
                
                which.min(temp_spreads) %>% .[1] %>% temp_relevant_strike_candidates[.,] -> temp_relevant_strike_candidates
                
                temp_percentage %>% abs -> temp_percentage_abs
                
                temp_colname <- c()
                Plus_Minus <- c()
                if(temp_percentage >= 0)
                {
                  Plus_Minus <- "Plus"
                }else{
                  Plus_Minus <- "Minus"
                }# End of 'if(temp_percentage >= 0)'
                
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
          temp_FnO %>% filter(INSTRUMENT == "OPTIDX") %>% 
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
            # i_10 <- 2; i_10 <- 1; i_10 <- 5
            for (i_10 in 1: length(Percentages_from_spot))
            {
              i_10 %>% Percentages_from_spot[.] -> temp_percentage
              
              temp_Start_Col_Index <- expiry_date_col_index + 9*(i_10-1) + 1
              temp_End_Col_Index <- temp_Start_Col_Index + 9 - 1
              
              temp_SPOT_CLOSE %>% {.*(1+temp_percentage/100)} -> temp_theoretical_strike
              
              temp_PE_FnO_of_temp_Symbol %>% filter(STRIKE_PR <= 1.01*temp_theoretical_strike) %>% 
                filter(STRIKE_PR >= 0.99*temp_theoretical_strike) -> temp_relevant_strike_candidates
              
              temp_relevant_strike_candidates %>% select(EXPIRY_DT) %>% unlist %>% as.character %>% 
                as.Date(format="%d-%b-%Y") %>% format("%Y%m%d") %>% {which(. == temp_expiry_date)} %>%  
                temp_relevant_strike_candidates[.,] -> temp_relevant_strike_candidates
              
              if(nrow(temp_relevant_strike_candidates) > 0)
              {
                temp_relevant_strike_candidates %>% select(STRIKE_PR) %>% unlist %>% as.numeric %>% 
                  {.-temp_theoretical_strike} %>% abs -> temp_spreads
                
                which.min(temp_spreads) %>% .[1] %>% temp_relevant_strike_candidates[.,] -> temp_relevant_strike_candidates
                
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
          print(paste0("Error! Check !! nrows of 'Relevant_SPOT_Data'", 
                       " 'temp_CE_FnO_of_temp_Symbol',",
                       " 'temp_PE_FnO_of_temp_Symbol' are: ",
                       nrow(Relevant_SPOT_Data),", ",
                       nrow(temp_CE_FnO_of_temp_Symbol),", ",
                       nrow(temp_PE_FnO_of_temp_Symbol)," respectively",sep=''))
        }# End of 'if(nrow(Relevant_SPOT_Data) > 0 & '
        
        # Adding 'temp_df' to 
        if(!is.na(temp_df$Date_YYYYMMDD_C))
        {
          temp_df %>% rbind(temp_Symbol_Data,.) -> temp_Symbol_Data
        } # End of 'if(!is.na(temp_df$Date_YYYYMMDD_C))'
        
      }# End of 'for (i_2 in 1:length(Relevant_files))'
      if(nrow(temp_Symbol_Data) > 0)
      {
        # Make output file
        OPTIDX_TS_Database %>% setwd
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

# "C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

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

# "C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

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

# #################################################################################
# #################################### Goals ######################################
# 
# # Date: 2020-June-11
# # Author: Arunabha Sarkar
# 
# # Goals: Using files in 'OPTIDX_TS', make excel file for drag and drop Option Strategy
# # File Name: OPTIDX_Excel_TS
# 
# #################################################################################
# #################################################################################
# 
# #################################################################################
# ##################### Initializing and loading Libraries ########################
# 
# library(dplyr)
# library(purrr)
# library(openxlsx)
# 
# #################################################################################
# #################################################################################
# 
# #################################################################################
# ############################### Set Directories #################################
# 
# # "C:/Users/aurnabha/Desktop/Central Database" -> Central_Database
# 
# Central_Database %>% paste0("/Nifty 50 Historical Data Investing dot com",
#                             sep='') -> Nifty50_Database  # To find out which day is open & NIFTY Spot
# 
# Central_Database %>% paste0("/NSE EQ Bhav Copies",sep='') -> EQ_Bhav_Database # To add spot value
# 
# Central_Database %>% paste0("/NSE FnO Bhav Copies",sep='') -> FnO_Bhav_Database # To get the raw data
# 
# Central_Database %>% paste0("/OPTIDX_TS",sep='') -> OPTIDX_TS_Database
# 
# Central_Database %>% paste0("/BANKNIFTY 50 Historical Data Investing dot com",sep='') -> BankNifty_Database
# 
# Central_Database %>% setwd
# 
# if(!file.exists('OPTIDX_Excel_TS'))
# {
#   print("Creating 'OPTIDX_Excel_TS' directory.")
#   dir.create(file.path(Central_Database, 'OPTIDX_Excel_TS'))
# }else{
#   print("'OPTIDX_Excel_TS' directory already exists.")
# } # End of 'if(!file.exists('OPTIDX_Excel_TS'))'
# 
# Central_Database %>% paste0("/OPTIDX_Excel_TS",sep='') -> OPTIDX_Excel_TS_Database
# 
# #################################################################################
# #################################################################################
# 
# #################################################################################
# ############################## Hyper parameters #################################
# 
# MAX_Expiry_Trials <- 20
# 
# MAX_Strike_p_Trials <- 40
# 
# temp_FnO_colnames <- c("INSTRUMENT","SYMBOL","EXPIRY_DT","STRIKE_PR","OPTIONTYPE",
#                        "OPEN","HIGH","LOW","CLOSE","SETTLE_PR","CONTRACTS","VAL_INLAKH",
#                        "OPEN_INT","CHG_IN_OI","TIMESTAMP")
# 
# temp_EQ_colnames <- c("SYMBOL","SERIES","OPEN","HIGH","LOW","CLOSE","LAST","PREVCLOSE",
#                       "TOTTRDQTY","TOTTRDVAL","TIMESTAMP")
# 
# #################################################################################
# #################################################################################
# 
# #################################################################################
# ############################# Finding Latest Date ###############################
# 
# # Find latest date of NIFTY50
# Nifty50_Database %>% setwd
# 
# list.files() %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>% 
#   as.Date(format="%Y%m%d") %>% which.max %>% list.files()[.] %>% 
#   read.csv %>% {. ->> Nifty_Data} %>% select(Date) %>% unlist %>% as.character %>% 
#   as.Date(format="%b %d, %Y") %>% max %>% format("%Y%m%d") %>% as.numeric -> Max_Nifty_Date
# 
# colnames(Nifty_Data) <- c("Date","Last","Open","High","Low","Volume","Change")
# 
# Nifty_Data %>% select(Date) %>% unlist %>% as.character %>% as.Date(format("%b %d, %Y")) %>% 
#   format("%Y%m%d") %>% as.numeric -> Nifty_Data$Date
# 
# Nifty_Data$Date %>% unlist %>% as.numeric %>% sort -> N50_Dates_YYYYMMDD
# 
# #################################################################################
# #################################################################################
# 
# #################################################################################
# ########################## Making OPTIDX_Excel files ############################
# 
# OPTIDX_TS_Database %>% setwd
# list.files() -> OPTIDX_TS_Database_Files
# 
# OPTIDX_TS_Database_Files %>% grep("BANK",.) %>% OPTIDX_TS_Database_Files[.] -> OPTIDX_TS_Database_Files
# 
# if (length(OPTIDX_TS_Database_Files) > 0)
# {
#   print(paste0("Found '", length(OPTIDX_TS_Database_Files), "' files in 'OPTIDX_TS_Database'",sep=''))
#   
#   # i_1 <- 1
#   for (i_1 in 1: length(OPTIDX_TS_Database_Files))
#   {
#     # if(i_1 > 1)
#     # {
#     #   break
#     # } # End of 'if(i_1 > 1)'
#     
#     OPTIDX_TS_Database %>% setwd
#     
#     # Need top tackle existing file and new file case
#     Old_Excel_Exists <- FALSE
#     Overwrite_Required <- FALSE
#     
#     i_1 %>% OPTIDX_TS_Database_Files[.] -> temp_Options_csv_filename
#     
#     temp_Options_csv_filename %>% strsplit(split='_') %>% .[[1]] %>% .[1] -> temp_Symbol
#     
#     # Checking if previous excel output exists
#     OPTIDX_Excel_TS_Database %>% setwd
#     
#     temp_Symbol %>% paste0(.,"_Excel_TS.xlsx",sep='') -> temp_Options_excel_filename
#     OPTIDX_Excel_TS_Database %>% setwd
#     list.files() -> OPTIDX_Excel_TS_Database_Files
#     
#     if(temp_Options_excel_filename %in% OPTIDX_Excel_TS_Database_Files)
#     {
#       Old_Excel_Exists <- TRUE
#       print(paste0("Prior Excel file found for ", temp_Symbol,sep=''))
#     }else{
#       Old_Excel_Exists <- FALSE
#       print(paste0("NO prior Excel file for ", temp_Symbol,sep=''))
#     } # End of 'if(temp_Options_excel_filename %in% OPTIDX_Excel_TS_Database_Files)'
#     
#     if(Old_Excel_Exists)
#     {
#       # Find relevant new rows & Option Chain
#       temp_Options_excel_filename %>% strsplit(split="_") %>% .[[1]] %>% .[1] -> temp_Symbol_from_filename
#       
#       temp_Symbol_from_filename %>% gsub("&","_and_",x=.) -> temp_Symbol_in_sheet_names
#       
#       # Finding latest date in sheet and comparing to 'Max_Nifty_Date'
#       OPTIDX_Excel_TS_Database %>% setwd
#       
#       temp_Options_excel_filename %>% read.xlsx(sheet = 1) %>% select(Date_YYYYMMDD_C) %>% unlist %>% 
#         max -> temp_old_file_max_date
#       
#       if (temp_old_file_max_date >= as.numeric(Max_Nifty_Date))
#       {
#         # Up to date, no update required
#         Old_Excel_Exists <- TRUE
#         Overwrite_Required <- FALSE
#       }else{
#         # Not up to date, update is required
#         Old_Excel_Exists <- TRUE
#         Overwrite_Required <- TRUE
#         
#         # Finding number of expiries from 'OPTIDX_TS_Database' database
#         OPTIDX_TS_Database %>% setwd
#         
#         temp_Symbol_from_filename %>% paste0(.,"_Options.csv",sep='') %>% read.csv %T>% 
#           {. ->> temp_Symbol_raw_csv} %>% colnames -> temp_Symbol_raw_csv_colnames
#         
#         expiry_counter <- MAX_Expiry_Trials
#         Found_Expiry_limit <- FALSE
#         while (!Found_Expiry_limit)
#         {
#           expiry_trial <- grep(paste0("_Expiry_",expiry_counter,"_",sep=''),temp_Symbol_raw_csv_colnames)
#           if (length(expiry_trial) > 0 & expiry_counter > 0)
#           {
#             Found_Expiry_limit <- TRUE
#           } # End of 'if (length(expiry_trial) > 0)'
#           
#           if (Found_Expiry_limit)
#           {
#             break
#           } # End of 'if (Found_Expiry_limit)'
#           expiry_counter <- expiry_counter - 1
#           
#           if (expiry_counter < 0)
#           {
#             print(paste0("Check source csv file: '", temp_Symbol_from_filename, "_Options.csv",
#                          "', no expiry column found",sep=""))
#             break
#           } # End of 'if (expiry_counter < 0)'
#         } # End of 'while (!Found_Expiry_limit)'
#         
#         if (expiry_counter > 0 & Found_Expiry_limit)
#         {
#           "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#             paste0(" found ", expiry_counter, " expiries",sep='') %>% print
#           
#           # From 'expiry_counter', finding out sheet number of ATMs, expiries & other sheets
#           1 -> temp_sheet_num_CE_ATM
#           temp_sheet_num_CE_ATM %>% {(.+1):(.+expiry_counter)} -> temp_sheet_num_CE_expiries
#           temp_sheet_num_CE_expiries %>% max %>% {.+1} -> temp_sheet_num_PE_ATM
#           temp_sheet_num_PE_ATM %>% {(.+1):(.+expiry_counter)} -> temp_sheet_num_PE_expiries
#           temp_sheet_num_PE_expiries %>% max %>% {.+1} -> temp_sheet_num_EQ
#           temp_sheet_num_EQ %>% {.+1} -> temp_sheet_num_F
#           temp_sheet_num_F %>% {.+1} -> temp_sheet_num_O_CE
#           temp_sheet_num_O_CE %>% {.+1} -> temp_sheet_num_O_PE
#           
#           # Constructing output Excel Sheet:
#           temp_Symbol_Output_excel <- createWorkbook()
#           
#           ################## Tackling 'temp_sheet_num_CE_ATM'
#           "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#             paste0(" updating CE ATM",sep='') %>% print
#           
#           Symbol_CE_ATM_Sheetname <- paste0(temp_Symbol_in_sheet_names,"_CE_ATM",sep='')
#           addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_CE_ATM_Sheetname)
#           
#           OPTIDX_Excel_TS_Database %>% setwd
#           
#           temp_Options_excel_filename %>% read.xlsx(sheet = temp_sheet_num_CE_ATM) -> temp_Old_CE_ATM_sheet
#           
#           temp_Old_CE_ATM_sheet[,1] %>% unlist %>% as.numeric -> temp_Old_CE_ATM_sheet_dates
#           temp_Old_CE_ATM_sheet_dates %>% min -> temp_Old_CE_ATM_sheet_dates_min
#           
#           which(N50_Dates_YYYYMMDD >= temp_Old_CE_ATM_sheet_dates_min) %>% 
#             N50_Dates_YYYYMMDD[.] -> temp_relevant_N50_Dates_YYYYMMDD
#           
#           temp_relevant_N50_Dates_YYYYMMDD %>% {. %in% temp_Old_CE_ATM_sheet_dates} %>% {!.} %>% 
#             temp_relevant_N50_Dates_YYYYMMDD[.] -> temp_missing_dates_temp_sheet_num_CE_ATM
#           
#           temp_Old_CE_ATM_sheet %>% colnames -> temp_correct_colnames
#           
#           temp_correct_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
#             `colnames<-`(temp_correct_colnames) -> temp_new_CE_ATM
#           
#           if (length(temp_missing_dates_temp_sheet_num_CE_ATM) > 0)
#           {
#             temp_correct_colnames <- colnames(temp_Old_CE_ATM_sheet)
#             
#             # i_temp_missing_dates_temp_sheet_num_CE_ATM <- 1
#             for (i_temp_missing_dates_temp_sheet_num_CE_ATM in 1:length(temp_missing_dates_temp_sheet_num_CE_ATM)) 
#             {
#               temp_correct_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
#                 `colnames<-`(temp_correct_colnames) -> temp_temp_new_CE_ATM
#               
#               i_temp_missing_dates_temp_sheet_num_CE_ATM %>% 
#                 temp_missing_dates_temp_sheet_num_CE_ATM[.] %>% 
#                 {which(temp_Symbol_raw_csv$Date_YYYYMMDD_C == .)} %>% 
#                 temp_Symbol_raw_csv[.,temp_correct_colnames] -> temp_temp_new_CE_ATM
#               
#               if(!is.na(temp_temp_new_CE_ATM[1,1]))
#               {
#                 temp_temp_new_CE_ATM %>% rbind(temp_new_CE_ATM,.) -> temp_new_CE_ATM
#               } # End of 'if(!is.na(temp_temp_new_CE_ATM[1,1]))'
#             } # End of 'for (i_temp_missing_dates_temp_sheet_num_CE_ATM in 1:length(temp_missing_dates_temp_sheet_num_CE_ATM))'
#           } # End of 'if (length(temp_missing_dates_temp_sheet_num_CE_ATM) > 0)'
#           
#           temp_new_CE_ATM %>% rbind(temp_Old_CE_ATM_sheet,.) -> temp_new_CE_ATM
#           
#           writeData(temp_Symbol_Output_excel, sheet = Symbol_CE_ATM_Sheetname, x = temp_new_CE_ATM)
#           ################## 
#           
#           ################## Tackling 'temp_sheet_num_CE_expiries'
#           
#           # i_temp_sheet_num_CE_expiries <- 1
#           for (i_temp_sheet_num_CE_expiries in 1:length(temp_sheet_num_CE_expiries))
#           {
#             "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#               paste0(" updating CE Expiry #: ", i_temp_sheet_num_CE_expiries,
#                      " of ",length(temp_sheet_num_CE_expiries),sep='') %>% print
#             
#             # For each expiry, there is % difference based strike, both plus and minus
#             Symbol_CE_Expiry_Sheetname <- paste0(temp_Symbol_in_sheet_names,"_CE_Expiry_",
#                                                  i_temp_sheet_num_CE_expiries,sep='')
#             
#             addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_CE_Expiry_Sheetname)
#             
#             OPTIDX_Excel_TS_Database %>% setwd
#             
#             temp_Options_excel_filename %>% 
#               read.xlsx(sheet = temp_sheet_num_CE_expiries[i_temp_sheet_num_CE_expiries]) -> temp_Old_CE_Expiry_sheet
#             
#             temp_Old_CE_Expiry_sheet %>% colnames -> temp_correct_colnames
#             
#             temp_Old_CE_Expiry_sheet[,1] %>% unlist %>% as.numeric -> temp_Old_CE_Expiry_sheet_dates
#             temp_Old_CE_Expiry_sheet_dates %>% min -> temp_Old_CE_Expiry_sheet_dates_min
#             
#             which(N50_Dates_YYYYMMDD >= temp_Old_CE_Expiry_sheet_dates_min) %>% 
#               N50_Dates_YYYYMMDD[.] -> temp_relevant_N50_Dates_YYYYMMDD
#             
#             temp_relevant_N50_Dates_YYYYMMDD %>% {. %in% temp_Old_CE_Expiry_sheet_dates} %>% {!.} %>% 
#               temp_relevant_N50_Dates_YYYYMMDD[.] -> temp_missing_dates_temp_sheet_num_CE_Expiry
#             
#             temp_correct_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
#               `colnames<-`(temp_correct_colnames) -> temp_new_CE_Expiry
#             
#             if (length(temp_missing_dates_temp_sheet_num_CE_Expiry) > 0)
#             {
#               temp_correct_colnames <- colnames(temp_Old_CE_Expiry_sheet)
#               
#               # i_temp_missing_dates_temp_sheet_num_CE_Expiry <- 1
#               for (i_temp_missing_dates_temp_sheet_num_CE_Expiry in 1:length(temp_missing_dates_temp_sheet_num_CE_Expiry)) 
#               {
#                 temp_correct_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
#                   `colnames<-`(temp_correct_colnames) -> temp_temp_new_CE_Expiry
#                 
#                 i_temp_missing_dates_temp_sheet_num_CE_Expiry %>% 
#                   temp_missing_dates_temp_sheet_num_CE_Expiry[.] %>% 
#                   {which(temp_Symbol_raw_csv$Date_YYYYMMDD_C == .)} %>% 
#                   temp_Symbol_raw_csv[.,temp_correct_colnames] -> temp_temp_new_CE_Expiry
#                 
#                 if(!is.na(temp_temp_new_CE_Expiry[1,1]))
#                 {
#                   temp_temp_new_CE_Expiry %>% rbind(temp_new_CE_Expiry,.) -> temp_new_CE_Expiry
#                 } # End of 'if(!is.na(temp_temp_new_CE_Expiry[1,1]))'
#               } # End of 'for (i_temp_missing_dates_temp_sheet_num_CE_Expiry in 1:length(temp_missing_dates_temp_sheet_num_CE_Expiry)) '
#             } # End of 'if (length(temp_missing_dates_temp_sheet_num_CE_Expiry) > 0)'
#             
#             temp_new_CE_Expiry %>% rbind(temp_Old_CE_Expiry_sheet,.) -> temp_new_CE_Expiry
#             
#             writeData(temp_Symbol_Output_excel, sheet = Symbol_CE_Expiry_Sheetname, x = temp_new_CE_Expiry)
#             
#           }# End of 'for (i_temp_sheet_num_CE_expiries in 1:length(temp_sheet_num_CE_expiries))'
#           ################## 
#           
#           ################## Tackling 'temp_sheet_num_PE_ATM'
#           "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#             paste0(" updating PE ATM",sep='') %>% print
#           
#           Symbol_PE_ATM_Sheetname <- paste0(temp_Symbol_in_sheet_names,"_PE_ATM",sep='')
#           addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_PE_ATM_Sheetname)
#           
#           OPTIDX_Excel_TS_Database %>% setwd
#           
#           temp_Options_excel_filename %>% read.xlsx(sheet = temp_sheet_num_PE_ATM) -> temp_Old_PE_ATM_sheet
#           
#           temp_Old_PE_ATM_sheet[,1] %>% unlist %>% as.numeric -> temp_Old_PE_ATM_sheet_dates
#           temp_Old_PE_ATM_sheet_dates %>% min -> temp_Old_PE_ATM_sheet_dates_min
#           
#           which(N50_Dates_YYYYMMDD >= temp_Old_PE_ATM_sheet_dates_min) %>% 
#             N50_Dates_YYYYMMDD[.] -> temp_relevant_N50_Dates_YYYYMMDD
#           
#           temp_relevant_N50_Dates_YYYYMMDD %>% {. %in% temp_Old_PE_ATM_sheet_dates} %>% {!.} %>% 
#             temp_relevant_N50_Dates_YYYYMMDD[.] -> temp_missing_dates_temp_sheet_num_PE_ATM
#           
#           temp_Old_PE_ATM_sheet %>% colnames -> temp_correct_colnames
#           
#           temp_correct_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
#             `colnames<-`(temp_correct_colnames) -> temp_new_PE_ATM
#           
#           if (length(temp_missing_dates_temp_sheet_num_PE_ATM) > 0)
#           {
#             temp_correct_colnames <- colnames(temp_Old_PE_ATM_sheet)
#             
#             # i_temp_missing_dates_temp_sheet_num_PE_ATM <- 1
#             for (i_temp_missing_dates_temp_sheet_num_PE_ATM in 1:length(temp_missing_dates_temp_sheet_num_PE_ATM)) 
#             {
#               temp_correct_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
#                 `colnames<-`(temp_correct_colnames) -> temp_temp_new_PE_ATM
#               
#               i_temp_missing_dates_temp_sheet_num_PE_ATM %>% 
#                 temp_missing_dates_temp_sheet_num_PE_ATM[.] %>% 
#                 {which(temp_Symbol_raw_csv$Date_YYYYMMDD_C == .)} %>% 
#                 temp_Symbol_raw_csv[.,temp_correct_colnames] -> temp_temp_new_PE_ATM
#               
#               if(!is.na(temp_temp_new_PE_ATM[1,1]))
#               {
#                 temp_temp_new_PE_ATM %>% rbind(temp_new_PE_ATM,.) -> temp_new_PE_ATM
#               } # End of 'if(!is.na(temp_temp_new_PE_ATM[1,1]))'
#             } # End of 'for (i_temp_missing_dates_temp_sheet_num_PE_ATM in 1:length(temp_missing_dates_temp_sheet_num_PE_ATM))'
#           } # End of 'if (length(temp_missing_dates_temp_sheet_num_PE_ATM) > 0)'
#           
#           temp_new_PE_ATM %>% rbind(temp_Old_PE_ATM_sheet,.) -> temp_new_PE_ATM
#           
#           writeData(temp_Symbol_Output_excel, sheet = Symbol_PE_ATM_Sheetname, x = temp_new_PE_ATM)
#           ################## 
#           
#           ################## Tackling 'temp_sheet_num_PE_expiries'
#           
#           # i_temp_sheet_num_PE_expiries <- 1
#           for (i_temp_sheet_num_PE_expiries in 1:length(temp_sheet_num_PE_expiries))
#           {
#             "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#               paste0(" updating PE Expiry #: ", i_temp_sheet_num_PE_expiries,
#                      " of ",length(temp_sheet_num_PE_expiries),sep='') %>% print
#             
#             # For each expiry, there is % difference based strike, both plus and minus
#             Symbol_PE_Expiry_Sheetname <- paste0(temp_Symbol_in_sheet_names,"_PE_Expiry_",
#                                                  i_temp_sheet_num_PE_expiries,sep='')
#             
#             addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_PE_Expiry_Sheetname)
#             
#             OPTIDX_Excel_TS_Database %>% setwd
#             
#             temp_Options_excel_filename %>% 
#               read.xlsx(sheet = temp_sheet_num_PE_expiries[i_temp_sheet_num_PE_expiries]) -> temp_Old_PE_Expiry_sheet
#             
#             temp_Old_PE_Expiry_sheet %>% colnames -> temp_correct_colnames
#             
#             temp_Old_PE_Expiry_sheet[,1] %>% unlist %>% as.numeric -> temp_Old_PE_Expiry_sheet_dates
#             temp_Old_PE_Expiry_sheet_dates %>% min -> temp_Old_PE_Expiry_sheet_dates_min
#             
#             which(N50_Dates_YYYYMMDD >= temp_Old_PE_Expiry_sheet_dates_min) %>% 
#               N50_Dates_YYYYMMDD[.] -> temp_relevant_N50_Dates_YYYYMMDD
#             
#             temp_relevant_N50_Dates_YYYYMMDD %>% {. %in% temp_Old_PE_Expiry_sheet_dates} %>% {!.} %>% 
#               temp_relevant_N50_Dates_YYYYMMDD[.] -> temp_missing_dates_temp_sheet_num_PE_Expiry
#             
#             temp_correct_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
#               `colnames<-`(temp_correct_colnames) -> temp_new_PE_Expiry
#             
#             if (length(temp_missing_dates_temp_sheet_num_PE_Expiry) > 0)
#             {
#               temp_correct_colnames <- colnames(temp_Old_PE_Expiry_sheet)
#               
#               # i_temp_missing_dates_temp_sheet_num_PE_Expiry <- 1
#               for (i_temp_missing_dates_temp_sheet_num_PE_Expiry in 1:length(temp_missing_dates_temp_sheet_num_PE_Expiry)) 
#               {
#                 temp_correct_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
#                   `colnames<-`(temp_correct_colnames) -> temp_temp_new_PE_Expiry
#                 
#                 i_temp_missing_dates_temp_sheet_num_PE_Expiry %>% 
#                   temp_missing_dates_temp_sheet_num_PE_Expiry[.] %>% 
#                   {which(temp_Symbol_raw_csv$Date_YYYYMMDD_C == .)} %>% 
#                   temp_Symbol_raw_csv[.,temp_correct_colnames] -> temp_temp_new_PE_Expiry
#                 
#                 if(!is.na(temp_temp_new_PE_Expiry[1,1]))
#                 {
#                   temp_temp_new_PE_Expiry %>% rbind(temp_new_PE_Expiry,.) -> temp_new_PE_Expiry
#                 } # End of 'if(!is.na(temp_temp_new_PE_Expiry[1,1]))'
#               } # End of 'for (i_temp_missing_dates_temp_sheet_num_PE_Expiry in 1:length(temp_missing_dates_temp_sheet_num_PE_Expiry)) '
#             } # End of 'if (length(temp_missing_dates_temp_sheet_num_PE_Expiry) > 0)'
#             
#             temp_new_PE_Expiry %>% rbind(temp_Old_PE_Expiry_sheet,.) -> temp_new_PE_Expiry
#             
#             writeData(temp_Symbol_Output_excel, sheet = Symbol_PE_Expiry_Sheetname, x = temp_new_PE_Expiry)
#             
#           }# End of 'for (i_temp_sheet_num_PE_expiries in 1:length(temp_sheet_num_PE_expiries))'
#           ##################
#           
#           ################## Tackling 'temp_sheet_num_EQ'
#           "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#             paste0(" updating EQ",sep='') %>% print
#           
#           Symbol_EQ_Sheetname <- paste0(temp_Symbol_in_sheet_names,"_EQ",sep='')
#           addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_EQ_Sheetname)
#           
#           OPTIDX_Excel_TS_Database %>% setwd
#           
#           temp_Options_excel_filename %>% read.xlsx(sheet = temp_sheet_num_EQ) -> temp_Old_EQ_sheet
#           
#           temp_Old_EQ_sheet[,which(colnames(temp_Old_EQ_sheet) == "TIMESTAMP")] %>% unlist %>% 
#             as.numeric -> temp_Old_EQ_sheet_dates
#           temp_Old_EQ_sheet_dates %>% min -> temp_Old_EQ_sheet_dates_min
#           
#           which(N50_Dates_YYYYMMDD >= temp_Old_EQ_sheet_dates_min) %>% 
#             N50_Dates_YYYYMMDD[.] -> temp_relevant_N50_Dates_YYYYMMDD
#           
#           temp_relevant_N50_Dates_YYYYMMDD %>% {. %in% temp_Old_EQ_sheet_dates} %>% {!.} %>% 
#             temp_relevant_N50_Dates_YYYYMMDD[.] -> temp_missing_dates_temp_sheet_num_EQ
#           
#           temp_Old_EQ_sheet %>% colnames -> temp_correct_colnames
#           
#           temp_correct_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
#             `colnames<-`(temp_correct_colnames) -> temp_new_EQ
#           
#           if (length(temp_missing_dates_temp_sheet_num_EQ) > 0)
#           {
#             temp_correct_colnames <- colnames(temp_Old_EQ_sheet)
#             
#             # i_temp_missing_dates_temp_sheet_num_EQ <- 1
#             for (i_temp_missing_dates_temp_sheet_num_EQ in 1:length(temp_missing_dates_temp_sheet_num_EQ)) 
#             {
#               temp_correct_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
#                 `colnames<-`(temp_correct_colnames) -> temp_temp_new_EQ
#               
#               ##########################
#               
#               temp_Spot_Directory <- c()
#               
#               if (temp_Symbol == "BANKNIFTY")
#               {
#                 temp_Spot_Directory <- BankNifty_Database
#               }else{
#                 temp_Spot_Directory <- Nifty50_Database
#               } # End of 'if (temp_Symbol == "BANKNIFTY")'
#               
#               temp_Spot_Directory %>% setwd
#               
#               list.files() -> temp_Spot_Directory_files
#               
#               temp_Spot_Directory_files %>% strsplit(split=" ") %>% lapply(., function(YY){YY[1]}) %>% unlist %>% 
#                 as.numeric %>% sort %>% length %>% temp_Spot_Directory_files[.] %>% read.csv -> temp_Spot_Latest
#               
#               temp_Spot_Latest$Date %>% unlist %>% as.character %>% as.Date(format="%b %d, %Y") %>% 
#                 format("%Y%m%d") %>% as.numeric -> temp_Spot_Latest$Date
#               
#               # Find min & max date from temp_Symbol_raw_csv
#               
#               temp_Symbol_raw_csv %>% select(Date_YYYYMMDD_C) %>% unlist %>% as.character %>% 
#                 as.Date(format="%Y%m%d") -> All_temp_Symbol_raw_csv_dates
#               
#               All_temp_Symbol_raw_csv_dates %>% as.Date(format="%b %d, %Y") %>% 
#                 format("%Y%m%d") %>% as.numeric %>% max -> All_temp_Symbol_raw_csv_dates_Max
#               
#               All_temp_Symbol_raw_csv_dates %>% as.Date(format="%b %d, %Y") %>% 
#                 format("%Y%m%d") %>% as.numeric %>% min -> All_temp_Symbol_raw_csv_dates_Min
#               
#               temp_Spot_Latest %>% select(Date) %>% unlist %>% as.character %>% as.numeric %>% 
#                 {. <= All_temp_Symbol_raw_csv_dates_Max & . >= All_temp_Symbol_raw_csv_dates_Min} %>% 
#                 temp_Spot_Latest[.,-c(ncol(temp_Spot_Latest))] %>% 
#                 `colnames<-`(c("TIMESTAMP","CLOSE","OPEN","HIGH","LOW","TOTTRDVAL")) -> temp_Spot_Latest_relevant
#               
#               temp_Spot_Latest_relevant %>% select(TOTTRDVAL) %>% unlist %>% as.character %>% 
#                 gsub("-","0",.) %>% gsub("K","*1000",.) %>% gsub("M","*1000000",.) %>% 
#                 gsub("B","*1000000000",.) %>% gsub("T","*1000000000000",.) %>% as.list(.) %>% 
#                 lapply(.,function(MM){eval(parse(text=MM))}) %>% unlist -> temp_Spot_Latest_relevant$TOTTRDVAL
#               
#               temp_Spot_Latest_relevant$SYMBOL <- temp_Symbol
#               
#               temp_Spot_Latest_relevant$SERIES <- ""
#               
#               temp_Spot_Latest_relevant$PREVCLOSE <- ""
#               
#               temp_Spot_Latest_relevant$TOTTRDQTY <- ""
#               
#               temp_Spot_Latest_relevant[,c(7,8,3,4,2,5,9,10,6,1)] -> temp_Spot_Latest_relevant
#               
#               temp_Spot_Latest_relevant[with(temp_Spot_Latest_relevant, order(TIMESTAMP)), ] -> temp_Spot_Latest_relevant
#               
#               ##########################
#               
#               if(nrow(temp_Spot_Latest_relevant) > 0)
#               {
#                 temp_Spot_Latest_relevant -> temp_new_EQ
#                 
#               } # End of 'if(length(temp_relevant_EQ_file) > 0)'
#               
#             } # End of 'for (i_temp_missing_dates_temp_sheet_num_EQ in 1:length(temp_missing_dates_temp_sheet_num_EQ)) '
#           } # End of 'if (length(temp_missing_dates_temp_sheet_num_EQ) > 0)'
#           
#           temp_new_EQ[with(temp_new_EQ, order(TIMESTAMP)), ] -> temp_new_EQ
#           
#           writeData(temp_Symbol_Output_excel, sheet = Symbol_EQ_Sheetname, x = temp_new_EQ)
#           ##################
#           
#           ################## Tackling 'temp_sheet_num_F'
#           "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#             paste0(" updating F",sep='') %>% print
#           
#           Symbol_F_Sheetname <- paste0(temp_Symbol_in_sheet_names,"_F",sep='')
#           addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_F_Sheetname)
#           
#           OPTIDX_Excel_TS_Database %>% setwd
#           
#           temp_Options_excel_filename %>% read.xlsx(sheet = temp_sheet_num_F) -> temp_Old_F_sheet
#           
#           temp_Old_F_sheet[,which(colnames(temp_Old_F_sheet) == "TIMESTAMP")] %>% unlist %>% 
#             as.numeric -> temp_Old_F_sheet_dates
#           
#           temp_Old_F_sheet_dates %>% max -> temp_Old_F_sheet_dates_max
#           
#           which(N50_Dates_YYYYMMDD >= temp_Old_F_sheet_dates_max) %>% 
#             N50_Dates_YYYYMMDD[.] -> temp_relevant_N50_Dates_YYYYMMDD
#           
#           temp_relevant_N50_Dates_YYYYMMDD %>% {. %in% temp_Old_F_sheet_dates} %>% {!.} %>% 
#             temp_relevant_N50_Dates_YYYYMMDD[.] -> temp_missing_dates_temp_sheet_num_F
#           
#           temp_Old_F_sheet %>% colnames -> temp_correct_colnames
#           
#           temp_correct_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
#             `colnames<-`(temp_correct_colnames) -> temp_new_F
#           
#           if (length(temp_missing_dates_temp_sheet_num_F) > 0)
#           {
#             temp_correct_colnames <- colnames(temp_Old_F_sheet)
#             
#             # i_temp_missing_dates_temp_sheet_num_F <- 1
#             for (i_temp_missing_dates_temp_sheet_num_F in 1:length(temp_missing_dates_temp_sheet_num_F)) 
#             {
#               temp_correct_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
#                 `colnames<-`(temp_correct_colnames) -> temp_temp_new_F
#               
#               FnO_Bhav_Database %>% setwd
#               
#               list.files() -> All_FnO_files
#               
#               All_FnO_files %>% as.Date(format = "fo%d%b%Ybhav.csv") %>% 
#                 format("%Y%m%d") %>% as.character %>% as.numeric -> All_FnO_dates_YYYYMMDD
#               
#               which(All_FnO_dates_YYYYMMDD == temp_missing_dates_temp_sheet_num_F[i_temp_missing_dates_temp_sheet_num_F]) %>% 
#                 .[1] %>% All_FnO_files[.] -> temp_relevant_F_file
#               
#               if(length(temp_relevant_F_file) > 0)
#               {
#                 temp_relevant_F_file %>% read.csv -> temp_relevant_F_file_df
#                 
#                 temp_relevant_F_file_df %>% colnames %>% {which(. == "TIMESTAMP")} %>% 
#                   {1:.} %>% temp_relevant_F_file_df[,.] %>% 
#                   `colnames<-`(temp_FnO_colnames) -> temp_relevant_F_file_df
#                 
#                 temp_relevant_F_file_df %>% filter(SYMBOL == temp_Symbol_from_filename) %>% 
#                   filter(INSTRUMENT == "FUTIDX") -> temp_temp_new_F
#                 
#                 Correct_Date_format <- c()
#                 
#                 if(nchar(as.character(temp_temp_new_F[1,"TIMESTAMP"])) >= 10)
#                 {
#                   Correct_Date_format <- "%d-%b-%Y"
#                 }else{
#                   Correct_Date_format <- "%d-%b-%y"
#                 } # End of 'if(nchar(as.character(temp_temp_new_F[1,"TIMESTAMP"])) >= 10)'
#                 
#                 temp_temp_new_F$TIMESTAMP %>% unlist %>% as.character %>% as.Date(format=Correct_Date_format) %>% 
#                   format("%Y%m%d") %>% as.numeric -> temp_temp_new_F$TIMESTAMP
#                 
#                 temp_temp_new_F$EXPIRY_DT %>% unlist %>% as.character %>% as.Date(format=Correct_Date_format) %>% 
#                   format("%Y%m%d") %>% as.numeric -> temp_temp_new_F$EXPIRY_DT
#                 
#               } # End of 'if(length(temp_relevant_F_file) > 0)'
#               
#               if(!is.na(temp_temp_new_F[1,1]))
#               {
#                 temp_temp_new_F %>% rbind(temp_new_F,.) -> temp_new_F
#               } # End of 'if(!is.na(temp_temp_new_F[1,1]))'
#             } # End of 'for (i_temp_missing_dates_temp_sheet_num_F in 1:length(temp_missing_dates_temp_sheet_num_F))'
#           } # End of 'if (length(temp_missing_dates_temp_sheet_num_F) > 0)'
#           
#           temp_new_F[with(temp_new_F, order(TIMESTAMP,EXPIRY_DT,OPTIONTYPE,STRIKE_PR)), ] -> temp_new_F
#           
#           temp_new_F %>% rbind(temp_Old_F_sheet,.) -> temp_new_F
#           
#           writeData(temp_Symbol_Output_excel, sheet = Symbol_F_Sheetname, x = temp_new_F)
#           ##################
#           
#           ################## Tackling 'temp_sheet_num_O_CE'
#           "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#             paste0(" updating O CE",sep='') %>% print
#           
#           Symbol_O_CE_Sheetname <- paste0(temp_Symbol_in_sheet_names,"_O_CE",sep='')
#           addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_O_CE_Sheetname)
#           
#           OPTIDX_Excel_TS_Database %>% setwd
#           
#           temp_Options_excel_filename %>% read.xlsx(sheet = temp_sheet_num_O_CE) -> temp_Old_O_CE_sheet
#           
#           temp_Old_O_CE_sheet[,which(colnames(temp_Old_O_CE_sheet) == "TIMESTAMP")] %>% unlist %>% 
#             as.numeric -> temp_Old_O_CE_sheet_dates
#           temp_Old_O_CE_sheet_dates %>% min -> temp_Old_O_CE_sheet_dates_min
#           
#           which(N50_Dates_YYYYMMDD >= temp_Old_O_CE_sheet_dates_min) %>% 
#             N50_Dates_YYYYMMDD[.] -> temp_relevant_N50_Dates_YYYYMMDD
#           
#           temp_relevant_N50_Dates_YYYYMMDD %>% {. %in% temp_Old_O_CE_sheet_dates} %>% {!.} %>% 
#             temp_relevant_N50_Dates_YYYYMMDD[.] -> temp_missing_dates_temp_sheet_num_O_CE
#           
#           temp_Old_O_CE_sheet %>% colnames -> temp_correct_colnames
#           
#           temp_correct_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
#             `colnames<-`(temp_correct_colnames) -> temp_new_O_CE
#           
#           if (length(temp_missing_dates_temp_sheet_num_O_CE) > 0)
#           {
#             temp_correct_colnames <- colnames(temp_Old_O_CE_sheet)
#             
#             # i_temp_missing_dates_temp_sheet_num_O_CE <- 1
#             for (i_temp_missing_dates_temp_sheet_num_O_CE in 1:length(temp_missing_dates_temp_sheet_num_O_CE)) 
#             {
#               temp_correct_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
#                 `colnames<-`(temp_correct_colnames) -> temp_temp_new_O_CE
#               
#               FnO_Bhav_Database %>% setwd
#               
#               list.files() -> All_FnO_files
#               
#               All_FnO_files %>% as.Date(format = "fo%d%b%Ybhav.csv") %>% 
#                 format("%Y%m%d") %>% as.character %>% as.numeric -> All_FnO_dates_YYYYMMDD
#               
#               which(All_FnO_dates_YYYYMMDD == temp_missing_dates_temp_sheet_num_O_CE[i_temp_missing_dates_temp_sheet_num_O_CE]) %>% 
#                 .[1] %>% All_FnO_files[.] -> temp_relevant_O_CE_file
#               
#               if(length(temp_relevant_O_CE_file) > 0)
#               {
#                 temp_relevant_O_CE_file %>% read.csv -> temp_relevant_O_CE_file_df
#                 
#                 temp_relevant_O_CE_file_df %>% colnames %>% {which(. == "TIMESTAMP")} %>% 
#                   {1:.} %>% temp_relevant_O_CE_file_df[,.] %>% 
#                   `colnames<-`(temp_FnO_colnames) -> temp_relevant_O_CE_file_df
#                 
#                 temp_relevant_O_CE_file_df %>% filter(SYMBOL == temp_Symbol_from_filename) %>% 
#                   filter(INSTRUMENT == "OPTIDX") %>% filter(OPTIONTYPE == "CE") -> temp_temp_new_O_CE
#                 
#                 Correct_Date_format <- c()
#                 
#                 if(nchar(as.character(temp_temp_new_O_CE[1,"TIMESTAMP"])) >= 10)
#                 {
#                   Correct_Date_format <- "%d-%b-%Y"
#                 }else{
#                   Correct_Date_format <- "%d-%b-%y"
#                 } # End of 'if(nchar(as.character(temp_temp_new_O_CE[1,"TIMESTAMP"])) >= 10)'
#                 
#                 temp_temp_new_O_CE$TIMESTAMP %>% unlist %>% as.character %>% as.Date(format=Correct_Date_format) %>% 
#                   format("%Y%m%d") %>% as.numeric -> temp_temp_new_O_CE$TIMESTAMP
#                 
#                 temp_temp_new_O_CE$EXPIRY_DT %>% unlist %>% as.character %>% as.Date(format=Correct_Date_format) %>% 
#                   format("%Y%m%d") %>% as.numeric -> temp_temp_new_O_CE$EXPIRY_DT
#                 
#               } # End of 'if(length(temp_relevant_O_CE_file) > 0)'
#               
#               if(!is.na(temp_temp_new_O_CE[1,1]))
#               {
#                 temp_temp_new_O_CE %>% rbind(temp_new_O_CE,.) -> temp_new_O_CE
#               } # End of 'if(!is.na(temp_temp_new_O_CE[1,1]))'
#             } # End of 'for (i_temp_missing_dates_temp_sheet_num_O_CE in 1:length(temp_missing_dates_temp_sheet_num_O_CE))'
#           } # End of 'if (length(temp_missing_dates_temp_sheet_num_O_CE) > 0)'
#           
#           temp_new_O_CE[with(temp_new_O_CE, order(TIMESTAMP,EXPIRY_DT,OPTIONTYPE,STRIKE_PR)), ] -> temp_new_O_CE
#           
#           temp_new_O_CE %>% rbind(temp_Old_O_CE_sheet,.) -> temp_new_O_CE
#           
#           writeData(temp_Symbol_Output_excel, sheet = Symbol_O_CE_Sheetname, x = temp_new_O_CE)
#           ##################
#           
#           ################## Tackling 'temp_sheet_num_O_PE'
#           "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#             paste0(" updating O PE",sep='') %>% print
#           
#           Symbol_O_PE_Sheetname <- paste0(temp_Symbol_in_sheet_names,"_O_PE",sep='')
#           addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_O_PE_Sheetname)
#           
#           OPTIDX_Excel_TS_Database %>% setwd
#           
#           temp_Options_excel_filename %>% read.xlsx(sheet = temp_sheet_num_O_PE) -> temp_Old_O_PE_sheet
#           
#           temp_Old_O_PE_sheet[,which(colnames(temp_Old_O_PE_sheet) == "TIMESTAMP")] %>% unlist %>% 
#             as.numeric -> temp_Old_O_PE_sheet_dates
#           
#           temp_Old_O_PE_sheet_dates %>% min -> temp_Old_O_PE_sheet_dates_min
#           
#           which(N50_Dates_YYYYMMDD >= temp_Old_O_PE_sheet_dates_min) %>% 
#             N50_Dates_YYYYMMDD[.] -> temp_relevant_N50_Dates_YYYYMMDD
#           
#           temp_relevant_N50_Dates_YYYYMMDD %>% {. %in% temp_Old_O_PE_sheet_dates} %>% {!.} %>% 
#             temp_relevant_N50_Dates_YYYYMMDD[.] -> temp_missing_dates_temp_sheet_num_O_PE
#           
#           temp_Old_O_PE_sheet %>% colnames -> temp_correct_colnames
#           
#           temp_correct_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
#             `colnames<-`(temp_correct_colnames) -> temp_new_O_PE
#           
#           if (length(temp_missing_dates_temp_sheet_num_O_PE) > 0)
#           {
#             temp_correct_colnames <- colnames(temp_Old_O_PE_sheet)
#             
#             # i_temp_missing_dates_temp_sheet_num_O_PE <- 1
#             for (i_temp_missing_dates_temp_sheet_num_O_PE in 1:length(temp_missing_dates_temp_sheet_num_O_PE)) 
#             {
#               temp_correct_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
#                 `colnames<-`(temp_correct_colnames) -> temp_temp_new_O_PE
#               
#               FnO_Bhav_Database %>% setwd
#               
#               list.files() -> All_FnO_files
#               
#               All_FnO_files %>% as.Date(format = "fo%d%b%Ybhav.csv") %>% 
#                 format("%Y%m%d") %>% as.character %>% as.numeric -> All_FnO_dates_YYYYMMDD
#               
#               which(All_FnO_dates_YYYYMMDD == temp_missing_dates_temp_sheet_num_O_CE[i_temp_missing_dates_temp_sheet_num_O_PE]) %>% 
#                 .[1] %>% All_FnO_files[.] -> temp_relevant_O_PE_file
#               
#               if(length(temp_relevant_O_PE_file) > 0)
#               {
#                 temp_relevant_O_PE_file %>% read.csv -> temp_relevant_O_PE_file_df
#                 
#                 temp_relevant_O_PE_file_df %>% colnames %>% {which(. == "TIMESTAMP")} %>% 
#                   {1:.} %>% temp_relevant_O_PE_file_df[,.] %>% 
#                   `colnames<-`(temp_FnO_colnames) -> temp_relevant_O_PE_file_df
#                 
#                 temp_relevant_O_PE_file_df %>% filter(SYMBOL == temp_Symbol_from_filename) %>% 
#                   filter(INSTRUMENT == "OPTIDX") %>% filter(OPTIONTYPE == "PE") -> temp_temp_new_O_PE
#                 
#                 Correct_Date_format <- c()
#                 
#                 if(nchar(as.character(temp_temp_new_O_PE[1,"TIMESTAMP"])) >= 10)
#                 {
#                   Correct_Date_format <- "%d-%b-%Y"
#                 }else{
#                   Correct_Date_format <- "%d-%b-%y"
#                 } # End of 'if(nchar(as.character(temp_temp_new_O_PE[1,"TIMESTAMP"])) >= 10)'
#                 
#                 temp_temp_new_O_PE$TIMESTAMP %>% unlist %>% as.character %>% as.Date(format=Correct_Date_format) %>% 
#                   format("%Y%m%d") -> temp_temp_new_O_PE$TIMESTAMP
#                 
#                 temp_temp_new_O_PE$EXPIRY_DT %>% unlist %>% as.character %>% as.Date(format=Correct_Date_format) %>% 
#                   format("%Y%m%d") -> temp_temp_new_O_PE$EXPIRY_DT
#                 
#               } # End of 'if(length(temp_relevant_O_PE_file) > 0)'
#               
#               if(!is.na(temp_temp_new_O_PE[1,1]))
#               {
#                 temp_temp_new_O_PE %>% rbind(temp_new_O_PE,.) -> temp_new_O_PE
#               } # End of 'if(!is.na(temp_temp_new_O_PE[1,1]))'
#             } # End of 'for (i_temp_missing_dates_temp_sheet_num_O_PE in 1:length(temp_missing_dates_temp_sheet_num_O_PE))'
#           } # End of 'if (length(temp_missing_dates_temp_sheet_num_O_PE) > 0)'
#           
#           temp_new_O_PE[with(temp_new_O_PE, order(TIMESTAMP,EXPIRY_DT,OPTIONTYPE,STRIKE_PR)), ] -> temp_new_O_PE
#           
#           temp_new_O_PE %>% rbind(temp_Old_O_PE_sheet,.) -> temp_new_O_PE
#           
#           writeData(temp_Symbol_Output_excel, sheet = Symbol_O_PE_Sheetname, x = temp_new_O_PE)
#           ##################
#           
#         } # End of 'if (expiry_counter > 0 & expiry_counter)'
#         
#       } # End of '(temp_old_file_max_date >= as.numeric(Max_Nifty_Date))'
#       # Overwrite_Required <- FALSE # or TRUE, to be checked
#     }else{
#       Overwrite_Required <- FALSE # Because it is new writing
#       
#       # Reading old csv file
#       OPTIDX_TS_Database %>% setwd
#       temp_Options_csv_filename %>% read.csv -> temp_Symbol_raw_csv
#       
#       # Splitting into PE and CE
#       temp_Symbol_raw_csv[,as.integer(1:((ncol(temp_Symbol_raw_csv))/2))] -> temp_Symbol_raw_csv_CE
#       temp_Symbol_raw_csv[,((1 + ncol(temp_Symbol_raw_csv_CE)):ncol(temp_Symbol_raw_csv))] -> temp_Symbol_raw_csv_PE
#       
#       # Finding number of expiries
#       temp_Symbol_raw_csv_PE %>% colnames -> temp_Symbol_raw_csv_PE_colnames
#       temp_Symbol_raw_csv_CE %>% colnames -> temp_Symbol_raw_csv_CE_colnames
#       expiry_counter <- MAX_Expiry_Trials
#       Found_Expiry_limit <- FALSE
#       while (!Found_Expiry_limit)
#       {
#         expiry_trial <- grep(paste0("_Expiry_",expiry_counter,"_",sep=''),temp_Symbol_raw_csv_PE_colnames)
#         if (length(expiry_trial) > 0 & expiry_counter > 0)
#         {
#           Found_Expiry_limit <- TRUE
#         } # End of 'if (length(expiry_trial) > 0)'
#         
#         if (Found_Expiry_limit)
#         {
#           break
#         } # End of 'if (Found_Expiry_limit)'
#         expiry_counter <- expiry_counter - 1
#         
#         if (expiry_counter < 0)
#         {
#           print(paste0("Check source csv file: '", temp_Options_csv_filename,"' no expiry column found",sep=""))
#           break
#         } # End of 'if (expiry_counter < 0)'
#       } # End of 'while (!Found_Expiry_limit)'
#       
#       if (Found_Expiry_limit & expiry_counter > 0)
#       {
#         print(paste0("Found ", expiry_counter," expiries from file: '",temp_Options_csv_filename,"'",sep=""))
#         
#         # Constructing output Excel Sheet:
#         temp_Symbol_Output_excel <- createWorkbook()
#         temp_Symbol_Output_excel_sheetnames <- c()
#         Symbol_CE_ATM_Sheetname <- paste0(temp_Symbol,"_CE_ATM",sep='')
#         Symbol_CE_ATM_Sheetname %>% gsub("&","_and_",.) -> Symbol_CE_ATM_Sheetname
#         temp_Symbol_Output_excel_sheetnames <- c(temp_Symbol_Output_excel_sheetnames,Symbol_CE_ATM_Sheetname)
#         
#         print(paste0("Making 'CE ATM' of ", temp_Symbol,sep=''))
#         
#         # Constructing CE ATM
#         addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_CE_ATM_Sheetname)
#         grep("_ATM",temp_Symbol_raw_csv_CE_colnames) %>% max %>% {.+1} -> temp_Symbol_raw_csv_CE_max_ATM_column
#         writeData(temp_Symbol_Output_excel, sheet = Symbol_CE_ATM_Sheetname,
#                   x = temp_Symbol_raw_csv_CE[,(1:temp_Symbol_raw_csv_CE_max_ATM_column)])
#         
#         print(paste0("Making 'CE Expiries' of ", temp_Symbol,sep=''))
#         
#         # Constructing CE Expiries
#         # i_CE_expiry <- 1
#         for(i_CE_expiry in 1:expiry_counter)
#         {
#           paste0(temp_Symbol,"_CE_Expiry_",i_CE_expiry,sep="") -> temp_CE_temp_expiry_sheetname
#           temp_CE_temp_expiry_sheetname %>% gsub("&","_and_",.) -> temp_CE_temp_expiry_sheetname
#           temp_Symbol_Output_excel_sheetnames <- c(temp_Symbol_Output_excel_sheetnames,temp_CE_temp_expiry_sheetname)
#           
#           i_CE_expiry %>% paste0("_CE_Expiry_",.,"_",sep='') %>% 
#             grep(.,temp_Symbol_raw_csv_CE_colnames) -> temp_CE_temp_Expiry_columns
#           
#           which(temp_Symbol_raw_csv_CE_colnames == paste0("CE_Expiry_",i_CE_expiry,sep='')) -> temp_CE_expiry_date
#           
#           temp_Symbol_raw_csv_CE[,c(1,temp_CE_expiry_date,temp_CE_temp_Expiry_columns)] -> temp_CE_temp_Expiry_sheet
#           
#           addWorksheet(temp_Symbol_Output_excel, sheet = temp_CE_temp_expiry_sheetname)
#           writeData(temp_Symbol_Output_excel, sheet = temp_CE_temp_expiry_sheetname,
#                     x = temp_CE_temp_Expiry_sheet)
#         } # End of 'for(i_CE_expiry in 1:expiry_counter)'
#         
#         print(paste0("Making 'PE ATM' of ", temp_Symbol,sep=''))
#         
#         # Constructing PE ATM
#         Symbol_PE_ATM_Sheetname <- paste0(temp_Symbol,"_PE_ATM",sep='')
#         Symbol_PE_ATM_Sheetname %>% gsub("&","_and_",.) -> Symbol_PE_ATM_Sheetname
#         temp_Symbol_Output_excel_sheetnames <- c(temp_Symbol_Output_excel_sheetnames,Symbol_PE_ATM_Sheetname)
#         addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_PE_ATM_Sheetname)
#         grep("_ATM",temp_Symbol_raw_csv_PE_colnames) %>% max %>% {.+1} -> temp_Symbol_raw_csv_PE_max_ATM_column
#         
#         writeData(temp_Symbol_Output_excel, sheet = Symbol_PE_ATM_Sheetname,
#                   x = temp_Symbol_raw_csv_PE[,(1:temp_Symbol_raw_csv_PE_max_ATM_column)])
#         
#         print(paste0("Making 'PE Expiries' of ", temp_Symbol,sep=''))
#         
#         # Constructing PE Expiries
#         # i_PE_expiry <- 1
#         for(i_PE_expiry in 1:expiry_counter)
#         {
#           paste0(temp_Symbol,"_PE_Expiry_",i_PE_expiry,sep="") -> temp_PE_temp_expiry_sheetname
#           temp_PE_temp_expiry_sheetname %>% gsub("&","_and_",.) -> temp_PE_temp_expiry_sheetname
#           temp_Symbol_Output_excel_sheetnames <- c(temp_Symbol_Output_excel_sheetnames,temp_PE_temp_expiry_sheetname)
#           
#           i_PE_expiry %>% paste0("_PE_Expiry_",.,"_",sep='') %>% 
#             grep(.,temp_Symbol_raw_csv_PE_colnames) -> temp_PE_temp_Expiry_columns
#           
#           which(temp_Symbol_raw_csv_PE_colnames == paste0("PE_Expiry_",i_PE_expiry,sep='')) -> temp_PE_expiry_date
#           
#           temp_Symbol_raw_csv_PE[,c(1,temp_PE_expiry_date,temp_PE_temp_Expiry_columns)] -> temp_PE_temp_Expiry_sheet
#           
#           addWorksheet(temp_Symbol_Output_excel, sheet = temp_PE_temp_expiry_sheetname)
#           writeData(temp_Symbol_Output_excel, sheet = temp_PE_temp_expiry_sheetname,
#                     x = temp_PE_temp_Expiry_sheet)
#         } # End of 'for(i_PE_expiry in 1:expiry_counter)'
#         ############################
#         # Getting EQ and FnO data for all relevant expiries on a sheet
#         # Finding Min and max date from the 'temp_Symbol_raw_csv'
#         # "TOTTRDVAL_SPOT_C"
#         
#         # Find min & max date from temp_Symbol_raw_csv
#         
#         temp_Symbol_raw_csv %>% select(Date_YYYYMMDD_C) %>% unlist %>% as.character %>% 
#           as.Date(format="%Y%m%d") -> All_temp_Symbol_raw_csv_dates
#         
#         All_temp_Symbol_raw_csv_dates %>% as.Date(format="%Y-%m-%d") %>% 
#           format("%Y%m%d") %>% as.numeric %>% max -> All_temp_Symbol_raw_csv_dates_Max
#         
#         All_temp_Symbol_raw_csv_dates %>% as.Date(format="%Y-%m-%d") %>% 
#           format("%Y%m%d") %>% as.numeric %>% min -> All_temp_Symbol_raw_csv_dates_Min
#         
#         temp_Spot_Directory <- c()
#         
#         if (temp_Symbol == "BANKNIFTY")
#         {
#           temp_Spot_Directory <- BankNifty_Database
#         }else{
#           temp_Spot_Directory <- Nifty50_Database
#         } # End of 'if (temp_Symbol == "BANKNIFTY")'
#         
#         temp_Spot_Directory %>% setwd
#         
#         list.files() -> temp_Spot_Directory_files
#         
#         temp_Spot_Directory_files %>% strsplit(split=" ") %>% lapply(., function(YY){YY[1]}) %>% unlist %>% 
#           as.numeric %>% sort %>% length %>% temp_Spot_Directory_files[.] %>% read.csv -> temp_Spot_Latest
#         
#         temp_Spot_Latest$Date %>% unlist %>% as.character %>% as.Date(format="%b %d, %Y") %>% 
#           format("%Y%m%d") %>% as.numeric -> temp_Spot_Latest$Date
#         
#         temp_Spot_Latest %>% select(Date) %>% unlist %>% as.character %>% as.numeric %>% 
#           {. <= All_temp_Symbol_raw_csv_dates_Max & . >= All_temp_Symbol_raw_csv_dates_Min} %>% 
#           temp_Spot_Latest[.,-c(ncol(temp_Spot_Latest))] %>% 
#           `colnames<-`(c("TIMESTAMP","CLOSE","OPEN","HIGH","LOW","TOTTRDVAL")) -> temp_Spot_Latest_relevant
#         
#         temp_Spot_Latest_relevant %>% select(TOTTRDVAL) %>% unlist %>% as.character %>% 
#           gsub("-","0",.) %>% gsub("K","*1000",.) %>% gsub("M","*1000000",.) %>% 
#           gsub("B","*1000000000",.) %>% gsub("T","*1000000000000",.) %>% as.list(.) %>% 
#           lapply(.,function(MM){eval(parse(text=MM))}) %>% unlist -> temp_Spot_Latest_relevant$TOTTRDVAL
#         
#         temp_Spot_Latest_relevant$SYMBOL <- temp_Symbol
#         
#         temp_Spot_Latest_relevant$SERIES <- ""
#         
#         temp_Spot_Latest_relevant$PREVCLOSE <- ""
#         
#         temp_Spot_Latest_relevant$TOTTRDQTY <- ""
#         
#         temp_Spot_Latest_relevant[,c(7,8,3,4,2,5,9,10,6,1)] -> temp_Spot_Latest_relevant
#         
#         temp_Spot_Latest_relevant[with(temp_Spot_Latest_relevant, order(TIMESTAMP)), ] -> temp_Spot_Latest_relevant
#         
#         if(nrow(temp_Spot_Latest_relevant) > 0)
#         {
#           temp_Symbol_relevant_EQ_sheetname <- paste0(temp_Symbol,"_EQ",sep='')
#           temp_Symbol_relevant_EQ_sheetname %>% gsub("&","_and_",.) -> temp_Symbol_relevant_EQ_sheetname
#           
#           temp_Symbol_Output_excel_sheetnames <- c(temp_Symbol_Output_excel_sheetnames,temp_Symbol_relevant_EQ_sheetname)
#           
#           print(paste0("For Symbol ", temp_Symbol,", making new sheet: ",temp_Symbol_relevant_EQ_sheetname,sep=''))
#           
#           addWorksheet(temp_Symbol_Output_excel, sheet = temp_Symbol_relevant_EQ_sheetname)
#           writeData(temp_Symbol_Output_excel, sheet = temp_Symbol_relevant_EQ_sheetname,
#                     x = temp_Spot_Latest_relevant)
#         } # End of 'if(nrow(temp_Spot_Latest_relevant) > 0)'
#         ###################
#         
#         ###################
#         
#         # Finding relevant FnO separately and recoding it in a df, 
#         # using All_Fno_relevant_dates, All_FnO_relevant_dates_Max & All_FnO_relevant_dates_Min
#         
#         temp_Symbol_raw_csv %>% select(Date_YYYYMMDD_C) %>% unlist %>% as.character %>% 
#           as.Date(format="%Y%m%d") -> All_FnO_relevant_dates
#         
#         temp_FnO_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame(stringsAsFactors = F) %>% 
#           `colnames<-`(temp_FnO_colnames) -> temp_Symbol_relevant_F
#         
#         temp_FnO_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame(stringsAsFactors = F) %>% 
#           `colnames<-`(temp_FnO_colnames) -> temp_Symbol_relevant_O
#         
#         FnO_Bhav_Database %>% setwd
#         
#         if(length(All_FnO_relevant_dates) > 0)
#         {
#           FnO_Bhav_Database %>% setwd
#           
#           list.files() -> All_FnO_files
#           
#           All_FnO_relevant_dates %>% min -> All_FnO_relevant_dates_Min
#           
#           All_FnO_relevant_dates %>% max -> All_FnO_relevant_dates_Max
#           
#           All_FnO_files %>% as.Date(format="fo%d%b%Ybhav.csv") %>% {. <= All_FnO_relevant_dates_Max} %>% 
#             All_FnO_files[.] %>% as.Date(format="fo%d%b%Ybhav.csv") %>% {. >= All_FnO_relevant_dates_Min} %>% 
#             All_FnO_files[.] %>% unique -> All_relevant_FnO_files
#           
#           # Sorting All_relevant_FnO_files by date
#           All_relevant_FnO_files %>% as.Date(format="fo%d%b%Ybhav.csv") -> All_relevant_FnO_dates
#           
#           (1:length(All_relevant_FnO_dates)) -> names(All_relevant_FnO_dates)
#           
#           All_relevant_FnO_dates %>% sort %>% names(.) %>% as.numeric %>% 
#             All_relevant_FnO_files[.] %>% unlist %>% as.character -> All_relevant_FnO_files
#           
#           if (length(All_relevant_FnO_files) > 0)
#           {
#             # i_All_relevant_FnO_files <- 1
#             for(i_All_relevant_FnO_files in 1:length(All_relevant_FnO_files))
#             {
#               print(paste0("For Symbol ", temp_Symbol,", reading ",i_All_relevant_FnO_files,
#                            " out of ", length(All_relevant_FnO_files), " relevant FnO files.",sep=''))
#               
#               FnO_Bhav_Database %>% setwd
#               
#               i_All_relevant_FnO_files %>% All_relevant_FnO_files[.] %>% read.csv -> temp_FnO_csv
#               
#               which(colnames(temp_FnO_csv) == "TIMESTAMP") %>% {1:.} %>% temp_FnO_csv[,.] %>% 
#                 `colnames<-`(temp_FnO_colnames)-> temp_FnO_csv
#               
#               temp_FnO_csv %>% filter(INSTRUMENT == "OPTIDX") %>% filter(SYMBOL == temp_Symbol) %>% 
#                 filter(SYMBOL == temp_Symbol) -> temp_relevant_O_df
#               
#               temp_FnO_csv %>% filter(INSTRUMENT == "FUTIDX") %>% filter(SYMBOL == temp_Symbol) %>% 
#                 filter(SYMBOL == temp_Symbol) -> temp_relevant_F_df
#               
#               if(nrow(temp_relevant_F_df) > 0 & nrow(temp_relevant_O_df) > 0)
#               {
#                 temp_relevant_F_df %>% rbind(temp_Symbol_relevant_F,.) -> temp_Symbol_relevant_F
#                 
#                 temp_relevant_O_df %>% rbind(temp_Symbol_relevant_O,.) -> temp_Symbol_relevant_O
#               } # End of 'if(nrow(temp_relevant_F_df) > 0 & nrow(temp_relevant_O_df) > 0)'
#             } # End of 'forfor(i_All_relevant_FnO_files in 1:length(All_relevant_FnO_files))'
#           } # End of 'if (length(All_relevant_FnO_files) > 0)'
#         } # End of 'if(length(All_FnO_relevant_dates) > 0)'
#         ###################
#         
#         ################### Adding temp_Symbol_relevant_F to a sheet
#         
#         temp_Symbol_relevant_F %>% select(TIMESTAMP) %>% unlist %>% 
#           as.character -> temp_Symbol_relevant_F$TIMESTAMP
#         
#         temp_Symbol_relevant_F %>% select(EXPIRY_DT) %>% unlist %>% 
#           as.character -> temp_Symbol_relevant_F$EXPIRY_DT
#         
#         temp_Symbol_relevant_F %>% select(TIMESTAMP) %>% unlist %>% as.character %>% 
#           nchar %>% unique -> temp_Symbol_relevant_F_nchar_unique
#         
#         if(length(temp_Symbol_relevant_F_nchar_unique) > 1)
#         {
#           which( (temp_Symbol_relevant_F %>% select(TIMESTAMP) %>% unlist %>% as.character %>% 
#                     nchar) < 10) -> wrong_date_rows
#           
#           if ( length(wrong_date_rows) > 0)
#           {
#             wrong_date_rows %>% temp_Symbol_relevant_F[.,] %>% select(TIMESTAMP) %>% 
#               unlist %>% as.character %>% as.Date(format="%d-%b-%y") %>% 
#               format("%d-%b-%Y") -> temp_Symbol_relevant_F[wrong_date_rows,"TIMESTAMP"]
#             
#             wrong_date_rows %>% temp_Symbol_relevant_F[.,] %>% select(EXPIRY_DT) %>% 
#               unlist %>% as.character %>% as.Date(format="%d-%b-%y") %>% 
#               format("%d-%b-%Y") -> temp_Symbol_relevant_F[wrong_date_rows,"EXPIRY_DT"]
#           } # End of 'if ( length(wrong_date_rows) > 0)'
#           
#         } # End of 'if(length(temp_Symbol_relevant_F_nchar_unique) > 0
#         
#         temp_Symbol_relevant_F %>% select(TIMESTAMP) %>% unlist %>% as.character %>% 
#           as.Date(format="%d-%b-%Y") %>% format("%Y%m%d") %>% as.numeric -> temp_Symbol_relevant_F$TIMESTAMP
#         
#         temp_Symbol_relevant_F %>% select(EXPIRY_DT) %>% unlist %>% as.character %>% 
#           as.Date(format="%d-%b-%Y") %>% format("%Y%m%d") %>% as.numeric -> temp_Symbol_relevant_F$EXPIRY_DT
#         
#         temp_Symbol_relevant_F[with(temp_Symbol_relevant_F, order(EXPIRY_DT,TIMESTAMP)), ] -> temp_Symbol_relevant_F
#         
#         if(nrow(temp_Symbol_relevant_F) > 0)
#         {
#           temp_Symbol_relevant_F[with(temp_Symbol_relevant_F, 
#                                       order(TIMESTAMP,EXPIRY_DT,OPTIONTYPE,STRIKE_PR)), ] -> temp_Symbol_relevant_F
#           
#           temp_Symbol_relevant_F_sheetname <- paste0(temp_Symbol,"_F",sep='')
#           temp_Symbol_relevant_F_sheetname %>% gsub("&","_and_",.) -> temp_Symbol_relevant_F_sheetname
#           
#           temp_Symbol_Output_excel_sheetnames <- c(temp_Symbol_Output_excel_sheetnames,temp_Symbol_relevant_F_sheetname)
#           
#           print(paste0("For Symbol ", temp_Symbol,", making new sheet: ",temp_Symbol_relevant_F_sheetname,sep=''))
#           
#           addWorksheet(temp_Symbol_Output_excel, sheet = temp_Symbol_relevant_F_sheetname)
#           writeData(temp_Symbol_Output_excel, sheet = temp_Symbol_relevant_F_sheetname,
#                     x = temp_Symbol_relevant_F)
#         } # End of 'if(nrow(temp_Symbol_relevant_F) > 0)'
#         ###################
#         
#         ################### Adding temp_Symbol_relevant_O to a sheet
#         # temp_Symbol_relevant_O_backup <- temp_Symbol_relevant_O
#         temp_Symbol_relevant_O %>% select(TIMESTAMP) %>% unlist %>% 
#           as.character -> temp_Symbol_relevant_O$TIMESTAMP
#         
#         temp_Symbol_relevant_O %>% select(EXPIRY_DT) %>% unlist %>% 
#           as.character -> temp_Symbol_relevant_O$EXPIRY_DT
#         
#         temp_Symbol_relevant_O %>% select(TIMESTAMP) %>% unlist %>% as.character %>% 
#           nchar %>% unique -> temp_Symbol_relevant_O_nchar_unique
#         
#         if(length(temp_Symbol_relevant_O_nchar_unique) > 1)
#         {
#           which( (temp_Symbol_relevant_O %>% select(TIMESTAMP) %>% unlist %>% as.character %>% 
#                     nchar) < 10) -> wrong_date_rows
#           
#           if ( length(wrong_date_rows) > 0)
#           {
#             wrong_date_rows %>% temp_Symbol_relevant_O[.,] %>% select(TIMESTAMP) %>% 
#               unlist %>% as.character %>% as.Date(format="%d-%b-%y") %>% 
#               format("%d-%b-%Y") %>% as.character -> temp_Symbol_relevant_O[wrong_date_rows,"TIMESTAMP"]
#             
#             wrong_date_rows %>% temp_Symbol_relevant_O[.,] %>% select(EXPIRY_DT) %>% 
#               unlist %>% as.character %>% as.Date(format="%d-%b-%y") %>% 
#               format("%d-%b-%Y") %>% as.character -> temp_Symbol_relevant_O[wrong_date_rows,"EXPIRY_DT"]
#           } # End of 'if ( length(wrong_date_rows) > 0)'
#           
#         } # End of 'if(length(temp_Symbol_relevant_O_nchar_unique) > 0
#         
#         temp_Symbol_relevant_O %>% select(TIMESTAMP) %>% unlist %>% as.character %>% 
#           as.Date(format="%d-%b-%Y") %>% format("%Y%m%d") %>% as.numeric -> temp_Symbol_relevant_O$TIMESTAMP
#         
#         temp_Symbol_relevant_O %>% select(EXPIRY_DT) %>% unlist %>% as.character %>% 
#           as.Date(format="%d-%b-%Y") %>% format("%Y%m%d") %>% as.numeric -> temp_Symbol_relevant_O$EXPIRY_DT
#         
#         temp_Symbol_relevant_O[with(temp_Symbol_relevant_O, order(EXPIRY_DT,STRIKE_PR,TIMESTAMP)), ] -> temp_Symbol_relevant_O
#         
#         temp_Symbol_relevant_O[temp_Symbol_relevant_O$OPTIONTYPE == "CE",] -> temp_Symbol_relevant_O_CE
#         temp_Symbol_relevant_O[temp_Symbol_relevant_O$OPTIONTYPE == "PE",] -> temp_Symbol_relevant_O_PE
#         
#         if(nrow(temp_Symbol_relevant_O_CE) > 0)
#         {
#           temp_Symbol_relevant_O_CE[with(temp_Symbol_relevant_O_CE, 
#                                          order(TIMESTAMP,EXPIRY_DT,OPTIONTYPE,STRIKE_PR)), ] -> temp_Symbol_relevant_O_CE
#           
#           temp_Symbol_relevant_O_CE_sheetname <- paste0(temp_Symbol,"_O_CE",sep='')
#           temp_Symbol_relevant_O_CE_sheetname %>% gsub("&","_and_",.) -> temp_Symbol_relevant_O_CE_sheetname
#           temp_Symbol_Output_excel_sheetnames <- c(temp_Symbol_Output_excel_sheetnames,temp_Symbol_relevant_O_CE_sheetname)
#           
#           print(paste0("For Symbol ", temp_Symbol,", making new sheet: ",temp_Symbol_relevant_O_CE_sheetname,sep=''))
#           
#           addWorksheet(temp_Symbol_Output_excel, sheet = temp_Symbol_relevant_O_CE_sheetname)
#           writeData(temp_Symbol_Output_excel, sheet = temp_Symbol_relevant_O_CE_sheetname,
#                     x = temp_Symbol_relevant_O_CE)
#         } # End of 'if(nrow(temp_Symbol_relevant_O_CE) > 0)'
#         
#         if(nrow(temp_Symbol_relevant_O_PE) > 0)
#         {
#           temp_Symbol_relevant_O_PE[with(temp_Symbol_relevant_O_PE, 
#                                          order(TIMESTAMP,EXPIRY_DT,OPTIONTYPE,STRIKE_PR)), ] -> temp_Symbol_relevant_O_PE
#           
#           temp_Symbol_relevant_O_PE_sheetname <- paste0(temp_Symbol,"_O_PE",sep='')
#           temp_Symbol_relevant_O_PE_sheetname %>% gsub("&","_and_",.) -> temp_Symbol_relevant_O_PE_sheetname
#           temp_Symbol_Output_excel_sheetnames <- c(temp_Symbol_Output_excel_sheetnames,temp_Symbol_relevant_O_PE_sheetname)
#           
#           print(paste0("For Symbol ", temp_Symbol,", making new sheet: ",temp_Symbol_relevant_O_PE_sheetname,sep=''))
#           
#           addWorksheet(temp_Symbol_Output_excel, sheet = temp_Symbol_relevant_O_PE_sheetname)
#           writeData(temp_Symbol_Output_excel, sheet = temp_Symbol_relevant_O_PE_sheetname,
#                     x = temp_Symbol_relevant_O_PE)
#         } # End of 'if(nrow(temp_Symbol_relevant_O_PE) > 0)'
#         ###################
#         
#       } # End of 'if (Found_Expiry_limit)'
#       
#     } # End of 'if(Old_Excel_Exists)'
#     
#     # Saving the file, only when required
#     
#     if(!Old_Excel_Exists)
#     {
#       OPTIDX_Excel_TS_Database %>% setwd
#       paste0("Making new file: ",temp_Options_excel_filename, sep='')
#       saveWorkbook(temp_Symbol_Output_excel, temp_Options_excel_filename)
#     }else{
#       if(Overwrite_Required)
#       {
#         OPTIDX_Excel_TS_Database %>% setwd
#         "For Symbol: '" %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#           paste0("' overwriting! Deleting old file: '",temp_Options_excel_filename,"'",sep='') %>% print
#         
#         # file.remove(temp_Options_excel_filename) # ', overwrite = T' argument is a better approach
#         
#         "For Symbol: '" %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#           paste0("' saving with new data, new filename: '",temp_Options_excel_filename,"'",sep='') %>% print
#         
#         saveWorkbook(temp_Symbol_Output_excel, temp_Options_excel_filename, overwrite = T)
#       }else{
#         "For Symbol: '" %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#           paste0("', Old file: '",temp_Options_excel_filename,"' was up to date",sep='') %>% print
#       } # End of 'if(Overwrite_Required)'
#     } # End of 'if(!Old_Excel_Exists)'
#   } # End of 'for (i_1 in 1: length(OPTIDX_TS_Database_Files))'
# }else{
#   print("'OPTIDX_TS_Database' folder is empty")
# } # End of 'if (length(OPTIDX_TS_Database_Files) > 0)'
# 
# #################################################################################
# #################################################################################
# 
# #################################################################################
# #################################### Goals ######################################
# 
# # Date: 2020-June-08
# # Author: Arunabha Sarkar
# 
# # Goals: Using files in 'OPTSTK_TS', make excel file for drag and drop Option Strategy
# # File Name: OPTSTK_Excel_TS
# 
# #################################################################################
# #################################################################################
# 
# #################################################################################
# ##################### Initializing and loading Libraries ########################
# 
# library(dplyr)
# library(purrr)
# library(openxlsx)
# 
# #################################################################################
# #################################################################################
# 
# #################################################################################
# ############################### Set Directories #################################
# 
# # "C:/Users/aurnabha/Desktop/Central Database" -> Central_Database
# 
# Central_Database %>% paste0("/Nifty 50 Historical Data Investing dot com",
#                             sep='') -> Nifty50_Database  # To find out which day is open & NIFTY Spot
# 
# Central_Database %>% paste0("/Latest_NSE_Index_Composition",
#                             sep='') -> Latest_NSE_Index_Composition_Database
# 
# Central_Database %>% paste0("/NSE EQ Bhav Copies",sep='') -> EQ_Bhav_Database # To add spot value
# 
# Central_Database %>% paste0("/NSE FnO Bhav Copies",sep='') -> FnO_Bhav_Database # To get the raw data
# 
# Central_Database %>% paste0("/OPTSTK_TS",sep='') -> OPTSTK_TS_Database
# 
# Central_Database %>% setwd
# 
# if(!file.exists('OPTSTK_Excel_TS'))
# {
#   print("Creating 'OPTSTK_Excel_TS' directory.")
#   dir.create(file.path(Central_Database, 'OPTSTK_Excel_TS'))
# }else{
#   print("'OPTSTK_Excel_TS' directory already exists.")
# } # End of 'if(!file.exists('OPTSTK_Excel_TS'))'
# 
# Central_Database %>% paste0("/OPTSTK_Excel_TS",sep='') -> OPTSTK_Excel_TS_Database
# 
# #################################################################################
# #################################################################################
# 
# #################################################################################
# ############################## Hyper parameters #################################
# 
# MAX_Expiry_Trials <- 20
# 
# MAX_Strike_p_Trials <- 40
# 
# temp_FnO_colnames <- c("INSTRUMENT","SYMBOL","EXPIRY_DT","STRIKE_PR","OPTIONTYPE",
#                        "OPEN","HIGH","LOW","CLOSE","SETTLE_PR","CONTRACTS","VAL_INLAKH",
#                        "OPEN_INT","CHG_IN_OI","TIMESTAMP")
# 
# temp_EQ_colnames <- c("SYMBOL","SERIES","OPEN","HIGH","LOW","CLOSE","LAST","PREVCLOSE",
#                       "TOTTRDQTY","TOTTRDVAL","TIMESTAMP")
# 
# #################################################################################
# #################################################################################
# 
# #################################################################################
# ############################# Finding Latest Date ###############################
# 
# # Find latest date of NIFTY50
# Nifty50_Database %>% setwd
# 
# list.files() %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>% 
#   as.Date(format="%Y%m%d") %>% which.max %>% list.files()[.] %>% 
#   read.csv %>% {. ->> Nifty_Data} %>% select(Date) %>% unlist %>% as.character %>% 
#   as.Date(format="%b %d, %Y") %>% max %>% format("%Y%m%d") %>% as.numeric -> Max_Nifty_Date
# 
# colnames(Nifty_Data) <- c("Date","Last","Open","High","Low","Volume","Change")
# 
# Nifty_Data %>% select(Date) %>% unlist %>% as.character %>% as.Date(format("%b %d, %Y")) %>% 
#   format("%Y%m%d") %>% as.numeric -> Nifty_Data$Date
# 
# Nifty_Data$Date %>% unlist %>% as.numeric %>% sort -> N50_Dates_YYYYMMDD
# 
# #################################################################################
# #################################################################################
# 
# #################################################################################
# ########################## Making OPTSTK_Excel files ############################
# 
# #### Arranging 'Latest_File_Symbols' with Nifty50 first
# 
# Latest_NSE_Index_Composition_Database %>% setwd
# 
# list.files() -> Latest_NSE_Index_Composition_Database_Files
# 
# "Nifty50.csv" %>% grep(Latest_NSE_Index_Composition_Database_Files) %>% 
#   Latest_NSE_Index_Composition_Database_Files[.] -> All_Nifty_50_candidates
# 
# All_Nifty_50_candidates %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>%
#   as.Date(format="%Y%m%d") %>% which.max %>% All_Nifty_50_candidates[.] %>% 
#   read.csv %>% select(Symbol) %>% unlist %>% as.character -> Latest_Nifty_50_tickers
# 
# #######################################################
# 
# OPTSTK_TS_Database %>% setwd
# list.files() -> OPTSTK_TS_Database_Files
# 
# Latest_Nifty_50_tickers %>% paste0(.,"_Options.csv",sep="") %>% 
#   c(.,OPTSTK_TS_Database_Files) %>% unique -> OPTSTK_TS_Database_Files
# 
# if (length(OPTSTK_TS_Database_Files) > 0)
# {
#   print(paste0("Found '", length(OPTSTK_TS_Database_Files), "' files in 'OPTSTK_TS_Database'",sep=''))
#   
#   # i_1 <- 1
#   for (i_1 in 1: length(OPTSTK_TS_Database_Files))
#   {
#     OPTSTK_TS_Database %>% setwd
#     
#     # Need top tackle existing file and new file case
#     Old_Excel_Exists <- FALSE
#     Overwrite_Required <- FALSE
#     
#     i_1 %>% OPTSTK_TS_Database_Files[.] -> temp_Options_csv_filename
#     
#     temp_Options_csv_filename %>% strsplit(split='_') %>% .[[1]] %>% .[1] -> temp_Symbol
#     
#     # Checking if previous excel output exists
#     OPTSTK_Excel_TS_Database %>% setwd
#     
#     temp_Symbol %>% paste0(.,"_Excel_TS.xlsx",sep='') -> temp_Options_excel_filename
#     OPTSTK_Excel_TS_Database %>% setwd
#     list.files() -> OPTSTK_Excel_TS_Database_Files
#     
#     if(temp_Options_excel_filename %in% OPTSTK_Excel_TS_Database_Files)
#     {
#       Old_Excel_Exists <- TRUE
#       print(paste0("Prior Excel file found for ", temp_Symbol,sep=''))
#     }else{
#       Old_Excel_Exists <- FALSE
#       print(paste0("NO prior Excel file for ", temp_Symbol,sep=''))
#     } # End of 'if(temp_Options_excel_filename %in% OPTSTK_Excel_TS_Database_Files)'
#     
#     if(Old_Excel_Exists)
#     {
#       # Find relevant new rows & Option Chain
#       temp_Options_excel_filename %>% strsplit(split="_") %>% .[[1]] %>% .[1] -> temp_Symbol_from_filename
#       
#       temp_Symbol_from_filename %>% gsub("&","_and_",x=.) -> temp_Symbol_in_sheet_names
#       
#       # Finding latest date in sheet and comparing to 'Max_Nifty_Date'
#       OPTSTK_Excel_TS_Database %>% setwd
#       
#       temp_Options_excel_filename %>% read.xlsx(sheet = 1) %>% select(Date_YYYYMMDD_C) %>% unlist %>% 
#         max -> temp_old_file_max_date
#       
#       # Need latest date from from csv also
#       OPTSTK_TS_Database %>% setwd
#       temp_Symbol_from_filename %>% paste0(.,"_Options.csv",sep='') %>% read.csv %>% 
#         select(Date_YYYYMMDD_C) %>% unlist %>% as.numeric %>% max -> Max_temp_csv_date
#       
#       if (temp_old_file_max_date >= Max_temp_csv_date)
#       {
#         # "Up to date Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#         #   paste0(", ", i_1, " out of ", length(OPTSTK_TS_Database_Files),sep='') %>% print
#         
#         # Up to date, no update required
#         Old_Excel_Exists <- TRUE
#         Overwrite_Required <- FALSE
#       }else{
#         # Not up to date, update is required
#         Old_Excel_Exists <- TRUE
#         Overwrite_Required <- TRUE
#         
#         # Finding number of expiries from 'OPTSTK_TS_Database' database
#         OPTSTK_TS_Database %>% setwd
#         
#         temp_Symbol_from_filename %>% paste0(.,"_Options.csv",sep='') %>% read.csv %T>% 
#           {. ->> temp_Symbol_raw_csv} %>% colnames -> temp_Symbol_raw_csv_colnames
#         
#         expiry_counter <- MAX_Expiry_Trials
#         Found_Expiry_limit <- FALSE
#         while (!Found_Expiry_limit)
#         {
#           expiry_trial <- grep(paste0("_Expiry_",expiry_counter,"_",sep=''),temp_Symbol_raw_csv_colnames)
#           if (length(expiry_trial) > 0 & expiry_counter > 0)
#           {
#             Found_Expiry_limit <- TRUE
#           } # End of 'if (length(expiry_trial) > 0)'
#           
#           if (Found_Expiry_limit)
#           {
#             break
#           } # End of 'if (Found_Expiry_limit)'
#           expiry_counter <- expiry_counter - 1
#           
#           if (expiry_counter < 0)
#           {
#             print(paste0("Check source csv file: '", temp_Symbol_from_filename, "_Options.csv",
#                          "', no expiry column found",sep=""))
#             break
#           } # End of 'if (expiry_counter < 0)'
#         } # End of 'while (!Found_Expiry_limit)'
#         
#         if (expiry_counter > 0 & Found_Expiry_limit)
#         {
#           "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#             paste0(" found ", expiry_counter, " expiries",sep='') %>% print
#           
#           # Reading old csv file
#           OPTSTK_TS_Database %>% setwd
#           temp_Options_csv_filename %>% read.csv -> temp_Symbol_raw_csv
#           
#           temp_Symbol_raw_csv$Date_YYYYMMDD_C %>% unlist %>% as.character %>% 
#             as.numeric -> temp_Symbol_raw_csv_Dates
#           
#           temp_Symbol_raw_csv_Dates %>% max -> temp_Symbol_raw_csv_Dates_Max
#           
#           # From 'expiry_counter', finding out sheet number of ATMs, expiries & other sheets
#           1 -> temp_sheet_num_CE_ATM
#           temp_sheet_num_CE_ATM %>% {(.+1):(.+expiry_counter)} -> temp_sheet_num_CE_expiries
#           temp_sheet_num_CE_expiries %>% max %>% {.+1} -> temp_sheet_num_PE_ATM
#           temp_sheet_num_PE_ATM %>% {(.+1):(.+expiry_counter)} -> temp_sheet_num_PE_expiries
#           temp_sheet_num_PE_expiries %>% max %>% {.+1} -> temp_sheet_num_EQ
#           temp_sheet_num_EQ %>% {.+1} -> temp_sheet_num_F
#           temp_sheet_num_F %>% {.+1} -> temp_sheet_num_O_CE
#           temp_sheet_num_O_CE %>% {.+1} -> temp_sheet_num_O_PE
#           
#           # Constructing output Excel Sheet:
#           temp_Symbol_Output_excel <- createWorkbook()
#           
#           ################## Tackling 'temp_sheet_num_CE_ATM'
#           "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#             paste0(" updating CE ATM",sep='') %>% print
#           
#           Symbol_CE_ATM_Sheetname <- paste0(temp_Symbol_in_sheet_names,"_CE_ATM",sep='')
#           addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_CE_ATM_Sheetname)
#           
#           OPTSTK_Excel_TS_Database %>% setwd
#           
#           temp_Options_excel_filename %>% read.xlsx(sheet = temp_sheet_num_CE_ATM) -> temp_Old_CE_ATM_sheet
#           
#           temp_Old_CE_ATM_sheet[,1] %>% unlist %>% as.numeric -> temp_Old_CE_ATM_sheet_dates
#           temp_Old_CE_ATM_sheet_dates %>% min -> temp_Old_CE_ATM_sheet_dates_min
#           
#           which(N50_Dates_YYYYMMDD >= temp_Old_CE_ATM_sheet_dates_min) %>% 
#             N50_Dates_YYYYMMDD[.] -> temp_relevant_N50_Dates_YYYYMMDD
#           
#           temp_relevant_N50_Dates_YYYYMMDD %>% {. %in% temp_Old_CE_ATM_sheet_dates} %>% {!.} %>% 
#             temp_relevant_N50_Dates_YYYYMMDD[.] -> temp_missing_dates_temp_sheet_num_CE_ATM
#           
#           which(temp_missing_dates_temp_sheet_num_CE_ATM <= temp_Symbol_raw_csv_Dates_Max) %>% 
#             temp_missing_dates_temp_sheet_num_CE_ATM[.] -> temp_missing_dates_temp_sheet_num_CE_ATM
#           
#           temp_Old_CE_ATM_sheet %>% colnames -> temp_correct_colnames
#           
#           temp_correct_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
#             `colnames<-`(temp_correct_colnames) -> temp_new_CE_ATM
#           
#           if (length(temp_missing_dates_temp_sheet_num_CE_ATM) > 0)
#           {
#             temp_correct_colnames <- colnames(temp_Old_CE_ATM_sheet)
#             
#             # i_temp_missing_dates_temp_sheet_num_CE_ATM <- 1
#             for (i_temp_missing_dates_temp_sheet_num_CE_ATM in 1:length(temp_missing_dates_temp_sheet_num_CE_ATM)) 
#             {
#               Overwrite_Required <- TRUE
#               
#               temp_correct_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
#                 `colnames<-`(temp_correct_colnames) -> temp_temp_new_CE_ATM
#               
#               i_temp_missing_dates_temp_sheet_num_CE_ATM %>% 
#                 temp_missing_dates_temp_sheet_num_CE_ATM[.] %>% 
#                 {which(temp_Symbol_raw_csv$Date_YYYYMMDD_C == .)} %>% 
#                 temp_Symbol_raw_csv[.,temp_correct_colnames] -> temp_temp_new_CE_ATM
#               
#               if(!is.na(temp_temp_new_CE_ATM[1,1]))
#               {
#                 temp_temp_new_CE_ATM %>% rbind(temp_new_CE_ATM,.) -> temp_new_CE_ATM
#               } # End of 'if(!is.na(temp_temp_new_CE_ATM[1,1]))'
#             } # End of 'for (i_temp_missing_dates_temp_sheet_num_CE_ATM in 1:length(temp_missing_dates_temp_sheet_num_CE_ATM))'
#           } # End of 'if (length(temp_missing_dates_temp_sheet_num_CE_ATM) > 0)'
#           
#           temp_new_CE_ATM %>% rbind(temp_Old_CE_ATM_sheet,.) -> temp_new_CE_ATM
#           
#           writeData(temp_Symbol_Output_excel, sheet = Symbol_CE_ATM_Sheetname, x = temp_new_CE_ATM)
#           ################## 
#           
#           ################## Tackling 'temp_sheet_num_CE_expiries'
#           
#           # i_temp_sheet_num_CE_expiries <- 1
#           for (i_temp_sheet_num_CE_expiries in 1:length(temp_sheet_num_CE_expiries))
#           {
#             "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#               paste0(" updating CE Expiry #", i_temp_sheet_num_CE_expiries,
#                      " of ",length(temp_sheet_num_CE_expiries),sep='') %>% print
#             
#             # For each expiry, there is % difference based strike, both plus and minus
#             Symbol_CE_Expiry_Sheetname <- paste0(temp_Symbol_in_sheet_names,"_CE_Expiry_",
#                                                  i_temp_sheet_num_CE_expiries,sep='')
#             
#             addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_CE_Expiry_Sheetname)
#             
#             OPTSTK_Excel_TS_Database %>% setwd
#             
#             temp_Options_excel_filename %>% 
#               read.xlsx(sheet = temp_sheet_num_CE_expiries[i_temp_sheet_num_CE_expiries]) -> temp_Old_CE_Expiry_sheet
#             
#             temp_Old_CE_Expiry_sheet %>% colnames -> temp_correct_colnames
#             
#             temp_Old_CE_Expiry_sheet[,1] %>% unlist %>% as.numeric -> temp_Old_CE_Expiry_sheet_dates
#             temp_Old_CE_Expiry_sheet_dates %>% min -> temp_Old_CE_Expiry_sheet_dates_min
#             
#             which(N50_Dates_YYYYMMDD >= temp_Old_CE_Expiry_sheet_dates_min) %>% 
#               N50_Dates_YYYYMMDD[.] -> temp_relevant_N50_Dates_YYYYMMDD
#             
#             temp_relevant_N50_Dates_YYYYMMDD %>% {. %in% temp_Old_CE_Expiry_sheet_dates} %>% {!.} %>% 
#               temp_relevant_N50_Dates_YYYYMMDD[.] -> temp_missing_dates_temp_sheet_num_CE_Expiry
#             
#             which(temp_missing_dates_temp_sheet_num_CE_Expiry <= temp_Symbol_raw_csv_Dates_Max) %>% 
#               temp_missing_dates_temp_sheet_num_CE_Expiry[.] -> temp_missing_dates_temp_sheet_num_CE_Expiry
#             
#             temp_correct_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
#               `colnames<-`(temp_correct_colnames) -> temp_new_CE_Expiry
#             
#             if (length(temp_missing_dates_temp_sheet_num_CE_Expiry) > 0)
#             {
#               temp_correct_colnames <- colnames(temp_Old_CE_Expiry_sheet)
#               
#               # i_temp_missing_dates_temp_sheet_num_CE_Expiry <- 1
#               for (i_temp_missing_dates_temp_sheet_num_CE_Expiry in 1:length(temp_missing_dates_temp_sheet_num_CE_Expiry)) 
#               {
#                 Overwrite_Required <- TRUE
#                 
#                 temp_correct_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
#                   `colnames<-`(temp_correct_colnames) -> temp_temp_new_CE_Expiry
#                 
#                 i_temp_missing_dates_temp_sheet_num_CE_Expiry %>% 
#                   temp_missing_dates_temp_sheet_num_CE_Expiry[.] %>% 
#                   {which(temp_Symbol_raw_csv$Date_YYYYMMDD_C == .)} %>% 
#                   temp_Symbol_raw_csv[.,temp_correct_colnames] -> temp_temp_new_CE_Expiry
#                 
#                 if(!is.na(temp_temp_new_CE_Expiry[1,1]))
#                 {
#                   temp_temp_new_CE_Expiry %>% rbind(temp_new_CE_Expiry,.) -> temp_new_CE_Expiry
#                 } # End of 'if(!is.na(temp_temp_new_CE_Expiry[1,1]))'
#               } # End of 'for (i_temp_missing_dates_temp_sheet_num_CE_Expiry in 1:length(temp_missing_dates_temp_sheet_num_CE_Expiry)) '
#             } # End of 'if (length(temp_missing_dates_temp_sheet_num_CE_Expiry) > 0)'
#             
#             temp_new_CE_Expiry %>% rbind(temp_Old_CE_Expiry_sheet,.) -> temp_new_CE_Expiry
#             
#             writeData(temp_Symbol_Output_excel, sheet = Symbol_CE_Expiry_Sheetname, x = temp_new_CE_Expiry)
#             
#           }# End of 'for (i_temp_sheet_num_CE_expiries in 1:length(temp_sheet_num_CE_expiries))'
#           ################## 
#           
#           ################## Tackling 'temp_sheet_num_PE_ATM'
#           "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#             paste0(" updating PE ATM",sep='') %>% print
#           
#           Symbol_PE_ATM_Sheetname <- paste0(temp_Symbol_in_sheet_names,"_PE_ATM",sep='')
#           addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_PE_ATM_Sheetname)
#           
#           OPTSTK_Excel_TS_Database %>% setwd
#           
#           temp_Options_excel_filename %>% read.xlsx(sheet = temp_sheet_num_PE_ATM) -> temp_Old_PE_ATM_sheet
#           
#           temp_Old_PE_ATM_sheet[,1] %>% unlist %>% as.numeric -> temp_Old_PE_ATM_sheet_dates
#           temp_Old_PE_ATM_sheet_dates %>% min -> temp_Old_PE_ATM_sheet_dates_min
#           
#           which(N50_Dates_YYYYMMDD >= temp_Old_PE_ATM_sheet_dates_min) %>% 
#             N50_Dates_YYYYMMDD[.] -> temp_relevant_N50_Dates_YYYYMMDD
#           
#           temp_relevant_N50_Dates_YYYYMMDD %>% {. %in% temp_Old_PE_ATM_sheet_dates} %>% {!.} %>% 
#             temp_relevant_N50_Dates_YYYYMMDD[.] -> temp_missing_dates_temp_sheet_num_PE_ATM
#           
#           which(temp_missing_dates_temp_sheet_num_PE_ATM <= temp_Symbol_raw_csv_Dates_Max) %>% 
#             temp_missing_dates_temp_sheet_num_PE_ATM[.] -> temp_missing_dates_temp_sheet_num_PE_ATM
#           
#           temp_Old_PE_ATM_sheet %>% colnames -> temp_correct_colnames
#           
#           temp_correct_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
#             `colnames<-`(temp_correct_colnames) -> temp_new_PE_ATM
#           
#           if (length(temp_missing_dates_temp_sheet_num_PE_ATM) > 0)
#           {
#             temp_correct_colnames <- colnames(temp_Old_PE_ATM_sheet)
#             
#             # i_temp_missing_dates_temp_sheet_num_PE_ATM <- 1
#             for (i_temp_missing_dates_temp_sheet_num_PE_ATM in 1:length(temp_missing_dates_temp_sheet_num_PE_ATM)) 
#             {
#               Overwrite_Required <- TRUE
#               
#               temp_correct_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
#                 `colnames<-`(temp_correct_colnames) -> temp_temp_new_PE_ATM
#               
#               i_temp_missing_dates_temp_sheet_num_PE_ATM %>% 
#                 temp_missing_dates_temp_sheet_num_PE_ATM[.] %>% 
#                 {which(temp_Symbol_raw_csv$Date_YYYYMMDD_C == .)} %>% 
#                 temp_Symbol_raw_csv[.,temp_correct_colnames] -> temp_temp_new_PE_ATM
#               
#               if(!is.na(temp_temp_new_PE_ATM[1,1]))
#               {
#                 temp_temp_new_PE_ATM %>% rbind(temp_new_PE_ATM,.) -> temp_new_PE_ATM
#               } # End of 'if(!is.na(temp_temp_new_PE_ATM[1,1]))'
#             } # End of 'for (i_temp_missing_dates_temp_sheet_num_PE_ATM in 1:length(temp_missing_dates_temp_sheet_num_PE_ATM))'
#           } # End of 'if (length(temp_missing_dates_temp_sheet_num_PE_ATM) > 0)'
#           
#           temp_new_PE_ATM %>% rbind(temp_Old_PE_ATM_sheet,.) -> temp_new_PE_ATM
#           
#           writeData(temp_Symbol_Output_excel, sheet = Symbol_PE_ATM_Sheetname, x = temp_new_PE_ATM)
#           ################## 
#           
#           ################## Tackling 'temp_sheet_num_PE_expiries'
#           
#           # i_temp_sheet_num_PE_expiries <- 1
#           for (i_temp_sheet_num_PE_expiries in 1:length(temp_sheet_num_PE_expiries))
#           {
#             Overwrite_Required <- TRUE
#             
#             "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#               paste0(" updating PE Expiry #", i_temp_sheet_num_PE_expiries,
#                      " of ",length(temp_sheet_num_PE_expiries),sep='') %>% print
#             
#             # For each expiry, there is % difference based strike, both plus and minus
#             Symbol_PE_Expiry_Sheetname <- paste0(temp_Symbol_in_sheet_names,"_PE_Expiry_",
#                                                  i_temp_sheet_num_PE_expiries,sep='')
#             
#             addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_PE_Expiry_Sheetname)
#             
#             OPTSTK_Excel_TS_Database %>% setwd
#             
#             temp_Options_excel_filename %>% 
#               read.xlsx(sheet = temp_sheet_num_PE_expiries[i_temp_sheet_num_PE_expiries]) -> temp_Old_PE_Expiry_sheet
#             
#             temp_Old_PE_Expiry_sheet %>% colnames -> temp_correct_colnames
#             
#             temp_Old_PE_Expiry_sheet[,1] %>% unlist %>% as.numeric -> temp_Old_PE_Expiry_sheet_dates
#             temp_Old_PE_Expiry_sheet_dates %>% min -> temp_Old_PE_Expiry_sheet_dates_min
#             
#             which(N50_Dates_YYYYMMDD >= temp_Old_PE_Expiry_sheet_dates_min) %>% 
#               N50_Dates_YYYYMMDD[.] -> temp_relevant_N50_Dates_YYYYMMDD
#             
#             temp_relevant_N50_Dates_YYYYMMDD %>% {. %in% temp_Old_PE_Expiry_sheet_dates} %>% {!.} %>% 
#               temp_relevant_N50_Dates_YYYYMMDD[.] -> temp_missing_dates_temp_sheet_num_PE_Expiry
#             
#             which(temp_missing_dates_temp_sheet_num_PE_Expiry <= temp_Symbol_raw_csv_Dates_Max) %>% 
#               temp_missing_dates_temp_sheet_num_PE_Expiry[.] -> temp_missing_dates_temp_sheet_num_PE_Expiry
#             
#             temp_correct_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
#               `colnames<-`(temp_correct_colnames) -> temp_new_PE_Expiry
#             
#             if (length(temp_missing_dates_temp_sheet_num_PE_Expiry) > 0)
#             {
#               temp_correct_colnames <- colnames(temp_Old_PE_Expiry_sheet)
#               
#               # i_temp_missing_dates_temp_sheet_num_PE_Expiry <- 1
#               for (i_temp_missing_dates_temp_sheet_num_PE_Expiry in 1:length(temp_missing_dates_temp_sheet_num_PE_Expiry)) 
#               {
#                 Overwrite_Required <- TRUE
#                 
#                 temp_correct_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
#                   `colnames<-`(temp_correct_colnames) -> temp_temp_new_PE_Expiry
#                 
#                 i_temp_missing_dates_temp_sheet_num_PE_Expiry %>% 
#                   temp_missing_dates_temp_sheet_num_PE_Expiry[.] %>% 
#                   {which(temp_Symbol_raw_csv$Date_YYYYMMDD_C == .)} %>% 
#                   temp_Symbol_raw_csv[.,temp_correct_colnames] -> temp_temp_new_PE_Expiry
#                 
#                 if(!is.na(temp_temp_new_PE_Expiry[1,1]))
#                 {
#                   temp_temp_new_PE_Expiry %>% rbind(temp_new_PE_Expiry,.) -> temp_new_PE_Expiry
#                 } # End of 'if(!is.na(temp_temp_new_PE_Expiry[1,1]))'
#               } # End of 'for (i_temp_missing_dates_temp_sheet_num_PE_Expiry in 1:length(temp_missing_dates_temp_sheet_num_PE_Expiry)) '
#             } # End of 'if (length(temp_missing_dates_temp_sheet_num_PE_Expiry) > 0)'
#             
#             temp_new_PE_Expiry %>% rbind(temp_Old_PE_Expiry_sheet,.) -> temp_new_PE_Expiry
#             
#             writeData(temp_Symbol_Output_excel, sheet = Symbol_PE_Expiry_Sheetname, x = temp_new_PE_Expiry)
#             
#           }# End of 'for (i_temp_sheet_num_PE_expiries in 1:length(temp_sheet_num_PE_expiries))'
#           ##################
#           
#           ################## Tackling 'temp_sheet_num_EQ'
#           "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#             paste0(" updating EQ",sep='') %>% print
#           
#           Symbol_EQ_Sheetname <- paste0(temp_Symbol_in_sheet_names,"_EQ",sep='')
#           addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_EQ_Sheetname)
#           
#           OPTSTK_Excel_TS_Database %>% setwd
#           
#           temp_Options_excel_filename %>% read.xlsx(sheet = temp_sheet_num_EQ) -> temp_Old_EQ_sheet
#           
#           temp_Old_EQ_sheet[,which(colnames(temp_Old_EQ_sheet) == "TIMESTAMP")] %>% unlist %>% 
#             as.numeric -> temp_Old_EQ_sheet_dates
#           
#           temp_Old_EQ_sheet_dates %>% min -> temp_Old_EQ_sheet_dates_min
#           
#           which(N50_Dates_YYYYMMDD >= temp_Old_EQ_sheet_dates_min) %>% 
#             N50_Dates_YYYYMMDD[.] -> temp_relevant_N50_Dates_YYYYMMDD
#           
#           temp_relevant_N50_Dates_YYYYMMDD %>% {. %in% temp_Old_EQ_sheet_dates} %>% {!.} %>% 
#             temp_relevant_N50_Dates_YYYYMMDD[.] -> temp_missing_dates_temp_sheet_num_EQ
#           
#           which(temp_missing_dates_temp_sheet_num_EQ <= temp_Symbol_raw_csv_Dates_Max) %>% 
#             temp_missing_dates_temp_sheet_num_EQ[.] -> temp_missing_dates_temp_sheet_num_EQ
#           
#           temp_Old_EQ_sheet %>% colnames -> temp_correct_colnames
#           
#           temp_correct_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
#             `colnames<-`(temp_correct_colnames) -> temp_new_EQ
#           
#           if (length(temp_missing_dates_temp_sheet_num_EQ) > 0)
#           {
#             temp_correct_colnames <- colnames(temp_Old_EQ_sheet)
#             
#             # i_temp_missing_dates_temp_sheet_num_EQ <- 1
#             for (i_temp_missing_dates_temp_sheet_num_EQ in 1:length(temp_missing_dates_temp_sheet_num_EQ)) 
#             {
#               Overwrite_Required <- TRUE
#               
#               "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#                 paste0(" updating EQ, ",i_temp_missing_dates_temp_sheet_num_EQ, " of ",
#                        length(temp_missing_dates_temp_sheet_num_EQ),sep='') %>% print
#               
#               temp_correct_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
#                 `colnames<-`(temp_correct_colnames) -> temp_temp_new_EQ
#               
#               EQ_Bhav_Database %>% setwd
#               
#               list.files() -> All_EQ_files
#               
#               All_EQ_files %>% as.Date(format = "cm%d%b%Ybhav.csv") %>% 
#                 format("%Y%m%d") %>% as.character %>% as.numeric -> All_EQ_dates_YYYYMMDD
#               
#               which(All_EQ_dates_YYYYMMDD == temp_missing_dates_temp_sheet_num_EQ[i_temp_missing_dates_temp_sheet_num_EQ]) %>% 
#                 .[1] %>% All_EQ_files[.] -> temp_relevant_EQ_file
#               
#               if(length(temp_relevant_EQ_file) > 0 & !is.na(temp_relevant_EQ_file[1]))
#               {
#                 temp_relevant_EQ_file %>% read.csv -> temp_relevant_EQ_file_df
#                 
#                 temp_relevant_EQ_file_df %>% colnames %>% {which(. == "TIMESTAMP")} %>% 
#                   {1:.} %>% temp_relevant_EQ_file_df[,.] %>% 
#                   `colnames<-`(temp_EQ_colnames) -> temp_relevant_EQ_file_df
#                 
#                 temp_relevant_EQ_file_df %>% filter(SYMBOL == temp_Symbol_from_filename) %>% 
#                   filter(SERIES == "EQ") -> temp_temp_new_EQ
#                 
#                 Correct_Date_format <- c()
#                 
#                 if(nrow(temp_temp_new_EQ) > 0 & nchar(as.character(temp_temp_new_EQ[1,"TIMESTAMP"])) >= 10)
#                 {
#                   Correct_Date_format <- "%d-%b-%Y"
#                 }else{
#                   Correct_Date_format <- "%d-%b-%y"
#                 } # End of 'if(nchar(as.character(temp_temp_new_EQ[1,"TIMESTAMP"])) >= 10)'
#                 
#                 if(nrow(temp_temp_new_EQ) > 0)
#                 {
#                   temp_temp_new_EQ$TIMESTAMP %>% unlist %>% as.character %>% as.Date(format=Correct_Date_format) %>% 
#                     format("%Y%m%d") %>% as.numeric -> temp_temp_new_EQ$TIMESTAMP
#                 } # End of 'if(nrow(temp_temp_new_EQ) > 0)'
#               } # End of 'if(length(temp_relevant_EQ_file) > 0)'
#               
#               if(!is.na(temp_temp_new_EQ[1,1]))
#               {
#                 temp_temp_new_EQ %>% rbind(temp_new_EQ,.) -> temp_new_EQ
#                 
#                 temp_new_EQ[with(temp_new_EQ, order(TIMESTAMP)), ] -> temp_new_EQ
#               } # End of 'if(!is.na(temp_temp_new_EQ[1,1]))'
#             } # End of 'for (i_temp_missing_dates_temp_sheet_num_EQ in 1:length(temp_missing_dates_temp_sheet_num_EQ)) '
#           } # End of 'if (length(temp_missing_dates_temp_sheet_num_EQ) > 0)'
#           
#           temp_new_EQ %>% rbind(temp_Old_EQ_sheet,.) -> temp_new_EQ
#           
#           writeData(temp_Symbol_Output_excel, sheet = Symbol_EQ_Sheetname, x = temp_new_EQ)
#           ##################
#           
#           ################## Tackling 'temp_sheet_num_F'
#           "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#             paste0(" updating F",sep='') %>% print
#           
#           Symbol_F_Sheetname <- paste0(temp_Symbol_in_sheet_names,"_F",sep='')
#           addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_F_Sheetname)
#           
#           OPTSTK_Excel_TS_Database %>% setwd
#           
#           temp_Options_excel_filename %>% read.xlsx(sheet = temp_sheet_num_F) -> temp_Old_F_sheet
#           
#           temp_Old_F_sheet[,which(colnames(temp_Old_F_sheet) == "TIMESTAMP")] %>% unlist %>% 
#             as.numeric -> temp_Old_F_sheet_dates
#           
#           temp_Old_F_sheet_dates %>% min -> temp_Old_F_sheet_dates_min
#           
#           which(N50_Dates_YYYYMMDD >= temp_Old_F_sheet_dates_min) %>% 
#             N50_Dates_YYYYMMDD[.] -> temp_relevant_N50_Dates_YYYYMMDD
#           
#           temp_relevant_N50_Dates_YYYYMMDD %>% {. %in% temp_Old_F_sheet_dates} %>% {!.} %>% 
#             temp_relevant_N50_Dates_YYYYMMDD[.] -> temp_missing_dates_temp_sheet_num_F
#           
#           which(temp_missing_dates_temp_sheet_num_F <= temp_Symbol_raw_csv_Dates_Max) %>% 
#             temp_missing_dates_temp_sheet_num_F[.] -> temp_missing_dates_temp_sheet_num_F
#           
#           temp_Old_F_sheet %>% colnames -> temp_correct_colnames
#           
#           temp_correct_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
#             `colnames<-`(temp_correct_colnames) -> temp_new_F
#           
#           if (length(temp_missing_dates_temp_sheet_num_F) > 0)
#           {
#             temp_correct_colnames <- colnames(temp_Old_F_sheet)
#             
#             # i_temp_missing_dates_temp_sheet_num_F <- 1
#             for (i_temp_missing_dates_temp_sheet_num_F in 1:length(temp_missing_dates_temp_sheet_num_F)) 
#             {
#               Overwrite_Required <- TRUE
#               
#               "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#                 paste0(" updating F, ",i_temp_missing_dates_temp_sheet_num_F, " of ",
#                        length(temp_missing_dates_temp_sheet_num_F),sep='') %>% print
#               
#               temp_correct_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
#                 `colnames<-`(temp_correct_colnames) -> temp_temp_new_F
#               
#               FnO_Bhav_Database %>% setwd
#               
#               list.files() -> All_FnO_files
#               
#               All_FnO_files %>% as.Date(format = "fo%d%b%Ybhav.csv") %>% 
#                 format("%Y%m%d") %>% as.character %>% as.numeric -> All_FnO_dates_YYYYMMDD
#               
#               which(All_FnO_dates_YYYYMMDD == temp_missing_dates_temp_sheet_num_F[i_temp_missing_dates_temp_sheet_num_F]) %>% 
#                 .[1] %>% All_FnO_files[.] -> temp_relevant_F_file
#               
#               if(length(temp_relevant_F_file) > 0)
#               {
#                 temp_relevant_F_file %>% read.csv -> temp_relevant_F_file_df
#                 
#                 temp_relevant_F_file_df %>% colnames %>% {which(. == "TIMESTAMP")} %>% 
#                   {1:.} %>% temp_relevant_F_file_df[,.] %>% 
#                   `colnames<-`(temp_FnO_colnames) -> temp_relevant_F_file_df
#                 
#                 temp_relevant_F_file_df %>% filter(SYMBOL == temp_Symbol_from_filename) %>% 
#                   filter(INSTRUMENT == "FUTSTK") -> temp_temp_new_F
#                 
#                 Correct_Date_format <- c()
#                 
#                 if(nrow(temp_temp_new_F) > 0 & nchar(as.character(temp_temp_new_F[1,"TIMESTAMP"])) >= 10)
#                 {
#                   Correct_Date_format <- "%d-%b-%Y"
#                 }else{
#                   Correct_Date_format <- "%d-%b-%y"
#                 } # End of 'if(nchar(as.character(temp_temp_new_F[1,"TIMESTAMP"])) >= 10)'
#                 
#                 if(nrow(temp_temp_new_F) > 0)
#                 {
#                   temp_temp_new_F$TIMESTAMP %>% unlist %>% as.character %>% as.Date(format=Correct_Date_format) %>% 
#                     format("%Y%m%d") %>% as.numeric -> temp_temp_new_F$TIMESTAMP
#                 } # End of 'if(nrow(temp_temp_new_F) > 0)'
#               } # End of 'if(length(temp_relevant_F_file) > 0)'
#               
#               if(!is.na(temp_temp_new_F[1,1]))
#               {
#                 temp_temp_new_F %>% rbind(temp_new_F,.) -> temp_new_F
#               } # End of 'if(!is.na(temp_temp_new_F[1,1]))'
#             } # End of 'for (i_temp_missing_dates_temp_sheet_num_F in 1:length(temp_missing_dates_temp_sheet_num_F))'
#           } # End of 'if (length(temp_missing_dates_temp_sheet_num_F) > 0)'
#           
#           temp_new_F[with(temp_new_F, order(TIMESTAMP,EXPIRY_DT)), ] -> temp_new_F
#           
#           temp_new_F %>% rbind(temp_Old_F_sheet,.) -> temp_new_F
#           
#           writeData(temp_Symbol_Output_excel, sheet = Symbol_F_Sheetname, x = temp_new_F)
#           ##################
#           
#           ################## Tackling 'temp_sheet_num_O_CE'
#           "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#             paste0(" updating O CE",sep='') %>% print
#           
#           Symbol_O_CE_Sheetname <- paste0(temp_Symbol_in_sheet_names,"_O_CE",sep='')
#           addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_O_CE_Sheetname)
#           
#           OPTSTK_Excel_TS_Database %>% setwd
#           
#           temp_Options_excel_filename %>% read.xlsx(sheet = temp_sheet_num_O_CE) -> temp_Old_O_CE_sheet
#           
#           temp_Old_O_CE_sheet[,which(colnames(temp_Old_O_CE_sheet) == "TIMESTAMP")] %>% unlist %>% 
#             as.numeric -> temp_Old_O_CE_sheet_dates
#           temp_Old_O_CE_sheet_dates %>% min -> temp_Old_O_CE_sheet_dates_min
#           
#           which(N50_Dates_YYYYMMDD >= temp_Old_O_CE_sheet_dates_min) %>% 
#             N50_Dates_YYYYMMDD[.] -> temp_relevant_N50_Dates_YYYYMMDD
#           
#           temp_relevant_N50_Dates_YYYYMMDD %>% {. %in% temp_Old_O_CE_sheet_dates} %>% {!.} %>% 
#             temp_relevant_N50_Dates_YYYYMMDD[.] -> temp_missing_dates_temp_sheet_num_O_CE
#           
#           which(temp_missing_dates_temp_sheet_num_O_CE <= temp_Symbol_raw_csv_Dates_Max) %>% 
#             temp_missing_dates_temp_sheet_num_O_CE[.] -> temp_missing_dates_temp_sheet_num_O_CE
#           
#           temp_Old_O_CE_sheet %>% colnames -> temp_correct_colnames
#           
#           temp_correct_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
#             `colnames<-`(temp_correct_colnames) -> temp_new_O_CE
#           
#           if (length(temp_missing_dates_temp_sheet_num_O_CE) > 0)
#           {
#             temp_correct_colnames <- colnames(temp_Old_O_CE_sheet)
#             
#             # i_temp_missing_dates_temp_sheet_num_O_CE <- 1
#             for (i_temp_missing_dates_temp_sheet_num_O_CE in 1:length(temp_missing_dates_temp_sheet_num_O_CE)) 
#             {
#               Overwrite_Required <- TRUE
#               
#               "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#                 paste0(" updating O CE, ",i_temp_missing_dates_temp_sheet_num_O_CE, " of ",
#                        length(temp_missing_dates_temp_sheet_num_O_CE),sep='') %>% print
#               
#               temp_correct_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
#                 `colnames<-`(temp_correct_colnames) -> temp_temp_new_O_CE
#               
#               FnO_Bhav_Database %>% setwd
#               
#               list.files() -> All_FnO_files
#               
#               All_FnO_files %>% as.Date(format = "fo%d%b%Ybhav.csv") %>% 
#                 format("%Y%m%d") %>% as.character %>% as.numeric -> All_FnO_dates_YYYYMMDD
#               
#               which(All_FnO_dates_YYYYMMDD == temp_missing_dates_temp_sheet_num_O_CE[i_temp_missing_dates_temp_sheet_num_O_CE]) %>% 
#                 .[1] %>% All_FnO_files[.] -> temp_relevant_O_CE_file
#               
#               if(length(temp_relevant_O_CE_file) > 0)
#               {
#                 temp_relevant_O_CE_file %>% read.csv -> temp_relevant_O_CE_file_df
#                 
#                 temp_relevant_O_CE_file_df %>% colnames %>% {which(. == "TIMESTAMP")} %>% 
#                   {1:.} %>% temp_relevant_O_CE_file_df[,.] %>% 
#                   `colnames<-`(temp_FnO_colnames) -> temp_relevant_O_CE_file_df
#                 
#                 temp_relevant_O_CE_file_df %>% filter(SYMBOL == temp_Symbol_from_filename) %>% 
#                   filter(INSTRUMENT == "OPTSTK") %>% filter(OPTIONTYPE == "CE") -> temp_temp_new_O_CE
#                 
#                 Correct_Date_format <- c()
#                 
#                 if(nrow(temp_temp_new_O_CE) > 0 & nchar(as.character(temp_temp_new_O_CE[1,"TIMESTAMP"])) >= 10)
#                 {
#                   Correct_Date_format <- "%d-%b-%Y"
#                 }else{
#                   Correct_Date_format <- "%d-%b-%y"
#                 } # End of 'if(nchar(as.character(temp_temp_new_O_CE[1,"TIMESTAMP"])) >= 10)'
#                 
#                 if(nrow(temp_temp_new_O_CE) > 0)
#                 {
#                   temp_temp_new_O_CE$TIMESTAMP %>% unlist %>% as.character %>% as.Date(format=Correct_Date_format) %>% 
#                     format("%Y%m%d") %>% as.numeric -> temp_temp_new_O_CE$TIMESTAMP
#                   
#                   temp_temp_new_O_CE$EXPIRY_DT %>% unlist %>% as.character %>% as.Date(format=Correct_Date_format) %>% 
#                     format("%Y%m%d") %>% as.numeric -> temp_temp_new_O_CE$EXPIRY_DT
#                 } # End of 'if(nrow(temp_temp_new_O_CE) > 0)'
#               } # End of 'if(length(temp_relevant_O_CE_file) > 0)'
#               
#               if(!is.na(temp_temp_new_O_CE[1,1]))
#               {
#                 temp_temp_new_O_CE %>% rbind(temp_new_O_CE,.) -> temp_new_O_CE
#               } # End of 'if(!is.na(temp_temp_new_O_CE[1,1]))'
#             } # End of 'for (i_temp_missing_dates_temp_sheet_num_O_CE in 1:length(temp_missing_dates_temp_sheet_num_O_CE))'
#           } # End of 'if (length(temp_missing_dates_temp_sheet_num_O_CE) > 0)'
#           
#           temp_new_O_CE[with(temp_new_O_CE, order(TIMESTAMP,EXPIRY_DT,OPTIONTYPE,STRIKE_PR)), ] -> temp_new_O_CE
#           
#           temp_new_O_CE %>% rbind(temp_Old_O_CE_sheet,.) -> temp_new_O_CE
#           
#           writeData(temp_Symbol_Output_excel, sheet = Symbol_O_CE_Sheetname, x = temp_new_O_CE)
#           ##################
#           
#           ################## Tackling 'temp_sheet_num_O_PE'
#           "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#             paste0(" updating O PE",sep='') %>% print
#           
#           Symbol_O_PE_Sheetname <- paste0(temp_Symbol_in_sheet_names,"_O_PE",sep='')
#           addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_O_PE_Sheetname)
#           
#           OPTSTK_Excel_TS_Database %>% setwd
#           
#           temp_Options_excel_filename %>% read.xlsx(sheet = temp_sheet_num_O_PE) -> temp_Old_O_PE_sheet
#           
#           temp_Old_O_PE_sheet[,which(colnames(temp_Old_O_PE_sheet) == "TIMESTAMP")] %>% unlist %>% 
#             as.numeric -> temp_Old_O_PE_sheet_dates
#           temp_Old_O_PE_sheet_dates %>% min -> temp_Old_O_PE_sheet_dates_min
#           
#           which(N50_Dates_YYYYMMDD >= temp_Old_O_PE_sheet_dates_min) %>% 
#             N50_Dates_YYYYMMDD[.] -> temp_relevant_N50_Dates_YYYYMMDD
#           
#           temp_relevant_N50_Dates_YYYYMMDD %>% {. %in% temp_Old_O_PE_sheet_dates} %>% {!.} %>% 
#             temp_relevant_N50_Dates_YYYYMMDD[.] -> temp_missing_dates_temp_sheet_num_O_PE
#           
#           which(temp_missing_dates_temp_sheet_num_O_PE <= temp_Symbol_raw_csv_Dates_Max) %>% 
#             temp_missing_dates_temp_sheet_num_O_PE[.] -> temp_missing_dates_temp_sheet_num_O_PE
#           
#           temp_Old_O_PE_sheet %>% colnames -> temp_correct_colnames
#           
#           temp_correct_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
#             `colnames<-`(temp_correct_colnames) -> temp_new_O_PE
#           
#           if (length(temp_missing_dates_temp_sheet_num_O_PE) > 0)
#           {
#             temp_correct_colnames <- colnames(temp_Old_O_PE_sheet)
#             
#             # i_temp_missing_dates_temp_sheet_num_O_PE <- 1
#             for (i_temp_missing_dates_temp_sheet_num_O_PE in 1:length(temp_missing_dates_temp_sheet_num_O_PE)) 
#             {
#               Overwrite_Required <- TRUE
#               
#               "For Symbol: " %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#                 paste0(" updating O PE, ",i_temp_missing_dates_temp_sheet_num_O_PE, " of ",
#                        length(temp_missing_dates_temp_sheet_num_O_PE),sep='') %>% print
#               
#               temp_correct_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
#                 `colnames<-`(temp_correct_colnames) -> temp_temp_new_O_PE
#               
#               FnO_Bhav_Database %>% setwd
#               
#               list.files() -> All_FnO_files
#               
#               All_FnO_files %>% as.Date(format = "fo%d%b%Ybhav.csv") %>% 
#                 format("%Y%m%d") %>% as.character %>% as.numeric -> All_FnO_dates_YYYYMMDD
#               
#               which(All_FnO_dates_YYYYMMDD == temp_missing_dates_temp_sheet_num_O_CE[i_temp_missing_dates_temp_sheet_num_O_PE]) %>% 
#                 .[1] %>% All_FnO_files[.] -> temp_relevant_O_PE_file
#               
#               if(length(temp_relevant_O_PE_file) > 0)
#               {
#                 temp_relevant_O_PE_file %>% read.csv -> temp_relevant_O_PE_file_df
#                 
#                 temp_relevant_O_PE_file_df %>% colnames %>% {which(. == "TIMESTAMP")} %>% 
#                   {1:.} %>% temp_relevant_O_PE_file_df[,.] %>% 
#                   `colnames<-`(temp_FnO_colnames) -> temp_relevant_O_PE_file_df
#                 
#                 temp_relevant_O_PE_file_df %>% filter(SYMBOL == temp_Symbol_from_filename) %>% 
#                   filter(INSTRUMENT == "OPTSTK") %>% filter(OPTIONTYPE == "PE") -> temp_temp_new_O_PE
#                 
#                 Correct_Date_format <- c()
#                 
#                 if(nrow(temp_temp_new_O_PE) > 0 & nchar(as.character(temp_temp_new_O_PE[1,"TIMESTAMP"])) >= 10)
#                 {
#                   Correct_Date_format <- "%d-%b-%Y"
#                 }else{
#                   Correct_Date_format <- "%d-%b-%y"
#                 } # End of 'if(nchar(as.character(temp_temp_new_O_PE[1,"TIMESTAMP"])) >= 10)'
#                 
#                 if(nrow(temp_temp_new_O_PE) > 0)
#                 {
#                   temp_temp_new_O_PE$TIMESTAMP %>% unlist %>% as.character %>% as.Date(format=Correct_Date_format) %>% 
#                     format("%Y%m%d") -> temp_temp_new_O_PE$TIMESTAMP
#                   
#                   temp_temp_new_O_PE$EXPIRY_DT %>% unlist %>% as.character %>% as.Date(format=Correct_Date_format) %>% 
#                     format("%Y%m%d") -> temp_temp_new_O_PE$EXPIRY_DT
#                 } # End of 'if(nrow(temp_temp_new_O_PE) > 0)'
#               } # End of 'if(length(temp_relevant_O_PE_file) > 0)'
#               
#               if(!is.na(temp_temp_new_O_PE[1,1]))
#               {
#                 temp_temp_new_O_PE %>% rbind(temp_new_O_PE,.) -> temp_new_O_PE
#               } # End of 'if(!is.na(temp_temp_new_O_PE[1,1]))'
#             } # End of 'for (i_temp_missing_dates_temp_sheet_num_O_PE in 1:length(temp_missing_dates_temp_sheet_num_O_PE))'
#           } # End of 'if (length(temp_missing_dates_temp_sheet_num_O_PE) > 0)'
#           
#           temp_new_O_PE[with(temp_new_O_PE, order(TIMESTAMP,EXPIRY_DT,OPTIONTYPE,STRIKE_PR)), ] -> temp_new_O_PE
#           
#           temp_new_O_PE %>% rbind(temp_Old_O_PE_sheet,.) -> temp_new_O_PE
#           
#           writeData(temp_Symbol_Output_excel, sheet = Symbol_O_PE_Sheetname, x = temp_new_O_PE)
#           ##################
#           
#         } # End of 'if (expiry_counter > 0 & expiry_counter)'
#         
#       } # End of '(temp_old_file_max_date >= as.numeric(Max_Nifty_Date))'
#       # Overwrite_Required <- FALSE # or TRUE, to be checked
#     }else{
#       Overwrite_Required <- FALSE # Because it is new writing
#       
#       # Reading old csv file
#       OPTSTK_TS_Database %>% setwd
#       temp_Options_csv_filename %>% read.csv -> temp_Symbol_raw_csv
#       
#       # Splitting into PE and CE
#       temp_Symbol_raw_csv[,as.integer(1:((ncol(temp_Symbol_raw_csv))/2))] -> temp_Symbol_raw_csv_CE
#       temp_Symbol_raw_csv[,((1 + ncol(temp_Symbol_raw_csv_CE)):ncol(temp_Symbol_raw_csv))] -> temp_Symbol_raw_csv_PE
#       
#       # Finding number of expiries
#       temp_Symbol_raw_csv_PE %>% colnames -> temp_Symbol_raw_csv_PE_colnames
#       temp_Symbol_raw_csv_CE %>% colnames -> temp_Symbol_raw_csv_CE_colnames
#       expiry_counter <- MAX_Expiry_Trials
#       Found_Expiry_limit <- FALSE
#       while (!Found_Expiry_limit)
#       {
#         expiry_trial <- grep(paste0("_Expiry_",expiry_counter,"_",sep=''),temp_Symbol_raw_csv_PE_colnames)
#         if (length(expiry_trial) > 0 & expiry_counter > 0)
#         {
#           Found_Expiry_limit <- TRUE
#         } # End of 'if (length(expiry_trial) > 0)'
#         
#         if (Found_Expiry_limit)
#         {
#           break
#         } # End of 'if (Found_Expiry_limit)'
#         expiry_counter <- expiry_counter - 1
#         
#         if (expiry_counter < 0)
#         {
#           print(paste0("Check source csv file: '", temp_Options_csv_filename,"' no expiry column found",sep=""))
#           break
#         } # End of 'if (expiry_counter < 0)'
#       } # End of 'while (!Found_Expiry_limit)'
#       
#       if (Found_Expiry_limit & expiry_counter > 0)
#       {
#         print(paste0("Found ", expiry_counter," expiries from file: '",temp_Options_csv_filename,"'",sep=""))
#         
#         # Constructing output Excel Sheet:
#         temp_Symbol_Output_excel <- createWorkbook()
#         temp_Symbol_Output_excel_sheetnames <- c()
#         Symbol_CE_ATM_Sheetname <- paste0(temp_Symbol,"_CE_ATM",sep='')
#         Symbol_CE_ATM_Sheetname %>% gsub("&","_and_",.) -> Symbol_CE_ATM_Sheetname
#         temp_Symbol_Output_excel_sheetnames <- c(temp_Symbol_Output_excel_sheetnames,Symbol_CE_ATM_Sheetname)
#         
#         print(paste0("Making 'CE ATM' of ", temp_Symbol,sep=''))
#         
#         # Constructing CE ATM
#         addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_CE_ATM_Sheetname)
#         grep("_ATM",temp_Symbol_raw_csv_CE_colnames) %>% max %>% {.+1} -> temp_Symbol_raw_csv_CE_max_ATM_column
#         writeData(temp_Symbol_Output_excel, sheet = Symbol_CE_ATM_Sheetname,
#                   x = temp_Symbol_raw_csv_CE[,(1:temp_Symbol_raw_csv_CE_max_ATM_column)])
#         
#         print(paste0("Making 'CE Expiries' of ", temp_Symbol,sep=''))
#         
#         # Constructing CE Expiries
#         # i_CE_expiry <- 1
#         for(i_CE_expiry in 1:expiry_counter)
#         {
#           paste0(temp_Symbol,"_CE_Expiry_",i_CE_expiry,sep="") -> temp_CE_temp_expiry_sheetname
#           temp_CE_temp_expiry_sheetname %>% gsub("&","_and_",.) -> temp_CE_temp_expiry_sheetname
#           temp_Symbol_Output_excel_sheetnames <- c(temp_Symbol_Output_excel_sheetnames,temp_CE_temp_expiry_sheetname)
#           
#           i_CE_expiry %>% paste0("_CE_Expiry_",.,"_",sep='') %>% 
#             grep(.,temp_Symbol_raw_csv_CE_colnames) -> temp_CE_temp_Expiry_columns
#           
#           which(temp_Symbol_raw_csv_CE_colnames == paste0("CE_Expiry_",i_CE_expiry,sep='')) -> temp_CE_expiry_date
#           
#           temp_Symbol_raw_csv_CE[,c(1,temp_CE_expiry_date,temp_CE_temp_Expiry_columns)] -> temp_CE_temp_Expiry_sheet
#           
#           addWorksheet(temp_Symbol_Output_excel, sheet = temp_CE_temp_expiry_sheetname)
#           writeData(temp_Symbol_Output_excel, sheet = temp_CE_temp_expiry_sheetname,
#                     x = temp_CE_temp_Expiry_sheet)
#         } # End of 'for(i_CE_expiry in 1:expiry_counter)'
#         
#         print(paste0("Making 'PE ATM' of ", temp_Symbol,sep=''))
#         
#         # Constructing PE ATM
#         Symbol_PE_ATM_Sheetname <- paste0(temp_Symbol,"_PE_ATM",sep='')
#         Symbol_PE_ATM_Sheetname %>% gsub("&","_and_",.) -> Symbol_PE_ATM_Sheetname
#         temp_Symbol_Output_excel_sheetnames <- c(temp_Symbol_Output_excel_sheetnames,Symbol_PE_ATM_Sheetname)
#         addWorksheet(temp_Symbol_Output_excel, sheet = Symbol_PE_ATM_Sheetname)
#         grep("_ATM",temp_Symbol_raw_csv_PE_colnames) %>% max %>% {.+1} -> temp_Symbol_raw_csv_PE_max_ATM_column
#         
#         writeData(temp_Symbol_Output_excel, sheet = Symbol_PE_ATM_Sheetname,
#                   x = temp_Symbol_raw_csv_PE[,(1:temp_Symbol_raw_csv_PE_max_ATM_column)])
#         
#         print(paste0("Making 'PE Expiries' of ", temp_Symbol,sep=''))
#         
#         # Constructing PE Expiries
#         # i_PE_expiry <- 1
#         for(i_PE_expiry in 1:expiry_counter)
#         {
#           paste0(temp_Symbol,"_PE_Expiry_",i_PE_expiry,sep="") -> temp_PE_temp_expiry_sheetname
#           temp_PE_temp_expiry_sheetname %>% gsub("&","_and_",.) -> temp_PE_temp_expiry_sheetname
#           temp_Symbol_Output_excel_sheetnames <- c(temp_Symbol_Output_excel_sheetnames,temp_PE_temp_expiry_sheetname)
#           
#           i_PE_expiry %>% paste0("_PE_Expiry_",.,"_",sep='') %>% 
#             grep(.,temp_Symbol_raw_csv_PE_colnames) -> temp_PE_temp_Expiry_columns
#           
#           which(temp_Symbol_raw_csv_PE_colnames == paste0("PE_Expiry_",i_PE_expiry,sep='')) -> temp_PE_expiry_date
#           
#           temp_Symbol_raw_csv_PE[,c(1,temp_PE_expiry_date,temp_PE_temp_Expiry_columns)] -> temp_PE_temp_Expiry_sheet
#           
#           addWorksheet(temp_Symbol_Output_excel, sheet = temp_PE_temp_expiry_sheetname)
#           writeData(temp_Symbol_Output_excel, sheet = temp_PE_temp_expiry_sheetname,
#                     x = temp_PE_temp_Expiry_sheet)
#         } # End of 'for(i_PE_expiry in 1:expiry_counter)'
#         
#         # Getting EQ and FnO data for all relevant expiries on a sheet
#         # Finding Min and max date from the 'temp_Symbol_raw_csv'
#         
#         temp_Symbol_raw_csv %>% select(Date_YYYYMMDD_C) %>% unlist %>% as.character %>% 
#           as.Date(format="%Y%m%d") -> All_FnO_relevant_dates
#         
#         All_FnO_relevant_dates %>% max -> All_FnO_relevant_dates_Max
#         
#         All_FnO_relevant_dates %>% min -> All_FnO_relevant_dates_Min
#         
#         # Finding relevant EQ and recoding it in a df
#         
#         temp_EQ_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame(stringsAsFactors = F) %>% 
#           `colnames<-`(temp_EQ_colnames) -> temp_Symbol_relevant_EQ
#         
#         if(length(All_FnO_relevant_dates) > 0)
#         {
#           EQ_Bhav_Database %>% setwd
#           
#           list.files() -> All_EQ_files
#           
#           All_EQ_files %>% as.Date(format="cm%d%b%Ybhav.csv") %>% {. <= All_FnO_relevant_dates_Max} %>% 
#             All_EQ_files[.] %>% as.Date(format="cm%d%b%Ybhav.csv") %>% {. >= All_FnO_relevant_dates_Min} %>% 
#             All_EQ_files[.] %>% unique -> All_relevant_EQ_files
#           
#           # Sorting All_relevant_EQ_files by date
#           All_relevant_EQ_files %>% as.Date(format="cm%d%b%Ybhav.csv") -> All_relevant_EQ_dates
#           
#           (1:length(All_relevant_EQ_dates)) -> names(All_relevant_EQ_dates)
#           
#           All_relevant_EQ_dates %>% sort %>% names(.) %>% as.numeric %>% All_relevant_EQ_files[.] %>% 
#             unlist %>% as.character -> All_relevant_EQ_files
#           
#           if (length(All_relevant_EQ_files) > 0)
#           {
#             # i_All_relevant_EQ_files <- 1
#             for(i_All_relevant_EQ_files in 1:length(All_relevant_EQ_files))
#             {
#               print(paste0("For Symbol ", temp_Symbol,", reading ",i_All_relevant_EQ_files,
#                            " out of ", length(All_relevant_EQ_files), " relevant EQ files.",sep=''))
#               
#               EQ_Bhav_Database %>% setwd
#               
#               i_All_relevant_EQ_files %>% All_relevant_EQ_files[.] %>% read.csv -> temp_EQ_csv
#               
#               which(colnames(temp_EQ_csv) == "TIMESTAMP") %>% {1:.} %>% temp_EQ_csv[,.] %>% 
#                 `colnames<-`(temp_EQ_colnames)-> temp_EQ_csv
#               
#               temp_EQ_csv %>% filter(SERIES == "EQ") %>% filter(SYMBOL == temp_Symbol) %>% 
#                 filter(SYMBOL == temp_Symbol) -> temp_relevant_EQ_df
#               
#               if(nrow(temp_relevant_EQ_df) > 0)
#               {
#                 temp_relevant_EQ_df %>% rbind(temp_Symbol_relevant_EQ,.) -> temp_Symbol_relevant_EQ
#               } # End of 'if(nrow(temp_relevant_EQ_df) > 0)'
#             } # End of 'for(i_All_relevant_EQ_files in 1:length(All_relevant_EQ_files))'
#           } # End of 'if (length(All_relevant_EQ_files) > 0)'
#         } # End of 'if(nrow(temp_contract_identication_df_abridged) > 0)'
#         ###################
#         
#         ################### Adding temp_Symbol_relevant_EQ to a sheet
#         
#         temp_Symbol_relevant_EQ %>% select(TIMESTAMP) %>% unlist %>% as.character -> temp_Symbol_relevant_EQ$TIMESTAMP
#         
#         temp_Symbol_relevant_EQ %>% select(TIMESTAMP) %>% unlist %>% as.character %>% 
#           nchar %>% unique -> temp_Symbol_relevant_EQ_nchar_unique
#         
#         if(length(temp_Symbol_relevant_EQ_nchar_unique) > 1)
#         {
#           which( (temp_Symbol_relevant_EQ %>% select(TIMESTAMP) %>% unlist %>% as.character %>% nchar)
#                  < 10) -> wrong_date_rows
#           
#           if(length(wrong_date_rows) > 0)
#           {
#             wrong_date_rows %>% temp_Symbol_relevant_EQ[.,] %>% select(TIMESTAMP) %>% 
#               unlist %>% as.character %>% as.Date(format="%d-%b-%y") %>% 
#               format("%d-%b-%Y") -> temp_Symbol_relevant_EQ[wrong_date_rows,"TIMESTAMP"]
#           } # End of 'if(length(wrong_date_rows) > 0)'
#           
#         } # End of 'if(length(temp_Symbol_relevant_EQ_nchar_unique) > 0)'
#         
#         temp_Symbol_relevant_EQ %>% select(TIMESTAMP) %>% unlist %>% as.character %>% 
#           as.Date(format="%d-%b-%Y") %>% format("%Y%m%d") %>% as.numeric -> temp_Symbol_relevant_EQ$TIMESTAMP
#         
#         temp_Symbol_relevant_EQ %>% arrange(.,TIMESTAMP) -> temp_Symbol_relevant_EQ
#         
#         if(nrow(temp_Symbol_relevant_EQ) > 0)
#         {
#           temp_Symbol_relevant_EQ_sheetname <- paste0(temp_Symbol,"_EQ",sep='')
#           temp_Symbol_relevant_EQ_sheetname %>% gsub("&","_and_",.) -> temp_Symbol_relevant_EQ_sheetname
#           
#           temp_Symbol_Output_excel_sheetnames <- c(temp_Symbol_Output_excel_sheetnames,temp_Symbol_relevant_EQ_sheetname)
#           
#           print(paste0("For Symbol ", temp_Symbol,", making new sheet: ",temp_Symbol_relevant_EQ_sheetname,sep=''))
#           
#           addWorksheet(temp_Symbol_Output_excel, sheet = temp_Symbol_relevant_EQ_sheetname)
#           writeData(temp_Symbol_Output_excel, sheet = temp_Symbol_relevant_EQ_sheetname,
#                     x = temp_Symbol_relevant_EQ)
#         } # End of 'if(nrow(temp_Symbol_relevant_EQ) > 0)'
#         ###################
#         
#         ###################
#         
#         # Finding relevant FnO separately and recoding it in a df, 
#         # using All_Fno_relevant_dates, All_FnO_relevant_dates_Max & All_FnO_relevant_dates_Min
#         temp_FnO_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame(stringsAsFactors = F) %>% 
#           `colnames<-`(temp_FnO_colnames) -> temp_Symbol_relevant_F
#         
#         temp_FnO_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame(stringsAsFactors = F) %>% 
#           `colnames<-`(temp_FnO_colnames) -> temp_Symbol_relevant_O
#         
#         if(length(All_FnO_relevant_dates) > 0)
#         {
#           FnO_Bhav_Database %>% setwd
#           
#           list.files() -> All_FnO_files
#           
#           All_FnO_files %>% as.Date(format="fo%d%b%Ybhav.csv") %>% {. <= All_FnO_relevant_dates_Max} %>% 
#             All_FnO_files[.] %>% as.Date(format="fo%d%b%Ybhav.csv") %>% {. >= All_FnO_relevant_dates_Min} %>% 
#             All_FnO_files[.] %>% unique -> All_relevant_FnO_files
#           
#           # Sorting All_relevant_FnO_files by date
#           All_relevant_FnO_files %>% as.Date(format="fo%d%b%Ybhav.csv") -> All_relevant_FnO_dates
#           
#           (1:length(All_relevant_FnO_dates)) -> names(All_relevant_FnO_dates)
#           
#           All_relevant_FnO_dates %>% sort %>% names(.) %>% as.numeric %>% 
#             All_relevant_FnO_files[.] %>% unlist %>% as.character -> All_relevant_FnO_files
#           
#           if (length(All_relevant_FnO_files) > 0)
#           {
#             # i_All_relevant_FnO_files <- 1
#             for(i_All_relevant_FnO_files in 1:length(All_relevant_FnO_files))
#             {
#               print(paste0("For Symbol ", temp_Symbol,", reading ",i_All_relevant_FnO_files,
#                            " out of ", length(All_relevant_FnO_files), " relevant FnO files.",sep=''))
#               
#               FnO_Bhav_Database %>% setwd
#               
#               i_All_relevant_FnO_files %>% All_relevant_FnO_files[.] %>% read.csv -> temp_FnO_csv
#               
#               which(colnames(temp_FnO_csv) == "TIMESTAMP") %>% {1:.} %>% temp_FnO_csv[,.] %>% 
#                 `colnames<-`(temp_FnO_colnames)-> temp_FnO_csv
#               
#               temp_FnO_csv %>% filter(INSTRUMENT == "OPTSTK") %>% filter(SYMBOL == temp_Symbol) %>% 
#                 filter(SYMBOL == temp_Symbol) -> temp_relevant_O_df
#               
#               temp_FnO_csv %>% filter(INSTRUMENT == "FUTSTK") %>% filter(SYMBOL == temp_Symbol) %>% 
#                 filter(SYMBOL == temp_Symbol) -> temp_relevant_F_df
#               
#               if(nrow(temp_relevant_F_df) > 0 & nrow(temp_relevant_O_df) > 0)
#               {
#                 temp_relevant_F_df %>% rbind(temp_Symbol_relevant_F,.) -> temp_Symbol_relevant_F
#                 
#                 temp_relevant_O_df %>% rbind(temp_Symbol_relevant_O,.) -> temp_Symbol_relevant_O
#               } # End of 'if(nrow(temp_relevant_F_df) > 0 & nrow(temp_relevant_O_df) > 0)'
#             } # End of 'forfor(i_All_relevant_FnO_files in 1:length(All_relevant_FnO_files))'
#           } # End of 'if (length(All_relevant_FnO_files) > 0)'
#         } # End of 'if(length(All_FnO_relevant_dates) > 0)'
#         ###################
#         
#         ################### Adding temp_Symbol_relevant_F to a sheet
#         
#         temp_Symbol_relevant_F %>% select(TIMESTAMP) %>% unlist %>% 
#           as.character -> temp_Symbol_relevant_F$TIMESTAMP
#         
#         temp_Symbol_relevant_F %>% select(EXPIRY_DT) %>% unlist %>% 
#           as.character -> temp_Symbol_relevant_F$EXPIRY_DT
#         
#         temp_Symbol_relevant_F %>% select(TIMESTAMP) %>% unlist %>% as.character %>% 
#           nchar %>% unique -> temp_Symbol_relevant_F_nchar_unique
#         
#         if(length(temp_Symbol_relevant_F_nchar_unique) > 1)
#         {
#           which( (temp_Symbol_relevant_F %>% select(TIMESTAMP) %>% unlist %>% as.character %>% 
#                     nchar) < 10) -> wrong_date_rows
#           
#           if (length(wrong_date_rows) > 0)
#           {
#             wrong_date_rows %>% temp_Symbol_relevant_F[.,] %>% select(TIMESTAMP) %>% 
#               unlist %>% as.character %>% as.Date(format="%d-%b-%y") %>% 
#               format("%d-%b-%Y") %>% as.character -> temp_Symbol_relevant_F[wrong_date_rows,"TIMESTAMP"]
#             
#             wrong_date_rows %>% temp_Symbol_relevant_F[.,] %>% select(EXPIRY_DT) %>% 
#               unlist %>% as.character %>% as.Date(format="%d-%b-%y") %>% 
#               format("%d-%b-%Y") %>% as.character -> temp_Symbol_relevant_F[wrong_date_rows,"EXPIRY_DT"]
#           }# End of 'if (length(wrong_date_rows) > 0)'
#           
#         } # End of 'if(length(temp_Symbol_relevant_F_nchar_unique) > 0
#         
#         temp_Symbol_relevant_F %>% select(TIMESTAMP) %>% unlist %>% as.character %>% 
#           as.Date(format="%d-%b-%Y") %>% format("%Y%m%d") %>% as.numeric -> temp_Symbol_relevant_F$TIMESTAMP
#         
#         temp_Symbol_relevant_F %>% select(EXPIRY_DT) %>% unlist %>% as.character %>% 
#           as.Date(format="%d-%b-%Y") %>% format("%Y%m%d") %>% as.numeric -> temp_Symbol_relevant_F$EXPIRY_DT
#         
#         temp_Symbol_relevant_F[with(temp_Symbol_relevant_F, order(TIMESTAMP,EXPIRY_DT)), ] -> temp_Symbol_relevant_F
#         
#         if(nrow(temp_Symbol_relevant_F) > 0)
#         {
#           temp_Symbol_relevant_F_sheetname <- paste0(temp_Symbol,"_F",sep='')
#           temp_Symbol_relevant_F_sheetname %>% gsub("&","_and_",.) -> temp_Symbol_relevant_F_sheetname
#           
#           temp_Symbol_Output_excel_sheetnames <- c(temp_Symbol_Output_excel_sheetnames,temp_Symbol_relevant_F_sheetname)
#           
#           print(paste0("For Symbol ", temp_Symbol,", making new sheet: ",temp_Symbol_relevant_F_sheetname,sep=''))
#           
#           addWorksheet(temp_Symbol_Output_excel, sheet = temp_Symbol_relevant_F_sheetname)
#           writeData(temp_Symbol_Output_excel, sheet = temp_Symbol_relevant_F_sheetname,
#                     x = temp_Symbol_relevant_F)
#         } # End of 'if(nrow(temp_Symbol_relevant_F) > 0)'
#         ###################
#         
#         ################### Adding temp_Symbol_relevant_O to a sheet
#         
#         temp_Symbol_relevant_O %>% select(TIMESTAMP) %>% unlist %>% 
#           as.character -> temp_Symbol_relevant_O$TIMESTAMP
#         
#         temp_Symbol_relevant_O %>% select(EXPIRY_DT) %>% unlist %>% 
#           as.character -> temp_Symbol_relevant_O$EXPIRY_DT
#         
#         temp_Symbol_relevant_O %>% select(TIMESTAMP) %>% unlist %>% as.character %>% 
#           nchar %>% unique -> temp_Symbol_relevant_O_nchar_unique
#         
#         if(length(temp_Symbol_relevant_O_nchar_unique) > 1)
#         {
#           which( (temp_Symbol_relevant_O %>% select(TIMESTAMP) %>% unlist %>% as.character %>% 
#                     nchar) < 10) -> wrong_date_rows
#           
#           if (length(wrong_date_rows) > 0)
#           {
#             wrong_date_rows %>% temp_Symbol_relevant_O[.,] %>% select(TIMESTAMP) %>% 
#               unlist %>% as.character %>% as.Date(format="%d-%b-%y") %>% 
#               format("%d-%b-%Y") -> temp_Symbol_relevant_O[wrong_date_rows,"TIMESTAMP"]
#             
#             wrong_date_rows %>% temp_Symbol_relevant_O[.,] %>% select(EXPIRY_DT) %>% 
#               unlist %>% as.character %>% as.Date(format="%d-%b-%y") %>% 
#               format("%d-%b-%Y") -> temp_Symbol_relevant_O[wrong_date_rows,"EXPIRY_DT"]
#           } # End of 'if (length(wrong_date_rows) > 0)'
#           
#         } # End of 'if(length(temp_Symbol_relevant_O_nchar_unique) > 0
#         
#         temp_Symbol_relevant_O %>% select(TIMESTAMP) %>% unlist %>% as.character %>% 
#           as.Date(format="%d-%b-%Y") %>% format("%Y%m%d") %>% as.numeric -> temp_Symbol_relevant_O$TIMESTAMP
#         
#         temp_Symbol_relevant_O %>% select(EXPIRY_DT) %>% unlist %>% as.character %>% 
#           as.Date(format="%d-%b-%Y") %>% format("%Y%m%d") %>% as.numeric -> temp_Symbol_relevant_O$EXPIRY_DT
#         
#         temp_Symbol_relevant_O[with(temp_Symbol_relevant_O, order(TIMESTAMP,EXPIRY_DT,OPTIONTYPE,STRIKE_PR)), ] -> temp_Symbol_relevant_O
#         
#         temp_Symbol_relevant_O[temp_Symbol_relevant_O$OPTIONTYPE == "CE",] -> temp_Symbol_relevant_O_CE
#         temp_Symbol_relevant_O[temp_Symbol_relevant_O$OPTIONTYPE == "PE",] -> temp_Symbol_relevant_O_PE
#         
#         if(nrow(temp_Symbol_relevant_O_CE) > 0)
#         {
#           temp_Symbol_relevant_O_CE_sheetname <- paste0(temp_Symbol,"_O_CE",sep='')
#           temp_Symbol_relevant_O_CE_sheetname %>% gsub("&","_and_",.) -> temp_Symbol_relevant_O_CE_sheetname
#           temp_Symbol_Output_excel_sheetnames <- c(temp_Symbol_Output_excel_sheetnames,temp_Symbol_relevant_O_CE_sheetname)
#           
#           print(paste0("For Symbol ", temp_Symbol,", making new sheet: ",temp_Symbol_relevant_O_CE_sheetname,sep=''))
#           
#           addWorksheet(temp_Symbol_Output_excel, sheet = temp_Symbol_relevant_O_CE_sheetname)
#           writeData(temp_Symbol_Output_excel, sheet = temp_Symbol_relevant_O_CE_sheetname,
#                     x = temp_Symbol_relevant_O_CE)
#         } # End of 'if(nrow(temp_Symbol_relevant_O_CE) > 0)'
#         
#         if(nrow(temp_Symbol_relevant_O_PE) > 0)
#         {
#           temp_Symbol_relevant_O_PE_sheetname <- paste0(temp_Symbol,"_O_PE",sep='')
#           temp_Symbol_relevant_O_PE_sheetname %>% gsub("&","_and_",.) -> temp_Symbol_relevant_O_PE_sheetname
#           temp_Symbol_Output_excel_sheetnames <- c(temp_Symbol_Output_excel_sheetnames,temp_Symbol_relevant_O_PE_sheetname)
#           
#           print(paste0("For Symbol ", temp_Symbol,", making new sheet: ",temp_Symbol_relevant_O_PE_sheetname,sep=''))
#           
#           addWorksheet(temp_Symbol_Output_excel, sheet = temp_Symbol_relevant_O_PE_sheetname)
#           writeData(temp_Symbol_Output_excel, sheet = temp_Symbol_relevant_O_PE_sheetname,
#                     x = temp_Symbol_relevant_O_PE)
#         } # End of 'if(nrow(temp_Symbol_relevant_O_PE) > 0)'
#         ###################
#         
#       } # End of 'if (Found_Expiry_limit)'
#       
#     } # End of 'if(Old_Excel_Exists)'
#     
#     # Saving the file, only when required
#     
#     if(!Old_Excel_Exists)
#     {
#       OPTSTK_Excel_TS_Database %>% setwd
#       paste0("Making new file: ",temp_Options_excel_filename, sep='')
#       saveWorkbook(temp_Symbol_Output_excel, temp_Options_excel_filename)
#     }else{
#       if(Overwrite_Required)
#       {
#         OPTSTK_Excel_TS_Database %>% setwd
#         "For Symbol: '" %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#           paste0("' overwriting! Deleting old file: '",temp_Options_excel_filename,"'",sep='') %>% print
#         
#         # file.remove(temp_Options_excel_filename) # ', overwrite = T' argument is a better approach
#         
#         "For Symbol: '" %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#           paste0("' saving with new data, new filename: '",temp_Options_excel_filename,"'",sep='') %>% print
#         
#         saveWorkbook(temp_Symbol_Output_excel, temp_Options_excel_filename, overwrite = T)
#       }else{
#         "For Symbol: '" %>% paste0(temp_Symbol_from_filename,sep='') %>% 
#           paste0("', Old file: '",temp_Options_excel_filename,"' was up to date",sep='') %>% print
#       } # End of 'if(Overwrite_Required)'
#     } # End of 'if(!Old_Excel_Exists)'
#   } # End of 'for (i_1 in 1: length(OPTSTK_TS_Database_Files))'
# }else{
#   print("'OPTSTK_TS_Database' folder is empty")
# } # End of 'if (length(OPTSTK_TS_Database_Files) > 0)'
# 
# #################################################################################
# #################################################################################

#################################################################################
#################################### Goals ######################################

# Date: 2020-June-18
# Author: Arunabha Sarkar

# Goals: Survivor Adjustment Nifty 50
# File Name: Survivor_Adjustment_Nifty_50

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

Output_filename <- 'Survivor_Adjustment_Nifty_50_Tickers.csv'

"C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% paste0("/Nifty 50 Historical Data Investing dot com",
                            sep='') -> Nifty50_Database  # To find out which day is open & NIFTY Spot

Central_Database %>% paste0("/Survivorship Adjustments Indexes",sep='') -> Survivorship_Adjustments_Database  # To find out which day is open & NIFTY Spot

Central_Database %>% gsub("Central Database","",.) %>% 
  paste0(.,"Codes_Handling_Central_Database/Survivorship Adjustments Indexes/Raw Data Files",
         sep='') -> Survivorship_Adjustments_Raw_Database

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
######################## Finding Latest Nifty 50 List ###########################

Survivorship_Adjustments_Raw_Database %>% setwd

list.files() -> Survivorship_Adjustments_Raw_Database_Files

Survivorship_Adjustments_Raw_Database_Files %>% tolower %>% grep("nifty50",.) %>% 
  Survivorship_Adjustments_Raw_Database_Files[.] -> All_Nifty_50_candidates

All_Nifty_50_candidates %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>%
  as.Date(format="%Y%m%d") %>% which.max %>% All_Nifty_50_candidates[.] %>% 
  read.xlsx(sheet = 1,detectDates = T,check.names = T,na.strings = "") -> Latest_Nifty_50_candidates

if(exists("Latest_Nifty_50_candidates"))
{
  'Latest_Nifty_50_candidates' %>% paste0("Found '",.,"'",sep='') %>% print
  
  Latest_Nifty_50_candidates[1,-1] %>% unlist %>% as.character %>% as.numeric %>% 
    as.Date(origin="1899-12-30") %>% format("%Y%M%d") %>% as.numeric %>% 
    as.character %>% paste0("Included On ",.,sep='') %>% 
    c("Company",.) -> colnames(Latest_Nifty_50_candidates)
  
  which(Latest_Nifty_50_candidates$Company == "Company Name") %>% seq(1,.,1) %>% {-(.)} %>% 
    Latest_Nifty_50_candidates[.,] -> Latest_Nifty_50_candidates
  
  Latest_Nifty_50_candidates %>% colnames %>% {.[-1]} %>% strsplit(split = ' ') %>% 
    lapply(.,rev) %>% map(1) %>% unlist %>% as.character %>% as.numeric %>% sort %T>%
    {. ->> Latest_Nifty_50_candidates_dates} %>% min -> Min_Latest_Nifty_50_candidates
  
  'Latest_Nifty_50_candidates' %>% paste0("Reformatted '",.,"'",sep='') %>% print
}else{
  # Couldn't find 'Latest_Nifty_50_candidates'
  'Latest_Nifty_50_candidates' %>% paste0("Couldn't find '",.,"'",sep='') %>% print
} # End of 'if(exists("Latest_Nifty_50_candidates"))'

#################################################################################
#################################################################################

#################################################################################
####################### Making Survivorship Adjustments #########################

# If previous output doesn't exist, then make from start using 'Latest_Nifty_50_candidates'

if(exists("Latest_Nifty_50_candidates"))
{
  # Checking for previous output
  Survivorship_Adjustments_Database %>% setwd
  list.files() -> Survivorship_Adjustments_Database_files
  if (Output_filename %in% Survivorship_Adjustments_Database_files)
  {
    # Found existing Nifty 50 survivorship adjusted file
    # Update/overwrite over last two prowessIQ updates
    
    "Found existing Nifty 50 survivorship adjusted file" %>% print
    
    # Finding latest 2 expiries from 'Latest_Nifty_50_candidates'
    Latest_Nifty_50_candidates %>% colnames %>% {.[-1]} %>% strsplit(split=" ") %>% lapply(.,rev) %>% 
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
      
      # 'Latest_Nifty_50_candidates'
      which(Latest_Nifty_50_candidates_dates >= temp_date_YYYYMMDD) %>% 
        {if(length(.) == 0){return(length(Latest_Nifty_50_candidates_dates))}else{return(.)}} %>% 
        {. + 1} %>% min -> Temp_correct_col_Latest_Nifty_50_candidates
      
      Latest_Nifty_50_candidates[,c(1,Temp_correct_col_Latest_Nifty_50_candidates)] %>% 
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
    "NOT found existing Nifty 50 survivorship adjusted file" %>% print
    
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
      # 'Latest_Nifty_50_candidates'
      which(Latest_Nifty_50_candidates_dates >= temp_date_YYYYMMDD) %>% 
        {if(length(.) == 0){return(length(Latest_Nifty_50_candidates_dates))}else{return(.)}} %>% 
        {. + 1} %>% min -> Temp_correct_col_Latest_Nifty_50_candidates
      
      Latest_Nifty_50_candidates[,c(1,Temp_correct_col_Latest_Nifty_50_candidates)] %>% 
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
  # Couldn't find 'Latest_Nifty_50_candidates'
  'Latest_Nifty_50_candidates' %>% paste0("Couldn't find '",.,"'",sep='') %>% print
} # End of 'if(exists("Latest_Nifty_50_candidates"))'

#################################################################################
#################################################################################

#################################################################################
#################################### Goals ######################################

# Date: 2020-June-18
# Author: Arunabha Sarkar

# Goals: Survivor Adjustment Nifty 100
# File Name: Survivor_Adjustment_Nifty_100

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

Output_Col_Nums <- 101

Output_filename <- 'Survivor_Adjustment_Nifty_100_Tickers.csv'

"C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% paste0("/Nifty 50 Historical Data Investing dot com",
                            sep='') -> Nifty50_Database  # To find out which day is open & NIFTY Spot

Central_Database %>% paste0("/Survivorship Adjustments Indexes",sep='') -> Survivorship_Adjustments_Database  # To find out which day is open & NIFTY Spot

Central_Database %>% gsub("Central Database","",.) %>% 
  paste0(.,"Codes_Handling_Central_Database/Survivorship Adjustments Indexes/Raw Data Files",
         sep='') -> Survivorship_Adjustments_Raw_Database
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
######################## Finding Latest Nifty 50 List ###########################

Survivorship_Adjustments_Raw_Database %>% setwd

list.files() -> Survivorship_Adjustments_Raw_Database_Files

Survivorship_Adjustments_Raw_Database_Files %>% tolower %>% grep("nifty100",.) %>% 
  Survivorship_Adjustments_Raw_Database_Files[.] -> All_Nifty_100_candidates

All_Nifty_100_candidates %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>%
  as.Date(format="%Y%m%d") %>% which.max %>% All_Nifty_100_candidates[.] %>% 
  read.xlsx(sheet = 1,detectDates = T,check.names = T,na.strings = "") -> Latest_Nifty_100_candidates

if(exists("Latest_Nifty_100_candidates"))
{
  'Latest_Nifty_100_candidates' %>% paste0("Found '",.,"'",sep='') %>% print
  
  Latest_Nifty_100_candidates[1,-1] %>% unlist %>% as.character %>% as.numeric %>% 
    as.Date(origin="1899-12-30") %>% format("%Y%M%d") %>% as.numeric %>% 
    as.character %>% paste0("Included On ",.,sep='') %>% 
    c("Company",.) -> colnames(Latest_Nifty_100_candidates)
  
  which(Latest_Nifty_100_candidates$Company == "Company Name") %>% seq(1,.,1) %>% {-(.)} %>% 
    Latest_Nifty_100_candidates[.,] -> Latest_Nifty_100_candidates
  
  Latest_Nifty_100_candidates %>% colnames %>% {.[-1]} %>% strsplit(split = ' ') %>% 
    lapply(.,rev) %>% map(1) %>% unlist %>% as.character %>% as.numeric %>% sort %T>%
    {. ->> Latest_Nifty_100_candidates_dates} %>% min -> Min_Latest_Nifty_100_candidates
  
  'Latest_Nifty_100_candidates' %>% paste0("Reformatted '",.,"'",sep='') %>% print
}else{
  # Couldn't find 'Latest_Nifty_100_candidates'
  'Latest_Nifty_100_candidates' %>% paste0("Couldn't find '",.,"'",sep='') %>% print
} # End of 'if(exists("Latest_Nifty_100_candidates"))'

#################################################################################
#################################################################################

#################################################################################
####################### Making Survivorship Adjustments #########################

# If previous output doesn't exist, then make from start using 'Latest_Nifty_100_candidates'

if(exists("Latest_Nifty_100_candidates"))
{
  # Checking for previous output
  Survivorship_Adjustments_Database %>% setwd
  list.files() -> Survivorship_Adjustments_Database_files
  if (Output_filename %in% Survivorship_Adjustments_Database_files)
  {
    # Found existing Nifty 100 survivorship adjusted file
    # Update/overwrite over last two prowessIQ updates
    
    "Found existing Nifty 100 survivorship adjusted file" %>% print
    
    # Finding latest 2 expiries from 'Latest_Nifty_50_candidates'
    Latest_Nifty_100_candidates %>% colnames %>% {.[-1]} %>% strsplit(split=" ") %>% lapply(.,rev) %>% 
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
      
      # 'Latest_Nifty_100_candidates'
      which(Latest_Nifty_100_candidates_dates >= temp_date_YYYYMMDD) %>% 
        {if(length(.) == 0){return(length(Latest_Nifty_100_candidates_dates))}else{return(.)}} %>% 
        {. + 1} %>% min -> Temp_correct_col_Latest_Nifty_100_candidates
      
      Latest_Nifty_100_candidates[,c(1,Temp_correct_col_Latest_Nifty_100_candidates)] %>% 
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
    "NOT found existing Nifty 100 survivorship adjusted file" %>% print
    
    # Find min starting date of Nifty 100
    which(N50_Dates_YYYYMMDD >= Min_Latest_Nifty_100_candidates) %>% 
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
      # 'Latest_Nifty_100_candidates'
      which(Latest_Nifty_100_candidates_dates >= temp_date_YYYYMMDD) %>% 
        {if(length(.) == 0){return(length(Latest_Nifty_100_candidates_dates))}else{return(.)}} %>% 
        {. + 1} %>% min -> Temp_correct_col_Latest_Nifty_100_candidates
      
      Latest_Nifty_100_candidates[,c(1,Temp_correct_col_Latest_Nifty_100_candidates)] %>% 
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
  # Couldn't find 'Latest_Nifty_100_candidates'
  'Latest_Nifty_100_candidates' %>% paste0("Couldn't find '",.,"'",sep='') %>% print
} # End of 'if(exists("Latest_Nifty_100_candidates"))'

#################################################################################
#################################################################################

#################################################################################
#################################### Goals ######################################

# Date: 2020-June-18
# Author: Arunabha Sarkar

# Goals: Survivor Adjustment Nifty 200
# File Name: Survivor_Adjustment_Nifty_200

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

Output_Col_Nums <- 201

Output_filename <- 'Survivor_Adjustment_Nifty_200_Tickers.csv'

"C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% gsub("Central Database","",.) %>% 
  paste0(.,
         "Codes_Handling_Central_Database/Survivorship Adjustments Indexes/Till 2017 Index Construction",
         sep='') %>% paste0(.,"/Output",sep='') -> Till_2017_Output_Folder

Central_Database %>% paste0("/Nifty 50 Historical Data Investing dot com",
                            sep='') -> Nifty50_Database  # To find out which day is open & NIFTY Spot

Central_Database %>% paste0("/Survivorship Adjustments Indexes",sep='') -> Survivorship_Adjustments_Database

Central_Database %>% gsub("Central Database","",.) %>% 
  paste0(.,"Codes_Handling_Central_Database/Survivorship Adjustments Indexes/Raw Data Files",
         sep='') -> Survivorship_Adjustments_Raw_Database

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

names(Prowess_Company_Names) <- Prowess_Company_Tickers

names(Prowess_Company_Tickers) <- Prowess_Company_Names

#################################################################################
#################################################################################

#################################################################################
####################### Finding Latest Nifty 200 List ###########################

Survivorship_Adjustments_Raw_Database %>% setwd

list.files() -> Survivorship_Adjustments_Raw_Database_Files

Survivorship_Adjustments_Raw_Database_Files %>% tolower %>% grep("nifty200",.) %>% 
  Survivorship_Adjustments_Raw_Database_Files[.] -> All_Nifty_200_candidates

All_Nifty_200_candidates %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>%
  as.Date(format="%Y%m%d") %>% which.max %>% All_Nifty_200_candidates[.] %>% 
  read.xlsx(sheet = 1,detectDates = T,check.names = T,na.strings = "") -> Latest_Nifty_200_candidates

if(exists("Latest_Nifty_200_candidates"))
{
  'Latest_Nifty_200_candidates' %>% paste0("Found '",.,"'",sep='') %>% print
  
  Latest_Nifty_200_candidates[1,-1] %>% unlist %>% as.character %>% as.numeric %>% 
    as.Date(origin="1899-12-30") %>% format("%Y%M%d") %>% as.numeric %>% 
    as.character %>% paste0("Included On ",.,sep='') %>% 
    c("Company",.) -> colnames(Latest_Nifty_200_candidates)
  
  which(Latest_Nifty_200_candidates$Company == "Company Name") %>% seq(1,.,1) %>% {-(.)} %>% 
    Latest_Nifty_200_candidates[.,] -> Latest_Nifty_200_candidates
  
  Latest_Nifty_200_candidates %>% colnames %>% {.[-1]} %>% strsplit(split = ' ') %>% 
    lapply(.,rev) %>% map(1) %>% unlist %>% as.character %>% as.numeric %>% sort %T>%
    {. ->> Latest_Nifty_200_candidates_dates} %>% min -> Min_Latest_Nifty_200_candidates
  
  'Latest_Nifty_200_candidates' %>% paste0("Reformatted '",.,"'",sep='') %>% print
}else{
  # Couldn't find 'Latest_Nifty_200_candidates'
  'Latest_Nifty_200_candidates' %>% paste0("Couldn't find '",.,"'",sep='') %>% print
} # End of 'if(exists("Latest_Nifty_200_candidates"))'

#################################################################################
#################################################################################

#################################################################################
####################### Making Survivorship Adjustments #########################

# If previous output doesn't exist, then make from start using 'Latest_Nifty_200_candidates'

if(exists("Latest_Nifty_200_candidates"))
{
  # Checking for previous output
  Survivorship_Adjustments_Database %>% setwd
  list.files() -> Survivorship_Adjustments_Database_files
  if (Output_filename %in% Survivorship_Adjustments_Database_files)
  {
    # Found existing Nifty 200 survivorship adjusted file
    # Update/overwrite over last two prowessIQ updates
    
    "Found existing Nifty 200 survivorship adjusted file" %>% print
    
    # Finding latest 2 expiries from 'Latest_Nifty_50_candidates'
    Latest_Nifty_200_candidates %>% colnames %>% {.[-1]} %>% strsplit(split=" ") %>% lapply(.,rev) %>% 
      map(1) %>% unlist %>% as.character %>% as.numeric %>% sort %>% rev %>% .[2] -> Relevant_Min_Date
    
    Relevant_Min_Date %>% {which(N50_Dates_YYYYMMDD >= .)} %>% N50_Dates_YYYYMMDD[.] -> Relevant_Dates
    
    Survivorship_Adjustments_Database %>% setwd
    Output_filename %>% read.csv -> Old_file
    
    which(as.numeric(Old_file$Dates_YYYYMMDD) < Relevant_Min_Date) %>% Old_file[.,] -> temp_new
    
    temp_new %>% colnames %>% length %>% matrix(data = NA, nrow = length(Relevant_Dates), ncol = .) %>% 
      data.frame %>% `colnames<-`(colnames(temp_new)) -> temp_temp_new
    
    temp_temp_new$Dates_YYYYMMDD <- Relevant_Dates
    
    # i_1 <- 1; i_1 <- 361
    for(i_1 in 1:length(Relevant_Dates))
    {
      "Making company ticker list for row number " %>% paste0(i_1,sep='') %>% 
        paste0(" out of ", length(Relevant_Dates),sep='') %>% print
      
      i_1 %>% Relevant_Dates[.] %>% as.numeric -> temp_date_YYYYMMDD
      
      # 'Latest_Nifty_200_candidates'
      which(Latest_Nifty_200_candidates_dates >= temp_date_YYYYMMDD) %>% 
        {if(length(.) == 0){return(length(Latest_Nifty_200_candidates_dates))}else{return(.)}} %>% 
        {. + 1} %>% min -> Temp_correct_col_Latest_Nifty_200_candidates
      
      Latest_Nifty_200_candidates[,c(1,Temp_correct_col_Latest_Nifty_200_candidates)] %>% 
        na.omit %>% .[,1] %>% unlist %>% as.character -> temp_company_names
      
      temp_company_names %>% gsub(" [Merged]","",.,fixed=TRUE) -> temp_company_names
      
      # 'Prowess_Company_Names' & 'Prowess_Company_Tickers'
      Prowess_Company_Tickers[temp_company_names] %>% unlist %>% 
        as.character -> temp_company_tickers
      
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
    "NOT found existing Nifty 200 survivorship adjusted file" %>% print
    
    ############ Loading data till 2017
    Till_2017_Output_Folder %>% setwd
    list.files() -> Till_2017_Output_Folder_Files
    Till_2017_Output_Folder_Files %>% grep("Nifty_200_",.) %>% 
      Till_2017_Output_Folder_Files[.] %>% read.csv -> Existing_till_2017_data
    
    Existing_till_2017_data %>% select(Dates_YYYYMMDD) %>% unlist %>% 
      as.character %>% as.numeric %>% max -> Existing_till_2017_data_max
    
    # Find min starting date of Nifty 200
    which(N50_Dates_YYYYMMDD >= Min_Latest_Nifty_200_candidates) %>% 
      N50_Dates_YYYYMMDD[.] %>% as.numeric %>% as.character -> Relevant_Dates
    
    # Filtering 'Relevant_Dates' from dates in 'Existing_till_2017_data'
    Relevant_Dates %>% as.numeric %>% 
      {which(. > Existing_till_2017_data_max)} %>% Relevant_Dates[.] -> Relevant_Dates
    
    Relevant_Dates %>% length %>% matrix(data = NA, nrow = ., ncol = Output_Col_Nums) %>% 
      data.frame(stringsAsFactors = F) -> Survivorship_Adjustments_Company_Names_df
    
    Output_Col_Nums %>% {.-1} %>% seq(1,.,1) %>% paste0("Company_",.,sep='') %>% 
      c("Dates_YYYYMMDD",.) -> colnames(Survivorship_Adjustments_Company_Names_df)
    
    Survivorship_Adjustments_Company_Names_df$Dates_YYYYMMDD <- as.character(Relevant_Dates)
    
    ### Shifted to new file
    
    # i_1 <- 1; i_1 <- 607
    for(i_1 in 1:nrow(Survivorship_Adjustments_Company_Names_df))
    {
      # print(i_1)
      "Making company ticker list for row number " %>% paste0(i_1,sep='') %>% 
        paste0(" out of ", nrow(Survivorship_Adjustments_Company_Names_df),sep='') %>% print
      
      i_1 %>% Survivorship_Adjustments_Company_Names_df$Dates_YYYYMMDD[.] %>% as.numeric -> temp_date_YYYYMMDD
      # 'Latest_Nifty_200_candidates'
      which(Latest_Nifty_200_candidates_dates >= temp_date_YYYYMMDD) %>% 
        {if(length(.) == 0){return(length(Latest_Nifty_200_candidates_dates))}else{return(.)}} %>% 
        {. + 1} %>% min -> Temp_correct_col_Latest_Nifty_200_candidates
      
      Latest_Nifty_200_candidates[,c(1,Temp_correct_col_Latest_Nifty_200_candidates)] %>% 
        na.omit %>% .[,1] %>% unlist %>% as.character %>% gsub(" [Merged]","",.,
                                                               fixed=TRUE) -> temp_company_names
      
      # 'Prowess_Company_Names' & 'Prowess_Company_Tickers'
      Prowess_Company_Tickers[temp_company_names] %>% unlist %>% 
        as.character -> temp_company_tickers
      
      temp_company_tickers %>% 
        c(.,rep(NA,(Output_Col_Nums-1-length(temp_company_tickers)))) -> Survivorship_Adjustments_Company_Names_df[i_1,-1]
    } # End of 'for(i_1 in 1:nrow(Survivorship_Adjustments_Company_Names_df))'
    
    # Save the file
    if(nrow(Existing_till_2017_data) > 0)
    {
      Existing_till_2017_data %>% 
        rbind(.,Survivorship_Adjustments_Company_Names_df) -> Survivorship_Adjustments_Company_Names_df
    } # End of 'if(nrow(Existing_till_2017_data) > 0)'
    
    print(paste0("Making new '",Output_filename,"'",sep=''))
    Survivorship_Adjustments_Database %>% setwd
    Survivorship_Adjustments_Company_Names_df %>% 
      write.csv(.,file=Output_filename,na = "",row.names = FALSE)
    
  } # End of 'if (Output_filename %in% Survivorship_Adjustments_Database_files)'
}else{
  # Couldn't find 'Latest_Nifty_200_candidates'
  'Latest_Nifty_200_candidates' %>% paste0("Couldn't find '",.,"'",sep='') %>% print
} # End of 'if(exists("Latest_Nifty_200_candidates"))'

#################################################################################
#################################################################################

#################################################################################
#################################### Goals ######################################

# Date: 2020-June-18
# Author: Arunabha Sarkar

# Goals: Survivor Adjustment Nifty Midcap 100
# File Name: Survivor_Adjustment_Nifty_Midcap_100

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

Output_Col_Nums <- 101

Output_filename <- 'Survivor_Adjustment_Nifty_Midcap_100_Tickers.csv'

"C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% gsub("Central Database","",.) %>% 
  paste0(.,
         "Codes_Handling_Central_Database/Survivorship Adjustments Indexes/Till 2017 Index Construction",
         sep='') %>% paste0(.,"/Output",sep='') -> Till_2017_Output_Folder

Central_Database %>% paste0("/Nifty 50 Historical Data Investing dot com",
                            sep='') -> Nifty50_Database  # To find out which day is open & NIFTY Spot

Central_Database %>% paste0("/Survivorship Adjustments Indexes",sep='') -> Survivorship_Adjustments_Database

Central_Database %>% gsub("Central Database","",.) %>% 
  paste0(.,"Codes_Handling_Central_Database/Survivorship Adjustments Indexes/Raw Data Files",
         sep='') -> Survivorship_Adjustments_Raw_Database

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

names(Prowess_Company_Names) <- Prowess_Company_Tickers

names(Prowess_Company_Tickers) <- Prowess_Company_Names

#################################################################################
#################################################################################

#################################################################################
##################### Finding Latest Nifty Midcap 100 List #######################

Survivorship_Adjustments_Raw_Database %>% setwd

list.files() -> Survivorship_Adjustments_Raw_Database_Files

Survivorship_Adjustments_Raw_Database_Files %>% tolower %>% grep("niftymidcap100",.) %>% 
  Survivorship_Adjustments_Raw_Database_Files[.] -> All_Index_candidates

All_Index_candidates %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>%
  as.Date(format="%Y%m%d") %>% which.max %>% All_Index_candidates[.] %>% 
  read.xlsx(sheet = 1,detectDates = T,check.names = T,na.strings = "") -> Latest_All_Index_candidates

if(exists("Latest_All_Index_candidates"))
{
  'Latest_All_Index_candidates' %>% paste0("Found '",.,"'",sep='') %>% print
  
  Latest_All_Index_candidates[1,-1] %>% unlist %>% as.character %>% as.numeric %>% 
    as.Date(origin="1899-12-30") %>% format("%Y%M%d") %>% as.numeric %>% 
    as.character %>% paste0("Included On ",.,sep='') %>% 
    c("Company",.) -> colnames(Latest_All_Index_candidates)
  
  which(Latest_All_Index_candidates$Company == "Company Name") %>% seq(1,.,1) %>% {-(.)} %>% 
    Latest_All_Index_candidates[.,] -> Latest_All_Index_candidates
  
  Latest_All_Index_candidates %>% colnames %>% {.[-1]} %>% strsplit(split = ' ') %>% 
    lapply(.,rev) %>% map(1) %>% unlist %>% as.character %>% as.numeric %>% sort %T>%
    {. ->> Latest_All_Index_candidates_dates} %>% min -> Min_Latest_All_Index_candidates
  
  'Latest_All_Index_candidates' %>% paste0("Reformatted '",.,"'",sep='') %>% print
}else{
  # Couldn't find 'Latest_All_Index_candidates'
  'Latest_All_Index_candidates' %>% paste0("Couldn't find '",.,"'",sep='') %>% print
} # End of 'if(exists("Latest_All_Index_candidates"))'

#################################################################################
#################################################################################

#################################################################################
####################### Making Survivorship Adjustments #########################

# If previous output doesn't exist, then make from start using 'Latest_All_Index_candidates'

if(exists("Latest_All_Index_candidates"))
{
  # Checking for previous output
  Survivorship_Adjustments_Database %>% setwd
  list.files() -> Survivorship_Adjustments_Database_files
  if (Output_filename %in% Survivorship_Adjustments_Database_files)
  {
    # Found existing Nifty Midcap 100 survivorship adjusted file
    # Update/overwrite over last two prowessIQ updates
    
    "Found existing Nifty Midcap 100 survivorship adjusted file" %>% print
    
    # Finding latest 2 expiries from 'Latest_Nifty_50_candidates'
    Latest_All_Index_candidates %>% colnames %>% {.[-1]} %>% strsplit(split=" ") %>% lapply(.,rev) %>% 
      map(1) %>% unlist %>% as.character %>% as.numeric %>% sort %>% rev %>% .[2] -> Relevant_Min_Date
    
    Relevant_Min_Date %>% {which(N50_Dates_YYYYMMDD >= .)} %>% N50_Dates_YYYYMMDD[.] -> Relevant_Dates
    
    Survivorship_Adjustments_Database %>% setwd
    Output_filename %>% read.csv -> Old_file
    
    which(as.numeric(Old_file$Dates_YYYYMMDD) < Relevant_Min_Date) %>% Old_file[.,] -> temp_new
    
    temp_new %>% colnames %>% length %>% matrix(data = NA, nrow = length(Relevant_Dates), ncol = .) %>% 
      data.frame %>% `colnames<-`(colnames(temp_new)) -> temp_temp_new
    
    temp_temp_new$Dates_YYYYMMDD <- Relevant_Dates
    
    # i_1 <- 1; i_1 <- 361
    for(i_1 in 1:length(Relevant_Dates))
    {
      "Making company ticker list for row number " %>% paste0(i_1,sep='') %>% 
        paste0(" out of ", length(Relevant_Dates),sep='') %>% print
      
      i_1 %>% Relevant_Dates[.] %>% as.numeric -> temp_date_YYYYMMDD
      
      # 'Latest_All_Index_candidates'
      which(Latest_All_Index_candidates_dates >= temp_date_YYYYMMDD) %>% 
        {if(length(.) == 0){return(length(Latest_All_Index_candidates_dates))}else{return(.)}} %>% 
        {. + 1} %>% min -> Temp_correct_col_Latest_Nifty_All_Index_candidates
      
      Latest_All_Index_candidates[,c(1,Temp_correct_col_Latest_Nifty_All_Index_candidates)] %>% 
        na.omit %>% .[,1] %>% unlist %>% as.character -> temp_company_names
      
      temp_company_names %>% gsub(" [Merged]","",.,fixed=TRUE) -> temp_company_names
      
      # 'Prowess_Company_Names' & 'Prowess_Company_Tickers'
      Prowess_Company_Tickers[temp_company_names] %>% unlist %>% 
        as.character -> temp_company_tickers
      
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
    "NOT found existing Nifty Midcap 100 survivorship adjusted file" %>% print
    
    ############ Loading data till 2017
    Till_2017_Output_Folder %>% setwd
    list.files() -> Till_2017_Output_Folder_Files
    Till_2017_Output_Folder_Files %>% grep("Nifty_Midcap_100_",.) %>% 
      Till_2017_Output_Folder_Files[.] %>% read.csv -> Existing_till_2017_data
    
    Existing_till_2017_data %>% select(Dates_YYYYMMDD) %>% unlist %>% 
      as.character %>% as.numeric %>% max -> Existing_till_2017_data_max
    
    # Find min starting date of Nifty Midcap 50 # Min_Latest_All_Index_candidates
    which(N50_Dates_YYYYMMDD >= Min_Latest_All_Index_candidates) %>% 
      N50_Dates_YYYYMMDD[.] %>% as.numeric %>% as.character -> Relevant_Dates
    
    # Filtering 'Relevant_Dates' from dates in 'Existing_till_2017_data'
    Relevant_Dates %>% as.numeric %>% 
      {which(. > Existing_till_2017_data_max)} %>% Relevant_Dates[.] -> Relevant_Dates
    
    Relevant_Dates %>% length %>% matrix(data = NA, nrow = ., ncol = Output_Col_Nums) %>% 
      data.frame(stringsAsFactors = F) -> Survivorship_Adjustments_Company_Names_df
    
    Output_Col_Nums %>% {.-1} %>% seq(1,.,1) %>% paste0("Company_",.,sep='') %>% 
      c("Dates_YYYYMMDD",.) -> colnames(Survivorship_Adjustments_Company_Names_df)
    
    Survivorship_Adjustments_Company_Names_df$Dates_YYYYMMDD <- as.character(Relevant_Dates)
    
    # i_1 <- 1; i_1 <- 250; i_1 <- 607
    for(i_1 in 1:nrow(Survivorship_Adjustments_Company_Names_df))
    {
      # print(i_1)
      "Making company ticker list for row number " %>% paste0(i_1,sep='') %>% 
        paste0(" out of ", nrow(Survivorship_Adjustments_Company_Names_df),sep='') %>% print
      
      i_1 %>% Survivorship_Adjustments_Company_Names_df$Dates_YYYYMMDD[.] %>% as.numeric -> temp_date_YYYYMMDD
      # 'Latest_Nifty_All_Index_candidates'
      which(Latest_All_Index_candidates_dates >= temp_date_YYYYMMDD) %>% 
        {if(length(.) == 0){return(length(Latest_All_Index_candidates_dates))}else{return(.)}} %>% 
        {. + 1} %>% min -> Temp_correct_col_Latest_Index_candidates
      
      Latest_All_Index_candidates[,c(1,Temp_correct_col_Latest_Index_candidates)] %>% 
        na.omit %>% .[,1] %>% unlist %>% as.character %>% 
        gsub(" [Merged]","",., fixed=TRUE) -> temp_company_names
      
      # 'Prowess_Company_Names' & 'Prowess_Company_Tickers'
      Prowess_Company_Tickers[temp_company_names] %>% unlist %>% 
        as.character -> temp_company_tickers
      
      temp_company_tickers %>% 
        c(.,rep(NA,(Output_Col_Nums-1-length(temp_company_tickers)))) -> Survivorship_Adjustments_Company_Names_df[i_1,-1]
    } # End of 'for(i_1 in 1:nrow(Survivorship_Adjustments_Company_Names_df))'
    
    # Save the file
    if(nrow(Existing_till_2017_data) > 0)
    {
      Existing_till_2017_data %>% 
        rbind(.,Survivorship_Adjustments_Company_Names_df) -> Survivorship_Adjustments_Company_Names_df
    } # End of 'if(nrow(Existing_till_2017_data) > 0)'
    
    print(paste0("Making new '",Output_filename,"'",sep=''))
    Survivorship_Adjustments_Database %>% setwd
    Survivorship_Adjustments_Company_Names_df %>% 
      write.csv(.,file=Output_filename,na = "",row.names = FALSE)
    
  } # End of 'if (Output_filename %in% Survivorship_Adjustments_Database_files)'
}else{
  # Couldn't find 'Latest_All_Index_candidates'
  'Latest_All_Index_candidates' %>% paste0("Couldn't find '",.,"'",sep='') %>% print
} # End of 'if(exists("Latest_All_Index_candidates"))'

#################################################################################
#################################################################################

#################################################################################
#################################### Goals ######################################

# Date: 2020-June-18
# Author: Arunabha Sarkar

# Goals: Survivor Adjustment Nifty Midcap 50
# File Name: Survivor_Adjustment_Nifty_Midcap_50

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

Output_filename <- 'Survivor_Adjustment_Nifty_Midcap_50_Tickers.csv'

"C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% gsub("Central Database","",.) %>% 
  paste0(.,
         "Codes_Handling_Central_Database/Survivorship Adjustments Indexes/Till 2017 Index Construction",
         sep='') %>% paste0(.,"/Output",sep='') -> Till_2017_Output_Folder

Central_Database %>% paste0("/Nifty 50 Historical Data Investing dot com",
                            sep='') -> Nifty50_Database  # To find out which day is open & NIFTY Spot

Central_Database %>% paste0("/Survivorship Adjustments Indexes",sep='') -> Survivorship_Adjustments_Database

Central_Database %>% gsub("Central Database","",.) %>% 
  paste0(.,"Codes_Handling_Central_Database/Survivorship Adjustments Indexes/Raw Data Files",
         sep='') -> Survivorship_Adjustments_Raw_Database

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

names(Prowess_Company_Names) <- Prowess_Company_Tickers

names(Prowess_Company_Tickers) <- Prowess_Company_Names

#################################################################################
#################################################################################

#################################################################################
##################### Finding Latest Nifty Midcap 50 List #######################

Survivorship_Adjustments_Raw_Database %>% setwd

list.files() -> Survivorship_Adjustments_Raw_Database_Files

Survivorship_Adjustments_Raw_Database_Files %>% tolower %>% grep("niftymidcap50",.) %>% 
  Survivorship_Adjustments_Raw_Database_Files[.] -> All_Index_candidates

All_Index_candidates %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>%
  as.Date(format="%Y%m%d") %>% which.max %>% All_Index_candidates[.] %>% 
  read.xlsx(sheet = 1,detectDates = T,check.names = T,na.strings = "") -> Latest_All_Index_candidates

if(exists("Latest_All_Index_candidates"))
{
  'Latest_All_Index_candidates' %>% paste0("Found '",.,"'",sep='') %>% print
  
  Latest_All_Index_candidates[1,-1] %>% unlist %>% as.character %>% as.numeric %>% 
    as.Date(origin="1899-12-30") %>% format("%Y%M%d") %>% as.numeric %>% 
    as.character %>% paste0("Included On ",.,sep='') %>% 
    c("Company",.) -> colnames(Latest_All_Index_candidates)
  
  which(Latest_All_Index_candidates$Company == "Company Name") %>% seq(1,.,1) %>% {-(.)} %>% 
    Latest_All_Index_candidates[.,] -> Latest_All_Index_candidates
  
  Latest_All_Index_candidates %>% colnames %>% {.[-1]} %>% strsplit(split = ' ') %>% 
    lapply(.,rev) %>% map(1) %>% unlist %>% as.character %>% as.numeric %>% sort %T>%
    {. ->> Latest_All_Index_candidates_dates} %>% min -> Min_Latest_All_Index_candidates
  
  'Latest_All_Index_candidates' %>% paste0("Reformatted '",.,"'",sep='') %>% print
}else{
  # Couldn't find 'Latest_All_Index_candidates'
  'Latest_All_Index_candidates' %>% paste0("Couldn't find '",.,"'",sep='') %>% print
} # End of 'if(exists("Latest_All_Index_candidates"))'

#################################################################################
#################################################################################

#################################################################################
####################### Making Survivorship Adjustments #########################

# If previous output doesn't exist, then make from start using 'Latest_All_Index_candidates'

if(exists("Latest_All_Index_candidates"))
{
  # Checking for previous output
  Survivorship_Adjustments_Database %>% setwd
  list.files() -> Survivorship_Adjustments_Database_files
  if (Output_filename %in% Survivorship_Adjustments_Database_files)
  {
    # Found existing Nifty Midcap 50 survivorship adjusted file
    # Update/overwrite over last two prowessIQ updates
    
    "Found existing Nifty Midcap 50 survivorship adjusted file" %>% print
    
    # Finding latest 2 expiries from 'Latest_Nifty_50_candidates'
    Latest_All_Index_candidates %>% colnames %>% {.[-1]} %>% strsplit(split=" ") %>% lapply(.,rev) %>% 
      map(1) %>% unlist %>% as.character %>% as.numeric %>% sort %>% rev %>% .[2] -> Relevant_Min_Date
    
    Relevant_Min_Date %>% {which(N50_Dates_YYYYMMDD >= .)} %>% N50_Dates_YYYYMMDD[.] -> Relevant_Dates
    
    Survivorship_Adjustments_Database %>% setwd
    Output_filename %>% read.csv -> Old_file
    
    which(as.numeric(Old_file$Dates_YYYYMMDD) < Relevant_Min_Date) %>% Old_file[.,] -> temp_new
    
    temp_new %>% colnames %>% length %>% matrix(data = NA, nrow = length(Relevant_Dates), ncol = .) %>% 
      data.frame %>% `colnames<-`(colnames(temp_new)) -> temp_temp_new
    
    temp_temp_new$Dates_YYYYMMDD <- Relevant_Dates
    
    # i_1 <- 1; i_1 <- 361
    for(i_1 in 1:length(Relevant_Dates))
    {
      "Making company ticker list for row number " %>% paste0(i_1,sep='') %>% 
        paste0(" out of ", length(Relevant_Dates),sep='') %>% print
      
      i_1 %>% Relevant_Dates[.] %>% as.numeric -> temp_date_YYYYMMDD
      
      # 'Latest_All_Index_candidates'
      which(Latest_All_Index_candidates_dates >= temp_date_YYYYMMDD) %>% 
        {if(length(.) == 0){return(length(Latest_All_Index_candidates_dates))}else{return(.)}} %>% 
        {. + 1} %>% min -> Temp_correct_col_Latest_Nifty_All_Index_candidates
      
      Latest_All_Index_candidates[,c(1,Temp_correct_col_Latest_Nifty_All_Index_candidates)] %>% 
        na.omit %>% .[,1] %>% unlist %>% as.character -> temp_company_names
      
      temp_company_names %>% gsub(" [Merged]","",.,fixed=TRUE) -> temp_company_names
      
      # 'Prowess_Company_Names' & 'Prowess_Company_Tickers'
      Prowess_Company_Tickers[temp_company_names] %>% unlist %>% 
        as.character -> temp_company_tickers
      
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
    "NOT found existing Nifty Midcap 50 survivorship adjusted file" %>% print
    
    ############ Loading data till 2017
    Till_2017_Output_Folder %>% setwd
    list.files() -> Till_2017_Output_Folder_Files
    Till_2017_Output_Folder_Files %>% grep("Nifty_Midcap_50_",.) %>% 
      Till_2017_Output_Folder_Files[.] %>% read.csv -> Existing_till_2017_data
    
    Existing_till_2017_data %>% select(Dates_YYYYMMDD) %>% unlist %>% 
      as.character %>% as.numeric %>% max -> Existing_till_2017_data_max
    
    # Find min starting date of Nifty Midcap 50 # Min_Latest_All_Index_candidates
    which(N50_Dates_YYYYMMDD >= Min_Latest_All_Index_candidates) %>% 
      N50_Dates_YYYYMMDD[.] %>% as.numeric %>% as.character -> Relevant_Dates
    
    # Filtering 'Relevant_Dates' from dates in 'Existing_till_2017_data'
    Relevant_Dates %>% as.numeric %>% 
      {which(. > Existing_till_2017_data_max)} %>% Relevant_Dates[.] -> Relevant_Dates
    
    Relevant_Dates %>% length %>% matrix(data = NA, nrow = ., ncol = Output_Col_Nums) %>% 
      data.frame(stringsAsFactors = F) -> Survivorship_Adjustments_Company_Names_df
    
    Output_Col_Nums %>% {.-1} %>% seq(1,.,1) %>% paste0("Company_",.,sep='') %>% 
      c("Dates_YYYYMMDD",.) -> colnames(Survivorship_Adjustments_Company_Names_df)
    
    Survivorship_Adjustments_Company_Names_df$Dates_YYYYMMDD <- as.character(Relevant_Dates)
    
    # i_1 <- 1; i_1 <- 607
    for(i_1 in 1:nrow(Survivorship_Adjustments_Company_Names_df))
    {
      # print(i_1)
      "Making company ticker list for row number " %>% paste0(i_1,sep='') %>% 
        paste0(" out of ", nrow(Survivorship_Adjustments_Company_Names_df),sep='') %>% print
      
      i_1 %>% Survivorship_Adjustments_Company_Names_df$Dates_YYYYMMDD[.] %>% as.numeric -> temp_date_YYYYMMDD
      # 'Latest_Nifty_All_Index_candidates'
      which(Latest_All_Index_candidates_dates >= temp_date_YYYYMMDD) %>% 
        {if(length(.) == 0){return(length(Latest_All_Index_candidates_dates))}else{return(.)}} %>% 
        {. + 1} %>% min -> Temp_correct_col_Latest_Index_candidates
      
      Latest_All_Index_candidates[,c(1,Temp_correct_col_Latest_Index_candidates)] %>% 
        na.omit %>% .[,1] %>% unlist %>% as.character %>% 
        gsub(" [Merged]","",., fixed=TRUE) -> temp_company_names
      
      # 'Prowess_Company_Names' & 'Prowess_Company_Tickers'
      Prowess_Company_Tickers[temp_company_names] %>% unlist %>% 
        as.character -> temp_company_tickers
      
      temp_company_tickers %>% 
        c(.,rep(NA,(Output_Col_Nums-1-length(temp_company_tickers)))) -> Survivorship_Adjustments_Company_Names_df[i_1,-1]
    } # End of 'for(i_1 in 1:nrow(Survivorship_Adjustments_Company_Names_df))'
    
    # Save the file
    if(nrow(Existing_till_2017_data) > 0)
    {
      Existing_till_2017_data %>% 
        rbind(.,Survivorship_Adjustments_Company_Names_df) -> Survivorship_Adjustments_Company_Names_df
    } # End of 'if(nrow(Existing_till_2017_data) > 0)'
    
    print(paste0("Making new '",Output_filename,"'",sep=''))
    Survivorship_Adjustments_Database %>% setwd
    Survivorship_Adjustments_Company_Names_df %>% 
      write.csv(.,file=Output_filename,na = "",row.names = FALSE)
    
  } # End of 'if (Output_filename %in% Survivorship_Adjustments_Database_files)'
}else{
  # Couldn't find 'Latest_All_Index_candidates'
  'Latest_All_Index_candidates' %>% paste0("Couldn't find '",.,"'",sep='') %>% print
} # End of 'if(exists("Latest_All_Index_candidates"))'

#################################################################################
#################################################################################

#################################################################################
#################################### Goals ######################################

# Date: 2020-June-18
# Author: Arunabha Sarkar

# Goals: Survivor Adjustment Nifty Next 50
# File Name: Survivor_Adjustment_Nifty_Next_50

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

Output_filename <- 'Survivor_Adjustment_Nifty_Next_50_Tickers.csv'

"C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% paste0("/Nifty 50 Historical Data Investing dot com",
                            sep='') -> Nifty50_Database  # To find out which day is open & NIFTY Spot

Central_Database %>% paste0("/Survivorship Adjustments Indexes",sep='') -> Survivorship_Adjustments_Database  # To find out which day is open & NIFTY Spot

Central_Database %>% gsub("Central Database","",.) %>% 
  paste0(.,"Codes_Handling_Central_Database/Survivorship Adjustments Indexes/Raw Data Files",
         sep='') -> Survivorship_Adjustments_Raw_Database

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
######################## Finding Latest Nifty 50 List ###########################

Survivorship_Adjustments_Raw_Database %>% setwd

list.files() -> Survivorship_Adjustments_Raw_Database_Files

Survivorship_Adjustments_Raw_Database_Files %>% tolower %>% grep("niftynext50",.) %>% 
  Survivorship_Adjustments_Raw_Database_Files[.] -> All_Nifty_Next_50_candidates

All_Nifty_Next_50_candidates %>% strsplit(.,split = ' ') %>% map(1) %>% unlist %>%
  as.Date(format="%Y%m%d") %>% which.max %>% All_Nifty_Next_50_candidates[.] %>% 
  read.xlsx(sheet = 1,detectDates = T,check.names = T,na.strings = "") -> Latest_Nifty_Next_50_candidates

if(exists("Latest_Nifty_Next_50_candidates"))
{
  'Latest_Nifty_Next_50_candidates' %>% paste0("Found '",.,"'",sep='') %>% print
  
  Latest_Nifty_Next_50_candidates[1,-1] %>% unlist %>% as.character %>% as.numeric %>% 
    as.Date(origin="1899-12-30") %>% format("%Y%M%d") %>% as.numeric %>% 
    as.character %>% paste0("Included On ",.,sep='') %>% 
    c("Company",.) -> colnames(Latest_Nifty_Next_50_candidates)
  
  which(Latest_Nifty_Next_50_candidates$Company == "Company Name") %>% seq(1,.,1) %>% {-(.)} %>% 
    Latest_Nifty_Next_50_candidates[.,] -> Latest_Nifty_Next_50_candidates
  
  Latest_Nifty_Next_50_candidates %>% colnames %>% {.[-1]} %>% strsplit(split = ' ') %>% 
    lapply(.,rev) %>% map(1) %>% unlist %>% as.character %>% as.numeric %>% sort %T>%
    {. ->> Latest_Nifty_Next_50_candidates_dates} %>% min -> Min_Latest_Nifty_Next_50_candidates
  
  'Latest_Nifty_Next_50_candidates' %>% paste0("Reformatted '",.,"'",sep='') %>% print
}else{
  # Couldn't find 'Latest_Nifty_Next_50_candidates'
  'Latest_Nifty_Next_50_candidates' %>% paste0("Couldn't find '",.,"'",sep='') %>% print
} # End of 'if(exists("Latest_Nifty_Next_50_candidates"))'

#################################################################################
#################################################################################

#################################################################################
####################### Making Survivorship Adjustments #########################

# If previous output doesn't exist, then make from start using 'Latest_Nifty_Next_50_candidates'

if(exists("Latest_Nifty_Next_50_candidates"))
{
  # Checking for previous output
  Survivorship_Adjustments_Database %>% setwd
  list.files() -> Survivorship_Adjustments_Database_files
  if (Output_filename %in% Survivorship_Adjustments_Database_files)
  {
    # Found existing Nifty Next 50 survivorship adjusted file
    # Update/overwrite over last two prowessIQ updates
    
    "Found existing Nifty Next 50 survivorship adjusted file" %>% print
    
    # Finding latest 2 expiries from 'Latest_Nifty_Next_50_candidates'
    Latest_Nifty_Next_50_candidates %>% colnames %>% {.[-1]} %>% strsplit(split=" ") %>% lapply(.,rev) %>% 
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
      
      # 'Latest_Nifty_Next_50_candidates'
      which(Latest_Nifty_Next_50_candidates_dates >= temp_date_YYYYMMDD) %>% 
        {if(length(.) == 0){return(length(Latest_Nifty_Next_50_candidates_dates))}else{return(.)}} %>% 
        {. + 1} %>% min -> Temp_correct_col_Latest_Nifty_Next_50_candidates
      
      Latest_Nifty_Next_50_candidates[,c(1,Temp_correct_col_Latest_Nifty_Next_50_candidates)] %>% 
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
    "NOT found existing Nifty Next 50 survivorship adjusted file" %>% print
    
    # Find min starting date of Nifty Next 50
    which(N50_Dates_YYYYMMDD >= Min_Latest_Nifty_Next_50_candidates) %>% 
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
      # 'Latest_Nifty_Next_50_candidates'
      which(Latest_Nifty_Next_50_candidates_dates >= temp_date_YYYYMMDD) %>% 
        {if(length(.) == 0){return(length(Latest_Nifty_Next_50_candidates_dates))}else{return(.)}} %>% 
        {. + 1} %>% min -> Temp_correct_col_Latest_Nifty_Next_50_candidates
      
      Latest_Nifty_Next_50_candidates[,c(1,Temp_correct_col_Latest_Nifty_Next_50_candidates)] %>% 
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
  # Couldn't find 'Latest_Nifty_Next_50_candidates'
  'Latest_Nifty_Next_50_candidates' %>% paste0("Couldn't find '",.,"'",sep='') %>% print
} # End of 'if(exists("Latest_Nifty_Next_50_candidates"))'

#################################################################################
#################################################################################

#################################################################################
#################################### Goals ######################################

# Date: 2020-June-18
# Author: Arunabha Sarkar

# Goals: Survivor Adjustment Nifty Midcap Next 50
# File Name: Survivor_Adjustment_Nifty_Midcap_Next_50

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

Output_filename <- 'Survivor_Adjustment_Nifty_Midcap_Next_50_Tickers.csv'

"C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% gsub("Central Database","",.) %>% 
  paste0(.,
         "Codes_Handling_Central_Database/Survivorship Adjustments Indexes/Till 2017 Index Construction",
         sep='') %>% paste0(.,"/Output",sep='') -> Till_2017_Output_Folder

Central_Database %>% paste0("/Nifty 50 Historical Data Investing dot com",
                            sep='') -> Nifty50_Database  # To find out which day is open & NIFTY Spot

Central_Database %>% paste0("/Survivorship Adjustments Indexes",sep='') -> Survivorship_Adjustments_Database

Central_Database %>% gsub("Central Database","",.) %>% 
  paste0(.,"Codes_Handling_Central_Database/Survivorship Adjustments Indexes/Raw Data Files",
         sep='') -> Survivorship_Adjustments_Raw_Database

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

names(Prowess_Company_Names) <- Prowess_Company_Tickers

names(Prowess_Company_Tickers) <- Prowess_Company_Names

#################################################################################
#################################################################################

#################################################################################
################# Finding NM50 and NM100 raw re-balance files ###################

Survivorship_Adjustments_Database %>% setwd

list.files() -> Survivorship_Adjustments_Database_Files

Survivorship_Adjustments_Database_Files %>% tolower %>% grep("nifty_midcap_100",.) %>% 
  Survivorship_Adjustments_Database_Files[.] %>% read.csv -> Existing_Nifty_Midcap_100

Survivorship_Adjustments_Database_Files %>% tolower %>% grep("nifty_midcap_50",.) %>% 
  Survivorship_Adjustments_Database_Files[.] %>% read.csv -> Existing_Nifty_Midcap_50

#################################################################################
#################################################################################

#################################################################################
####################### Making Survivorship Adjustments #########################

# If previous output doesn't exist, then make from start using 'Latest_All_Index_candidates'

# Checking for previous output
Survivorship_Adjustments_Database %>% setwd
list.files() -> Survivorship_Adjustments_Database_files
if (Output_filename %in% Survivorship_Adjustments_Database_files)
{
  # Found existing Nifty Midcap 50 survivorship adjusted file
  # Update/overwrite over last two prowessIQ updates
  
  "Found existing Nifty Midcap Next 50 survivorship adjusted file" %>% print
  
  Survivorship_Adjustments_Database %>% setwd
  Output_filename %>% read.csv -> Old_file
  
  # Deleting last 200 entries from Old_file
  Old_file %>% nrow %>% {.-200} %>% seq(1,.,1) %>% Old_file[.,] -> Old_file
  
  {which(as.numeric(N50_Dates_YYYYMMDD) > max(as.numeric(Old_file$Dates_YYYYMMDD)))} %>% 
    N50_Dates_YYYYMMDD[.] -> Relevant_Dates # This construct will get all the new dates as well
  
  Old_file %>% colnames %>% length %>% matrix(data = NA, nrow = length(Relevant_Dates), ncol = .) %>% 
    data.frame %>% `colnames<-`(colnames(Old_file)) -> temp_temp_new
  
  temp_temp_new$Dates_YYYYMMDD <- Relevant_Dates
  
  Relevant_Dates %>% length %>% matrix(data = NA, nrow = ., ncol = Output_Col_Nums) %>% 
    data.frame(stringsAsFactors = F) -> Survivorship_Adjustments_Company_Names_df
  
  Output_Col_Nums %>% {.-1} %>% seq(1,.,1) %>% paste0("Company_",.,sep='') %>% 
    c("Dates_YYYYMMDD",.) -> colnames(Survivorship_Adjustments_Company_Names_df)
  
  Survivorship_Adjustments_Company_Names_df$Dates_YYYYMMDD <- as.character(Relevant_Dates)
  
  # i_1 <- 1; i_1 <- 607
  for(i_1 in 1:nrow(Survivorship_Adjustments_Company_Names_df))
  {
    # print(i_1)
    "Making company ticker list for row number " %>% paste0(i_1,sep='') %>% 
      paste0(" out of ", nrow(Survivorship_Adjustments_Company_Names_df),sep='') %>% print
    
    i_1 %>% Survivorship_Adjustments_Company_Names_df$Dates_YYYYMMDD[.] %>% as.numeric -> temp_date_YYYYMMDD
    
    ## Executing NM100 minus NM50
    # Existing_Nifty_Midcap_50 # Existing_Nifty_Midcap_100
    
    Existing_Nifty_Midcap_100 %>% select(Dates_YYYYMMDD) %>% unlist %>% as.character -> Existing_Nifty_Midcap_100_Dates
    which(Existing_Nifty_Midcap_100_Dates == temp_date_YYYYMMDD) %>% 
      Existing_Nifty_Midcap_100[.,-1] %>% unlist %>% as.character -> temp_Existing_Nifty_Midcap_100
    
    Existing_Nifty_Midcap_50 %>% select(Dates_YYYYMMDD) %>% unlist %>% as.character -> Existing_Nifty_Midcap_50_Dates
    which(Existing_Nifty_Midcap_50_Dates == temp_date_YYYYMMDD) %>% 
      Existing_Nifty_Midcap_50[.,-1] %>% unlist %>% as.character -> temp_Existing_Nifty_Midcap_50
    
    (temp_Existing_Nifty_Midcap_100 %in% temp_Existing_Nifty_Midcap_50) %>% {!.} %>% 
      temp_Existing_Nifty_Midcap_100[.] %>% sort -> temp_company_tickers
    
    temp_company_tickers %>% 
      c(.,rep(NA,(Output_Col_Nums-1-length(temp_company_tickers)))) -> Survivorship_Adjustments_Company_Names_df[i_1,-1]
  } # End of 'for(i_1 in 1:nrow(Survivorship_Adjustments_Company_Names_df))'
  
  Old_file %>% rbind(.,Survivorship_Adjustments_Company_Names_df) -> temp_new
  
  # Save the file, delete the old as well
  Survivorship_Adjustments_Database %>% setwd
  print(paste0("Overwriting '",Output_filename,"'",sep=''))
  file.remove(Output_filename)
  temp_new %>% write.csv(.,file=Output_filename,na = "",row.names = FALSE)
  
}else{
  "NOT found existing Nifty Midcap Next 50 survivorship adjusted file" %>% print
  
  ############ Loading data till 2017
  Till_2017_Output_Folder %>% setwd
  list.files() -> Till_2017_Output_Folder_Files
  Till_2017_Output_Folder_Files %>% grep("Nifty_Midcap_Next_50_",.) %>% 
    Till_2017_Output_Folder_Files[.] %>% read.csv -> Existing_till_2017_data
  
  Existing_till_2017_data %>% select(Dates_YYYYMMDD) %>% unlist %>% 
    as.character %>% as.numeric %>% max -> Existing_till_2017_data_max
  
  # Find min starting date of Nifty Midcap Next 50 # Min_Latest_All_Index_candidates
  which(N50_Dates_YYYYMMDD >= Min_Latest_All_Index_candidates) %>% 
    N50_Dates_YYYYMMDD[.] %>% as.numeric %>% as.character -> Relevant_Dates
  
  # Filtering 'Relevant_Dates' from dates in 'Existing_till_2017_data'
  Relevant_Dates %>% as.numeric %>% 
    {which(. > Existing_till_2017_data_max)} %>% Relevant_Dates[.] -> Relevant_Dates
  
  Relevant_Dates %>% length %>% matrix(data = NA, nrow = ., ncol = Output_Col_Nums) %>% 
    data.frame(stringsAsFactors = F) -> Survivorship_Adjustments_Company_Names_df
  
  Output_Col_Nums %>% {.-1} %>% seq(1,.,1) %>% paste0("Company_",.,sep='') %>% 
    c("Dates_YYYYMMDD",.) -> colnames(Survivorship_Adjustments_Company_Names_df)
  
  Survivorship_Adjustments_Company_Names_df$Dates_YYYYMMDD <- as.character(Relevant_Dates)
  
  # i_1 <- 1; i_1 <- 607
  for(i_1 in 1:nrow(Survivorship_Adjustments_Company_Names_df))
  {
    # print(i_1)
    "Making company ticker list for row number " %>% paste0(i_1,sep='') %>% 
      paste0(" out of ", nrow(Survivorship_Adjustments_Company_Names_df),sep='') %>% print
    
    i_1 %>% Survivorship_Adjustments_Company_Names_df$Dates_YYYYMMDD[.] %>% as.numeric -> temp_date_YYYYMMDD
    
    ## Executing NM100 minus NM50
    # Existing_Nifty_Midcap_50 # Existing_Nifty_Midcap_100
    
    Existing_Nifty_Midcap_100 %>% select(Dates_YYYYMMDD) %>% unlist %>% as.character -> Existing_Nifty_Midcap_100_Dates
    which(Existing_Nifty_Midcap_100_Dates == temp_date_YYYYMMDD) %>% 
      Existing_Nifty_Midcap_100[.,-1] %>% unlist %>% as.character -> temp_Existing_Nifty_Midcap_100
    
    Existing_Nifty_Midcap_50 %>% select(Dates_YYYYMMDD) %>% unlist %>% as.character -> Existing_Nifty_Midcap_50_Dates
    which(Existing_Nifty_Midcap_50_Dates == temp_date_YYYYMMDD) %>% 
      Existing_Nifty_Midcap_50[.,-1] %>% unlist %>% as.character -> temp_Existing_Nifty_Midcap_50
    
    (temp_Existing_Nifty_Midcap_100 %in% temp_Existing_Nifty_Midcap_50) %>% {!.} %>% 
      temp_Existing_Nifty_Midcap_100[.] %>% sort -> temp_company_tickers
    
    temp_company_tickers %>% 
      c(.,rep(NA,(Output_Col_Nums-1-length(temp_company_tickers)))) -> Survivorship_Adjustments_Company_Names_df[i_1,-1]
  } # End of 'for(i_1 in 1:nrow(Survivorship_Adjustments_Company_Names_df))'
  
  # Save the file
  if(nrow(Existing_till_2017_data) > 0)
  {
    Existing_till_2017_data %>% 
      rbind(.,Survivorship_Adjustments_Company_Names_df) -> Survivorship_Adjustments_Company_Names_df
  } # End of 'if(nrow(Existing_till_2017_data) > 0)'
  
  print(paste0("Making new '",Output_filename,"'",sep=''))
  Survivorship_Adjustments_Database %>% setwd
  Survivorship_Adjustments_Company_Names_df %>% 
    write.csv(.,file=Output_filename,na = "",row.names = FALSE)
} # End of 'if (Output_filename %in% Survivorship_Adjustments_Database_files)'

#################################################################################
#################################################################################

#################################################################################
#################################### Goals ######################################

# Date: 2020-June-26
# Author: Arunabha Sarkar

# Goals: Survivor & Derivative Adjustment Nifty 50
# File Name: Survivor_Derivative_Adjustment_Nifty_50

#################################################################################
#################################################################################

#################################################################################
##################### Initializing and loading Libraries ########################

library(dplyr)
library(purrr)

#################################################################################
#################################################################################

#################################################################################
################## Set Directories & other Hyperparameters ######################

Output_Col_Nums <- 51

Output_filename <- 'Survivor_Derivative_Adjustment_Nifty_50_Tickers.csv'

"C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% paste0("/NSE FnO Bhav Copies",sep='') -> FnO_Bhav_Database

Central_Database %>% paste0("/Nifty 50 Historical Data Investing dot com",
                            sep='') -> Nifty50_Database  # To find out which day is open & NIFTY Spot

Central_Database %>% 
  paste0("/Survivorship Adjustments Indexes",sep='') -> Survivorship_Adjustments_Database

Central_Database %>% 
  paste0("/Survivorship Derivative Adjustments Indexes",sep='') -> Survivorship_Derivative_Adjustments_Database

Central_Database %>% setwd

if(!file.exists('Survivorship Derivative Adjustments Indexes'))
{
  print("Creating 'Survivorship Derivative Adjustments Indexes' directory.")
  dir.create(file.path(Central_Database, 'Survivorship Derivative Adjustments Indexes'))
}else{
  print("'Survivorship Derivative Adjustments Indexes' directory already exists.")
} # End of 'if(!file.exists('Survivorship Derivative Adjustments Indexes'))'

temp_FnO_colnames <- c("INSTRUMENT","SYMBOL","EXPIRY_DT","STRIKE_PR","OPTIONTYPE",
                       "OPEN","HIGH","LOW","CLOSE","SETTLE_PR","CONTRACTS","VAL_INLAKH",
                       "OPEN_INT","CHG_IN_OI","TIMESTAMP")

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
################### Finding if output file already exists #######################

Survivorship_Derivative_Adjustments_Database %>% setwd

list.files() -> Survivorship_Derivative_Adjustments_Database_Files

if(Output_filename %in% Survivorship_Derivative_Adjustments_Database_Files)
{
  Output_filename %>% paste0("'",.,"' existing file found",sep='') %>% print
  
  Survivorship_Derivative_Adjustments_Database %>% setwd
  Output_filename %>% read.csv -> Old_file
  
  # Deleting last 200 entries from Old_file
  Old_file %>% nrow %>% {.-200} %>% seq(1,.,1) %>% Old_file[.,] -> Old_file
  
  {which(as.numeric(N50_Dates_YYYYMMDD) > max(as.numeric(Old_file$Dates_YYYYMMDD)))} %>% 
    N50_Dates_YYYYMMDD[.] -> Relevant_Dates # This construct will get all the new dates as well
  
  Old_file %>% colnames %>% length %>% matrix(data = NA, nrow = length(Relevant_Dates), ncol = .) %>% 
    data.frame %>% `colnames<-`(colnames(Old_file)) -> temp_temp_new
  
  temp_temp_new$Dates_YYYYMMDD <- Relevant_Dates
  
  Output_filename %>% 
    gsub("Survivor_Derivative_Adjustment_","",.) %>% 
    gsub("_Tickers.csv","",.) -> temp_index
  
  #### Also load 'temp_S_A_file' for dates in 'temp_temp_new'
  Survivorship_Adjustments_Database %>% setwd
  
  list.files() -> Survivorship_Adjustments_Database_Files
  
  Survivorship_Adjustments_Database_Files %>% grep(temp_index,.) %>% 
    Survivorship_Adjustments_Database_Files[.] %>% read.csv -> temp_S_A_file
  
  which(as.numeric(temp_S_A_file$Dates_YYYYMMDD) >= min(Relevant_Dates)) %>% 
    temp_S_A_file[.,] -> temp_S_A_file
  
  temp_S_A_file %>% colnames -> output_colnames
  
  output_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
    `colnames<-`(output_colnames) -> temp_output_file
  
  # i_2 <- 1
  for (i_2 in 1:nrow(temp_temp_new))
  {
    "For Index " %>% paste0(.,temp_index,", processing row number ",i_2,
                            " of ",nrow(temp_temp_new), sep='') %>% print
    
    temp_S_A_file[,-1] %>% .[i_2,] %>% unlist %>% as.character -> temp_S_A_ticker
    
    temp_S_A_file[,1] %>% .[i_2] %>% as.character -> temp_S_A_date
    
    FnO_Bhav_Database %>% setwd
    list.files() -> FnO_Bhav_Database_Files
    
    FnO_Bhav_Database_Files %>% as.Date(format="fo%d%b%Ybhav.csv") %>% format("%Y%m%d") %>% 
      as.character -> FnO_Bhav_Database_Dates
    
    which(FnO_Bhav_Database_Dates == temp_S_A_date) %>% FnO_Bhav_Database_Files[.] -> relevant_bhav_copy
    
    if(exists('relevant_bhav_copy'))
    {
      FnO_Bhav_Database %>% setwd
      
      output_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
        `colnames<-`(output_colnames) -> temp_temp_output_file
      
      relevant_bhav_copy %>% read.csv %>% .[,1:length(temp_FnO_colnames)] %>% 
        `colnames<-`(temp_FnO_colnames) %>% select(SYMBOL) %>% unlist %>% 
        as.character %>% unique -> temp_D_ticker
      
      which(temp_S_A_ticker %in% temp_D_ticker) %>% temp_S_A_ticker[.] -> temp_S_D_A_ticker
      
      temp_S_D_A_ticker %>% c(.,rep(NA,(Output_Col_Nums-1-length(.)))) %>% 
        c(temp_S_A_date,.) -> temp_temp_output_file[1,]
      
      temp_temp_output_file %>% rbind(temp_output_file,.) -> temp_output_file
    }else{
      "Futures bhavcopy file not found for date " %>% paste0(.,temp_S_A_date,sep='') %>% print
    } # End of 'if(exists(relevant_bhav_copy))'
  } # End of 'for (i_2 in 1:nrow(temp_S_A_file))'
  
  Old_file %>% rbind(.,temp_output_file) -> temp_new
  
  # Save the file, delete the old as well
  Survivorship_Derivative_Adjustments_Database %>% setwd
  print(paste0("Overwriting '",Output_filename,"'",sep=''))
  file.remove(Output_filename)
  temp_new %>% write.csv(.,file=Output_filename,na = "",row.names = FALSE)
}else{
  Output_filename %>% paste0("'",.,"' existing file NOT found",sep='') %>% print
  
  Output_Col_Nums %>% {.-1} %>% seq(1,.,1) %>% paste0("Company_",.,sep='') %>% 
    c("Dates_YYYYMMDD",.) -> output_colnames
  
  output_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
    `colnames<-`(output_colnames) -> temp_output_file
  
  Survivorship_Adjustments_Database %>% setwd
  
  list.files() -> Survivorship_Adjustments_Database_Files
  
  Output_filename %>% 
    gsub("Survivor_Derivative_Adjustment_","",.) %>% 
    gsub("_Tickers.csv","",.) -> temp_index
  
  Survivorship_Adjustments_Database_Files %>% grep(temp_index,.) %>% 
    Survivorship_Adjustments_Database_Files[.] -> temp_S_A_filename
  
  if(exists('temp_S_A_filename'))
  {
    Survivorship_Adjustments_Database %>% setwd
    
    temp_S_A_filename %>% read.csv -> temp_S_A_file
    
    # i_1 <- 1
    for (i_1 in 1:nrow(temp_S_A_file))
    {
      "For Index " %>% paste0(.,temp_index,", processing row number ",i_1,
                              " of ",nrow(temp_S_A_file), sep='') %>% print
      
      temp_S_A_file[,-1] %>% .[i_1,] %>% unlist %>% as.character -> temp_S_A_ticker
      
      temp_S_A_file[,1] %>% .[i_1] %>% as.character -> temp_S_A_date
      
      FnO_Bhav_Database %>% setwd
      list.files() -> FnO_Bhav_Database_Files
      
      FnO_Bhav_Database_Files %>% as.Date(format="fo%d%b%Ybhav.csv") %>% format("%Y%m%d") %>% 
        as.character -> FnO_Bhav_Database_Dates
      
      which(FnO_Bhav_Database_Dates == temp_S_A_date) %>% FnO_Bhav_Database_Files[.] -> relevant_bhav_copy
      
      if(exists('relevant_bhav_copy'))
      {
        FnO_Bhav_Database %>% setwd
        
        output_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
          `colnames<-`(output_colnames) -> temp_temp_output_file
        
        relevant_bhav_copy %>% read.csv %>% .[,1:length(temp_FnO_colnames)] %>% 
          `colnames<-`(temp_FnO_colnames) %>% select(SYMBOL) %>% unlist %>% 
          as.character %>% unique -> temp_D_ticker
        
        which(temp_S_A_ticker %in% temp_D_ticker) %>% temp_S_A_ticker[.] -> temp_S_D_A_ticker
        
        temp_S_D_A_ticker %>% c(.,rep(NA,(Output_Col_Nums-1-length(.)))) %>% 
          c(temp_S_A_date,.) -> temp_temp_output_file[1,]
        
        temp_temp_output_file %>% rbind(temp_output_file,.) -> temp_output_file
      }else{
        "Futures bhavcopy file not found for date " %>% paste0(.,temp_S_A_date,sep='') %>% print
      } # End of 'if(exists(relevant_bhav_copy))'
    } # End of 'for (i_1 in 1:nrow(temp_S_A_file))'
    
    # Saving 'temp_output_file'
    if(nrow(temp_output_file) > 0)
    {
      "Saving NEW output for Index '" %>% 
        paste0(temp_index,"', file: '",Output_filename,"'",sep='') %>% print
      
      Survivorship_Derivative_Adjustments_Database %>% setwd
      
      temp_output_file %>% 
        write.csv(.,file=Output_filename,na = "",row.names = FALSE)
    }else{
      "No output could be made/saved for Index '" %>% 
        paste0(temp_index,"'",sep='') %>% print
    } # End of 'if(nrow(temp_output_file) > 0)'
    
  }else{
    "Survivorship Adjusted file NOT found for '" %>% 
      paste0(.,temp_index,"'",sep='') %>% print
  } # End of 'if(exists('temp_S_A_filename'))'
  
} # End of 'if(Output_filename %in% Survivorship_Derivative_Adjustments_Database_Files)'

#################################################################################
#################################################################################

#################################################################################
#################################### Goals ######################################

# Date: 2020-June-26
# Author: Arunabha Sarkar

# Goals: Survivor & Derivative Adjustment Nifty Next 50
# File Name: Survivor_Derivative_Adjustment_Nifty_Next_50

#################################################################################
#################################################################################

#################################################################################
##################### Initializing and loading Libraries ########################

library(dplyr)
library(purrr)

#################################################################################
#################################################################################

#################################################################################
################## Set Directories & other Hyperparameters ######################

Output_Col_Nums <- 51

Output_filename <- 'Survivor_Derivative_Adjustment_Nifty_Next_50_Tickers.csv'

"C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% paste0("/NSE FnO Bhav Copies",sep='') -> FnO_Bhav_Database

Central_Database %>% paste0("/Nifty 50 Historical Data Investing dot com",
                            sep='') -> Nifty50_Database  # To find out which day is open & NIFTY Spot

Central_Database %>% 
  paste0("/Survivorship Adjustments Indexes",sep='') -> Survivorship_Adjustments_Database

Central_Database %>% 
  paste0("/Survivorship Derivative Adjustments Indexes",sep='') -> Survivorship_Derivative_Adjustments_Database

Central_Database %>% setwd

if(!file.exists('Survivorship Derivative Adjustments Indexes'))
{
  print("Creating 'Survivorship Derivative Adjustments Indexes' directory.")
  dir.create(file.path(Central_Database, 'Survivorship Derivative Adjustments Indexes'))
}else{
  print("'Survivorship Derivative Adjustments Indexes' directory already exists.")
} # End of 'if(!file.exists('Survivorship Derivative Adjustments Indexes'))'

temp_FnO_colnames <- c("INSTRUMENT","SYMBOL","EXPIRY_DT","STRIKE_PR","OPTIONTYPE",
                       "OPEN","HIGH","LOW","CLOSE","SETTLE_PR","CONTRACTS","VAL_INLAKH",
                       "OPEN_INT","CHG_IN_OI","TIMESTAMP")

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
################### Finding if output file already exists #######################

Survivorship_Derivative_Adjustments_Database %>% setwd

list.files() -> Survivorship_Derivative_Adjustments_Database_Files

if(Output_filename %in% Survivorship_Derivative_Adjustments_Database_Files)
{
  Output_filename %>% paste0("'",.,"' existing file found",sep='') %>% print
  
  Survivorship_Derivative_Adjustments_Database %>% setwd
  Output_filename %>% read.csv -> Old_file
  
  # Deleting last 200 entries from Old_file
  Old_file %>% nrow %>% {.-200} %>% seq(1,.,1) %>% Old_file[.,] -> Old_file
  
  {which(as.numeric(N50_Dates_YYYYMMDD) > max(as.numeric(Old_file$Dates_YYYYMMDD)))} %>% 
    N50_Dates_YYYYMMDD[.] -> Relevant_Dates # This construct will get all the new dates as well
  
  Old_file %>% colnames %>% length %>% matrix(data = NA, nrow = length(Relevant_Dates), ncol = .) %>% 
    data.frame %>% `colnames<-`(colnames(Old_file)) -> temp_temp_new
  
  temp_temp_new$Dates_YYYYMMDD <- Relevant_Dates
  
  Output_filename %>% 
    gsub("Survivor_Derivative_Adjustment_","",.) %>% 
    gsub("_Tickers.csv","",.) -> temp_index
  
  #### Also load 'temp_S_A_file' for dates in 'temp_temp_new'
  Survivorship_Adjustments_Database %>% setwd
  
  list.files() -> Survivorship_Adjustments_Database_Files
  
  Survivorship_Adjustments_Database_Files %>% grep(temp_index,.) %>% 
    Survivorship_Adjustments_Database_Files[.] %>% read.csv -> temp_S_A_file
  
  which(as.numeric(temp_S_A_file$Dates_YYYYMMDD) >= min(Relevant_Dates)) %>% 
    temp_S_A_file[.,] -> temp_S_A_file
  
  temp_S_A_file %>% colnames -> output_colnames
  
  output_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
    `colnames<-`(output_colnames) -> temp_output_file
  
  # i_2 <- 1
  for (i_2 in 1:nrow(temp_temp_new))
  {
    "For Index " %>% paste0(.,temp_index,", processing row number ",i_2,
                            " of ",nrow(temp_temp_new), sep='') %>% print
    
    temp_S_A_file[,-1] %>% .[i_2,] %>% unlist %>% as.character -> temp_S_A_ticker
    
    temp_S_A_file[,1] %>% .[i_2] %>% as.character -> temp_S_A_date
    
    FnO_Bhav_Database %>% setwd
    list.files() -> FnO_Bhav_Database_Files
    
    FnO_Bhav_Database_Files %>% as.Date(format="fo%d%b%Ybhav.csv") %>% format("%Y%m%d") %>% 
      as.character -> FnO_Bhav_Database_Dates
    
    which(FnO_Bhav_Database_Dates == temp_S_A_date) %>% FnO_Bhav_Database_Files[.] -> relevant_bhav_copy
    
    if(exists('relevant_bhav_copy'))
    {
      FnO_Bhav_Database %>% setwd
      
      output_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
        `colnames<-`(output_colnames) -> temp_temp_output_file
      
      relevant_bhav_copy %>% read.csv %>% .[,1:length(temp_FnO_colnames)] %>% 
        `colnames<-`(temp_FnO_colnames) %>% select(SYMBOL) %>% unlist %>% 
        as.character %>% unique -> temp_D_ticker
      
      which(temp_S_A_ticker %in% temp_D_ticker) %>% temp_S_A_ticker[.] -> temp_S_D_A_ticker
      
      temp_S_D_A_ticker %>% c(.,rep(NA,(Output_Col_Nums-1-length(.)))) %>% 
        c(temp_S_A_date,.) -> temp_temp_output_file[1,]
      
      temp_temp_output_file %>% rbind(temp_output_file,.) -> temp_output_file
    }else{
      "Futures bhavcopy file not found for date " %>% paste0(.,temp_S_A_date,sep='') %>% print
    } # End of 'if(exists(relevant_bhav_copy))'
  } # End of 'for (i_2 in 1:nrow(temp_S_A_file))'
  
  Old_file %>% rbind(.,temp_output_file) -> temp_new
  
  # Save the file, delete the old as well
  Survivorship_Derivative_Adjustments_Database %>% setwd
  print(paste0("Overwriting '",Output_filename,"'",sep=''))
  file.remove(Output_filename)
  temp_new %>% write.csv(.,file=Output_filename,na = "",row.names = FALSE)
}else{
  Output_filename %>% paste0("'",.,"' existing file NOT found",sep='') %>% print
  
  Output_Col_Nums %>% {.-1} %>% seq(1,.,1) %>% paste0("Company_",.,sep='') %>% 
    c("Dates_YYYYMMDD",.) -> output_colnames
  
  output_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
    `colnames<-`(output_colnames) -> temp_output_file
  
  Survivorship_Adjustments_Database %>% setwd
  
  list.files() -> Survivorship_Adjustments_Database_Files
  
  Output_filename %>% 
    gsub("Survivor_Derivative_Adjustment_","",.) %>% 
    gsub("_Tickers.csv","",.) -> temp_index
  
  Survivorship_Adjustments_Database_Files %>% grep(temp_index,.) %>% 
    Survivorship_Adjustments_Database_Files[.] -> temp_S_A_filename
  
  if(exists('temp_S_A_filename'))
  {
    Survivorship_Adjustments_Database %>% setwd
    
    temp_S_A_filename %>% read.csv -> temp_S_A_file
    
    # i_1 <- 1
    for (i_1 in 1:nrow(temp_S_A_file))
    {
      "For Index " %>% paste0(.,temp_index,", processing row number ",i_1,
                              " of ",nrow(temp_S_A_file), sep='') %>% print
      
      temp_S_A_file[,-1] %>% .[i_1,] %>% unlist %>% as.character -> temp_S_A_ticker
      
      temp_S_A_file[,1] %>% .[i_1] %>% as.character -> temp_S_A_date
      
      FnO_Bhav_Database %>% setwd
      list.files() -> FnO_Bhav_Database_Files
      
      FnO_Bhav_Database_Files %>% as.Date(format="fo%d%b%Ybhav.csv") %>% format("%Y%m%d") %>% 
        as.character -> FnO_Bhav_Database_Dates
      
      which(FnO_Bhav_Database_Dates == temp_S_A_date) %>% FnO_Bhav_Database_Files[.] -> relevant_bhav_copy
      
      if(exists('relevant_bhav_copy'))
      {
        FnO_Bhav_Database %>% setwd
        
        output_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
          `colnames<-`(output_colnames) -> temp_temp_output_file
        
        relevant_bhav_copy %>% read.csv %>% .[,1:length(temp_FnO_colnames)] %>% 
          `colnames<-`(temp_FnO_colnames) %>% select(SYMBOL) %>% unlist %>% 
          as.character %>% unique -> temp_D_ticker
        
        which(temp_S_A_ticker %in% temp_D_ticker) %>% temp_S_A_ticker[.] -> temp_S_D_A_ticker
        
        temp_S_D_A_ticker %>% c(.,rep(NA,(Output_Col_Nums-1-length(.)))) %>% 
          c(temp_S_A_date,.) -> temp_temp_output_file[1,]
        
        temp_temp_output_file %>% rbind(temp_output_file,.) -> temp_output_file
      }else{
        "Futures bhavcopy file not found for date " %>% paste0(.,temp_S_A_date,sep='') %>% print
      } # End of 'if(exists(relevant_bhav_copy))'
    } # End of 'for (i_1 in 1:nrow(temp_S_A_file))'
    
    # Saving 'temp_output_file'
    if(nrow(temp_output_file) > 0)
    {
      "Saving NEW output for Index '" %>% 
        paste0(temp_index,"', file: '",Output_filename,"'",sep='') %>% print
      
      Survivorship_Derivative_Adjustments_Database %>% setwd
      
      temp_output_file %>% 
        write.csv(.,file=Output_filename,na = "",row.names = FALSE)
    }else{
      "No output could be made/saved for Index '" %>% 
        paste0(temp_index,"'",sep='') %>% print
    } # End of 'if(nrow(temp_output_file) > 0)'
    
  }else{
    "Survivorship Adjusted file NOT found for '" %>% 
      paste0(.,temp_index,"'",sep='') %>% print
  } # End of 'if(exists('temp_S_A_filename'))'
  
} # End of 'if(Output_filename %in% Survivorship_Derivative_Adjustments_Database_Files)'

#################################################################################
#################################################################################

#################################################################################
#################################### Goals ######################################

# Date: 2020-June-26
# Author: Arunabha Sarkar

# Goals: Survivor & Derivative Adjustment Nifty 100
# File Name: Survivor_Derivative_Adjustment_Nifty_100

#################################################################################
#################################################################################

#################################################################################
##################### Initializing and loading Libraries ########################

library(dplyr)
library(purrr)

#################################################################################
#################################################################################

#################################################################################
################## Set Directories & other Hyperparameters ######################

Output_Col_Nums <- 101

Output_filename <- 'Survivor_Derivative_Adjustment_Nifty_100_Tickers.csv'

"C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% paste0("/NSE FnO Bhav Copies",sep='') -> FnO_Bhav_Database

Central_Database %>% paste0("/Nifty 50 Historical Data Investing dot com",
                            sep='') -> Nifty50_Database  # To find out which day is open & NIFTY Spot

Central_Database %>% 
  paste0("/Survivorship Adjustments Indexes",sep='') -> Survivorship_Adjustments_Database

Central_Database %>% 
  paste0("/Survivorship Derivative Adjustments Indexes",sep='') -> Survivorship_Derivative_Adjustments_Database

Central_Database %>% setwd

if(!file.exists('Survivorship Derivative Adjustments Indexes'))
{
  print("Creating 'Survivorship Derivative Adjustments Indexes' directory.")
  dir.create(file.path(Central_Database, 'Survivorship Derivative Adjustments Indexes'))
}else{
  print("'Survivorship Derivative Adjustments Indexes' directory already exists.")
} # End of 'if(!file.exists('Survivorship Derivative Adjustments Indexes'))'

temp_FnO_colnames <- c("INSTRUMENT","SYMBOL","EXPIRY_DT","STRIKE_PR","OPTIONTYPE",
                       "OPEN","HIGH","LOW","CLOSE","SETTLE_PR","CONTRACTS","VAL_INLAKH",
                       "OPEN_INT","CHG_IN_OI","TIMESTAMP")

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
################### Finding if output file already exists #######################

Survivorship_Derivative_Adjustments_Database %>% setwd

list.files() -> Survivorship_Derivative_Adjustments_Database_Files

if(Output_filename %in% Survivorship_Derivative_Adjustments_Database_Files)
{
  Output_filename %>% paste0("'",.,"' existing file found",sep='') %>% print
  
  Survivorship_Derivative_Adjustments_Database %>% setwd
  Output_filename %>% read.csv -> Old_file
  
  # Deleting last 200 entries from Old_file
  Old_file %>% nrow %>% {.-200} %>% seq(1,.,1) %>% Old_file[.,] -> Old_file
  
  {which(as.numeric(N50_Dates_YYYYMMDD) > max(as.numeric(Old_file$Dates_YYYYMMDD)))} %>% 
    N50_Dates_YYYYMMDD[.] -> Relevant_Dates # This construct will get all the new dates as well
  
  Old_file %>% colnames %>% length %>% matrix(data = NA, nrow = length(Relevant_Dates), ncol = .) %>% 
    data.frame %>% `colnames<-`(colnames(Old_file)) -> temp_temp_new
  
  temp_temp_new$Dates_YYYYMMDD <- Relevant_Dates
  
  Output_filename %>% 
    gsub("Survivor_Derivative_Adjustment_","",.) %>% 
    gsub("_Tickers.csv","",.) -> temp_index
  
  #### Also load 'temp_S_A_file' for dates in 'temp_temp_new'
  Survivorship_Adjustments_Database %>% setwd
  
  list.files() -> Survivorship_Adjustments_Database_Files
  
  Survivorship_Adjustments_Database_Files %>% grep(temp_index,.) %>% 
    Survivorship_Adjustments_Database_Files[.] %>% read.csv -> temp_S_A_file
  
  which(as.numeric(temp_S_A_file$Dates_YYYYMMDD) >= min(Relevant_Dates)) %>% 
    temp_S_A_file[.,] -> temp_S_A_file
  
  temp_S_A_file %>% colnames -> output_colnames
  
  output_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
    `colnames<-`(output_colnames) -> temp_output_file
  
  # i_2 <- 1
  for (i_2 in 1:nrow(temp_temp_new))
  {
    "For Index " %>% paste0(.,temp_index,", processing row number ",i_2,
                            " of ",nrow(temp_temp_new), sep='') %>% print
    
    temp_S_A_file[,-1] %>% .[i_2,] %>% unlist %>% as.character -> temp_S_A_ticker
    
    temp_S_A_file[,1] %>% .[i_2] %>% as.character -> temp_S_A_date
    
    FnO_Bhav_Database %>% setwd
    list.files() -> FnO_Bhav_Database_Files
    
    FnO_Bhav_Database_Files %>% as.Date(format="fo%d%b%Ybhav.csv") %>% format("%Y%m%d") %>% 
      as.character -> FnO_Bhav_Database_Dates
    
    which(FnO_Bhav_Database_Dates == temp_S_A_date) %>% FnO_Bhav_Database_Files[.] -> relevant_bhav_copy
    
    if(exists('relevant_bhav_copy'))
    {
      FnO_Bhav_Database %>% setwd
      
      output_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
        `colnames<-`(output_colnames) -> temp_temp_output_file
      
      relevant_bhav_copy %>% read.csv %>% .[,1:length(temp_FnO_colnames)] %>% 
        `colnames<-`(temp_FnO_colnames) %>% select(SYMBOL) %>% unlist %>% 
        as.character %>% unique -> temp_D_ticker
      
      which(temp_S_A_ticker %in% temp_D_ticker) %>% temp_S_A_ticker[.] -> temp_S_D_A_ticker
      
      temp_S_D_A_ticker %>% c(.,rep(NA,(Output_Col_Nums-1-length(.)))) %>% 
        c(temp_S_A_date,.) -> temp_temp_output_file[1,]
      
      temp_temp_output_file %>% rbind(temp_output_file,.) -> temp_output_file
    }else{
      "Futures bhavcopy file not found for date " %>% paste0(.,temp_S_A_date,sep='') %>% print
    } # End of 'if(exists(relevant_bhav_copy))'
  } # End of 'for (i_2 in 1:nrow(temp_S_A_file))'
  
  Old_file %>% rbind(.,temp_output_file) -> temp_new
  
  # Save the file, delete the old as well
  Survivorship_Derivative_Adjustments_Database %>% setwd
  print(paste0("Overwriting '",Output_filename,"'",sep=''))
  file.remove(Output_filename)
  temp_new %>% write.csv(.,file=Output_filename,na = "",row.names = FALSE)
}else{
  Output_filename %>% paste0("'",.,"' existing file NOT found",sep='') %>% print
  
  Output_Col_Nums %>% {.-1} %>% seq(1,.,1) %>% paste0("Company_",.,sep='') %>% 
    c("Dates_YYYYMMDD",.) -> output_colnames
  
  output_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
    `colnames<-`(output_colnames) -> temp_output_file
  
  Survivorship_Adjustments_Database %>% setwd
  
  list.files() -> Survivorship_Adjustments_Database_Files
  
  Output_filename %>% 
    gsub("Survivor_Derivative_Adjustment_","",.) %>% 
    gsub("_Tickers.csv","",.) -> temp_index
  
  Survivorship_Adjustments_Database_Files %>% grep(temp_index,.) %>% 
    Survivorship_Adjustments_Database_Files[.] -> temp_S_A_filename
  
  if(exists('temp_S_A_filename'))
  {
    Survivorship_Adjustments_Database %>% setwd
    
    temp_S_A_filename %>% read.csv -> temp_S_A_file
    
    # i_1 <- 1
    for (i_1 in 1:nrow(temp_S_A_file))
    {
      "For Index " %>% paste0(.,temp_index,", processing row number ",i_1,
                              " of ",nrow(temp_S_A_file), sep='') %>% print
      
      temp_S_A_file[,-1] %>% .[i_1,] %>% unlist %>% as.character -> temp_S_A_ticker
      
      temp_S_A_file[,1] %>% .[i_1] %>% as.character -> temp_S_A_date
      
      FnO_Bhav_Database %>% setwd
      list.files() -> FnO_Bhav_Database_Files
      
      FnO_Bhav_Database_Files %>% as.Date(format="fo%d%b%Ybhav.csv") %>% format("%Y%m%d") %>% 
        as.character -> FnO_Bhav_Database_Dates
      
      which(FnO_Bhav_Database_Dates == temp_S_A_date) %>% FnO_Bhav_Database_Files[.] -> relevant_bhav_copy
      
      if(exists('relevant_bhav_copy'))
      {
        FnO_Bhav_Database %>% setwd
        
        output_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
          `colnames<-`(output_colnames) -> temp_temp_output_file
        
        relevant_bhav_copy %>% read.csv %>% .[,1:length(temp_FnO_colnames)] %>% 
          `colnames<-`(temp_FnO_colnames) %>% select(SYMBOL) %>% unlist %>% 
          as.character %>% unique -> temp_D_ticker
        
        which(temp_S_A_ticker %in% temp_D_ticker) %>% temp_S_A_ticker[.] -> temp_S_D_A_ticker
        
        temp_S_D_A_ticker %>% c(.,rep(NA,(Output_Col_Nums-1-length(.)))) %>% 
          c(temp_S_A_date,.) -> temp_temp_output_file[1,]
        
        temp_temp_output_file %>% rbind(temp_output_file,.) -> temp_output_file
      }else{
        "Futures bhavcopy file not found for date " %>% paste0(.,temp_S_A_date,sep='') %>% print
      } # End of 'if(exists(relevant_bhav_copy))'
    } # End of 'for (i_1 in 1:nrow(temp_S_A_file))'
    
    # Saving 'temp_output_file'
    if(nrow(temp_output_file) > 0)
    {
      "Saving NEW output for Index '" %>% 
        paste0(temp_index,"', file: '",Output_filename,"'",sep='') %>% print
      
      Survivorship_Derivative_Adjustments_Database %>% setwd
      
      temp_output_file %>% 
        write.csv(.,file=Output_filename,na = "",row.names = FALSE)
    }else{
      "No output could be made/saved for Index '" %>% 
        paste0(temp_index,"'",sep='') %>% print
    } # End of 'if(nrow(temp_output_file) > 0)'
    
  }else{
    "Survivorship Adjusted file NOT found for '" %>% 
      paste0(.,temp_index,"'",sep='') %>% print
  } # End of 'if(exists('temp_S_A_filename'))'
  
} # End of 'if(Output_filename %in% Survivorship_Derivative_Adjustments_Database_Files)'

#################################################################################
#################################################################################

#################################################################################
#################################### Goals ######################################

# Date: 2020-June-26
# Author: Arunabha Sarkar

# Goals: Survivor & Derivative Adjustment Nifty Midcap 100
# File Name: Survivor_Derivative_Adjustment_Nifty_Midcap_100

#################################################################################
#################################################################################

#################################################################################
##################### Initializing and loading Libraries ########################

library(dplyr)
library(purrr)

#################################################################################
#################################################################################

#################################################################################
################## Set Directories & other Hyperparameters ######################

Output_Col_Nums <- 101

Output_filename <- 'Survivor_Derivative_Adjustment_Nifty_Midcap_100_Tickers.csv'

"C:/Users/Arunabha Sarkar/Desktop/Work From home/Central Database" -> Central_Database

Central_Database %>% paste0("/NSE FnO Bhav Copies",sep='') -> FnO_Bhav_Database

Central_Database %>% paste0("/Nifty 50 Historical Data Investing dot com",
                            sep='') -> Nifty50_Database  # To find out which day is open & NIFTY Spot

Central_Database %>% 
  paste0("/Survivorship Adjustments Indexes",sep='') -> Survivorship_Adjustments_Database

Central_Database %>% 
  paste0("/Survivorship Derivative Adjustments Indexes",sep='') -> Survivorship_Derivative_Adjustments_Database

Central_Database %>% setwd

if(!file.exists('Survivorship Derivative Adjustments Indexes'))
{
  print("Creating 'Survivorship Derivative Adjustments Indexes' directory.")
  dir.create(file.path(Central_Database, 'Survivorship Derivative Adjustments Indexes'))
}else{
  print("'Survivorship Derivative Adjustments Indexes' directory already exists.")
} # End of 'if(!file.exists('Survivorship Derivative Adjustments Indexes'))'

temp_FnO_colnames <- c("INSTRUMENT","SYMBOL","EXPIRY_DT","STRIKE_PR","OPTIONTYPE",
                       "OPEN","HIGH","LOW","CLOSE","SETTLE_PR","CONTRACTS","VAL_INLAKH",
                       "OPEN_INT","CHG_IN_OI","TIMESTAMP")

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
################### Finding if output file already exists #######################

Survivorship_Derivative_Adjustments_Database %>% setwd

list.files() -> Survivorship_Derivative_Adjustments_Database_Files

if(Output_filename %in% Survivorship_Derivative_Adjustments_Database_Files)
{
  Output_filename %>% paste0("'",.,"' existing file found",sep='') %>% print
  
  Survivorship_Derivative_Adjustments_Database %>% setwd
  Output_filename %>% read.csv -> Old_file
  
  # Deleting last 200 entries from Old_file
  Old_file %>% nrow %>% {.-200} %>% seq(1,.,1) %>% Old_file[.,] -> Old_file
  
  {which(as.numeric(N50_Dates_YYYYMMDD) > max(as.numeric(Old_file$Dates_YYYYMMDD)))} %>% 
    N50_Dates_YYYYMMDD[.] -> Relevant_Dates # This construct will get all the new dates as well
  
  Old_file %>% colnames %>% length %>% matrix(data = NA, nrow = length(Relevant_Dates), ncol = .) %>% 
    data.frame %>% `colnames<-`(colnames(Old_file)) -> temp_temp_new
  
  temp_temp_new$Dates_YYYYMMDD <- Relevant_Dates
  
  Output_filename %>% 
    gsub("Survivor_Derivative_Adjustment_","",.) %>% 
    gsub("_Tickers.csv","",.) -> temp_index
  
  #### Also load 'temp_S_A_file' for dates in 'temp_temp_new'
  Survivorship_Adjustments_Database %>% setwd
  
  list.files() -> Survivorship_Adjustments_Database_Files
  
  Survivorship_Adjustments_Database_Files %>% grep(temp_index,.) %>% 
    Survivorship_Adjustments_Database_Files[.] %>% read.csv -> temp_S_A_file
  
  which(as.numeric(temp_S_A_file$Dates_YYYYMMDD) >= min(Relevant_Dates)) %>% 
    temp_S_A_file[.,] -> temp_S_A_file
  
  temp_S_A_file %>% colnames -> output_colnames
  
  output_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
    `colnames<-`(output_colnames) -> temp_output_file
  
  # i_2 <- 1
  for (i_2 in 1:nrow(temp_temp_new))
  {
    "For Index " %>% paste0(.,temp_index,", processing row number ",i_2,
                            " of ",nrow(temp_temp_new), sep='') %>% print
    
    temp_S_A_file[,-1] %>% .[i_2,] %>% unlist %>% as.character -> temp_S_A_ticker
    
    temp_S_A_file[,1] %>% .[i_2] %>% as.character -> temp_S_A_date
    
    FnO_Bhav_Database %>% setwd
    list.files() -> FnO_Bhav_Database_Files
    
    FnO_Bhav_Database_Files %>% as.Date(format="fo%d%b%Ybhav.csv") %>% format("%Y%m%d") %>% 
      as.character -> FnO_Bhav_Database_Dates
    
    which(FnO_Bhav_Database_Dates == temp_S_A_date) %>% FnO_Bhav_Database_Files[.] -> relevant_bhav_copy
    
    if(exists('relevant_bhav_copy'))
    {
      FnO_Bhav_Database %>% setwd
      
      output_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
        `colnames<-`(output_colnames) -> temp_temp_output_file
      
      relevant_bhav_copy %>% read.csv %>% .[,1:length(temp_FnO_colnames)] %>% 
        `colnames<-`(temp_FnO_colnames) %>% select(SYMBOL) %>% unlist %>% 
        as.character %>% unique -> temp_D_ticker
      
      which(temp_S_A_ticker %in% temp_D_ticker) %>% temp_S_A_ticker[.] -> temp_S_D_A_ticker
      
      temp_S_D_A_ticker %>% c(.,rep(NA,(Output_Col_Nums-1-length(.)))) %>% 
        c(temp_S_A_date,.) -> temp_temp_output_file[1,]
      
      temp_temp_output_file %>% rbind(temp_output_file,.) -> temp_output_file
    }else{
      "Futures bhavcopy file not found for date " %>% paste0(.,temp_S_A_date,sep='') %>% print
    } # End of 'if(exists(relevant_bhav_copy))'
  } # End of 'for (i_2 in 1:nrow(temp_S_A_file))'
  
  Old_file %>% rbind(.,temp_output_file) -> temp_new
  
  # Save the file, delete the old as well
  Survivorship_Derivative_Adjustments_Database %>% setwd
  print(paste0("Overwriting '",Output_filename,"'",sep=''))
  file.remove(Output_filename)
  temp_new %>% write.csv(.,file=Output_filename,na = "",row.names = FALSE)
}else{
  Output_filename %>% paste0("'",.,"' existing file NOT found",sep='') %>% print
  
  Output_Col_Nums %>% {.-1} %>% seq(1,.,1) %>% paste0("Company_",.,sep='') %>% 
    c("Dates_YYYYMMDD",.) -> output_colnames
  
  output_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
    `colnames<-`(output_colnames) -> temp_output_file
  
  Survivorship_Adjustments_Database %>% setwd
  
  list.files() -> Survivorship_Adjustments_Database_Files
  
  Output_filename %>% 
    gsub("Survivor_Derivative_Adjustment_","",.) %>% 
    gsub("_Tickers.csv","",.) -> temp_index
  
  Survivorship_Adjustments_Database_Files %>% grep(temp_index,.) %>% 
    Survivorship_Adjustments_Database_Files[.] -> temp_S_A_filename
  
  if(exists('temp_S_A_filename'))
  {
    Survivorship_Adjustments_Database %>% setwd
    
    temp_S_A_filename %>% read.csv -> temp_S_A_file
    
    # i_1 <- 1
    for (i_1 in 1:nrow(temp_S_A_file))
    {
      "For Index " %>% paste0(.,temp_index,", processing row number ",i_1,
                              " of ",nrow(temp_S_A_file), sep='') %>% print
      
      temp_S_A_file[,-1] %>% .[i_1,] %>% unlist %>% as.character -> temp_S_A_ticker
      
      temp_S_A_file[,1] %>% .[i_1] %>% as.character -> temp_S_A_date
      
      FnO_Bhav_Database %>% setwd
      list.files() -> FnO_Bhav_Database_Files
      
      FnO_Bhav_Database_Files %>% as.Date(format="fo%d%b%Ybhav.csv") %>% format("%Y%m%d") %>% 
        as.character -> FnO_Bhav_Database_Dates
      
      which(FnO_Bhav_Database_Dates == temp_S_A_date) %>% FnO_Bhav_Database_Files[.] -> relevant_bhav_copy
      
      if(exists('relevant_bhav_copy'))
      {
        FnO_Bhav_Database %>% setwd
        
        output_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
          `colnames<-`(output_colnames) -> temp_temp_output_file
        
        relevant_bhav_copy %>% read.csv %>% .[,1:length(temp_FnO_colnames)] %>% 
          `colnames<-`(temp_FnO_colnames) %>% select(SYMBOL) %>% unlist %>% 
          as.character %>% unique -> temp_D_ticker
        
        which(temp_S_A_ticker %in% temp_D_ticker) %>% temp_S_A_ticker[.] -> temp_S_D_A_ticker
        
        temp_S_D_A_ticker %>% c(.,rep(NA,(Output_Col_Nums-1-length(.)))) %>% 
          c(temp_S_A_date,.) -> temp_temp_output_file[1,]
        
        temp_temp_output_file %>% rbind(temp_output_file,.) -> temp_output_file
      }else{
        "Futures bhavcopy file not found for date " %>% paste0(.,temp_S_A_date,sep='') %>% print
      } # End of 'if(exists(relevant_bhav_copy))'
    } # End of 'for (i_1 in 1:nrow(temp_S_A_file))'
    
    # Saving 'temp_output_file'
    if(nrow(temp_output_file) > 0)
    {
      "Saving NEW output for Index '" %>% 
        paste0(temp_index,"', file: '",Output_filename,"'",sep='') %>% print
      
      Survivorship_Derivative_Adjustments_Database %>% setwd
      
      temp_output_file %>% 
        write.csv(.,file=Output_filename,na = "",row.names = FALSE)
    }else{
      "No output could be made/saved for Index '" %>% 
        paste0(temp_index,"'",sep='') %>% print
    } # End of 'if(nrow(temp_output_file) > 0)'
    
  }else{
    "Survivorship Adjusted file NOT found for '" %>% 
      paste0(.,temp_index,"'",sep='') %>% print
  } # End of 'if(exists('temp_S_A_filename'))'
  
} # End of 'if(Output_filename %in% Survivorship_Derivative_Adjustments_Database_Files)'

#################################################################################
#################################################################################

#################################################################################
#################################### Goals ######################################

# Date: 2020-June-26
# Author: Arunabha Sarkar

# Goals: Survivor & Derivative Adjustment Nifty Midcap 50
# File Name: Survivor_Derivative_Adjustment_Nifty_Midcap_50

#################################################################################
#################################################################################

#################################################################################
##################### Initializing and loading Libraries ########################

library(dplyr)
library(purrr)

#################################################################################
#################################################################################

#################################################################################
################## Set Directories & other Hyperparameters ######################

Output_Col_Nums <- 51

Output_filename <- 'Survivor_Derivative_Adjustment_Nifty_Midcap_50_Tickers.csv'

"C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% paste0("/NSE FnO Bhav Copies",sep='') -> FnO_Bhav_Database

Central_Database %>% paste0("/Nifty 50 Historical Data Investing dot com",
                            sep='') -> Nifty50_Database  # To find out which day is open & NIFTY Spot

Central_Database %>% 
  paste0("/Survivorship Adjustments Indexes",sep='') -> Survivorship_Adjustments_Database

Central_Database %>% 
  paste0("/Survivorship Derivative Adjustments Indexes",sep='') -> Survivorship_Derivative_Adjustments_Database

Central_Database %>% setwd

if(!file.exists('Survivorship Derivative Adjustments Indexes'))
{
  print("Creating 'Survivorship Derivative Adjustments Indexes' directory.")
  dir.create(file.path(Central_Database, 'Survivorship Derivative Adjustments Indexes'))
}else{
  print("'Survivorship Derivative Adjustments Indexes' directory already exists.")
} # End of 'if(!file.exists('Survivorship Derivative Adjustments Indexes'))'

temp_FnO_colnames <- c("INSTRUMENT","SYMBOL","EXPIRY_DT","STRIKE_PR","OPTIONTYPE",
                       "OPEN","HIGH","LOW","CLOSE","SETTLE_PR","CONTRACTS","VAL_INLAKH",
                       "OPEN_INT","CHG_IN_OI","TIMESTAMP")

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
################### Finding if output file already exists #######################

Survivorship_Derivative_Adjustments_Database %>% setwd

list.files() -> Survivorship_Derivative_Adjustments_Database_Files

if(Output_filename %in% Survivorship_Derivative_Adjustments_Database_Files)
{
  Output_filename %>% paste0("'",.,"' existing file found",sep='') %>% print
  
  Survivorship_Derivative_Adjustments_Database %>% setwd
  Output_filename %>% read.csv -> Old_file
  
  # Deleting last 200 entries from Old_file
  Old_file %>% nrow %>% {.-200} %>% seq(1,.,1) %>% Old_file[.,] -> Old_file
  
  {which(as.numeric(N50_Dates_YYYYMMDD) > max(as.numeric(Old_file$Dates_YYYYMMDD)))} %>% 
    N50_Dates_YYYYMMDD[.] -> Relevant_Dates # This construct will get all the new dates as well
  
  Old_file %>% colnames %>% length %>% matrix(data = NA, nrow = length(Relevant_Dates), ncol = .) %>% 
    data.frame %>% `colnames<-`(colnames(Old_file)) -> temp_temp_new
  
  temp_temp_new$Dates_YYYYMMDD <- Relevant_Dates
  
  Output_filename %>% 
    gsub("Survivor_Derivative_Adjustment_","",.) %>% 
    gsub("_Tickers.csv","",.) -> temp_index
  
  #### Also load 'temp_S_A_file' for dates in 'temp_temp_new'
  Survivorship_Adjustments_Database %>% setwd
  
  list.files() -> Survivorship_Adjustments_Database_Files
  
  Survivorship_Adjustments_Database_Files %>% grep(temp_index,.) %>% 
    Survivorship_Adjustments_Database_Files[.] %>% read.csv -> temp_S_A_file
  
  which(as.numeric(temp_S_A_file$Dates_YYYYMMDD) >= min(Relevant_Dates)) %>% 
    temp_S_A_file[.,] -> temp_S_A_file
  
  temp_S_A_file %>% colnames -> output_colnames
  
  output_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
    `colnames<-`(output_colnames) -> temp_output_file
  
  # i_2 <- 1
  for (i_2 in 1:nrow(temp_temp_new))
  {
    "For Index " %>% paste0(.,temp_index,", processing row number ",i_2,
                            " of ",nrow(temp_temp_new), sep='') %>% print
    
    temp_S_A_file[,-1] %>% .[i_2,] %>% unlist %>% as.character -> temp_S_A_ticker
    
    temp_S_A_file[,1] %>% .[i_2] %>% as.character -> temp_S_A_date
    
    FnO_Bhav_Database %>% setwd
    list.files() -> FnO_Bhav_Database_Files
    
    FnO_Bhav_Database_Files %>% as.Date(format="fo%d%b%Ybhav.csv") %>% format("%Y%m%d") %>% 
      as.character -> FnO_Bhav_Database_Dates
    
    which(FnO_Bhav_Database_Dates == temp_S_A_date) %>% FnO_Bhav_Database_Files[.] -> relevant_bhav_copy
    
    if(exists('relevant_bhav_copy'))
    {
      FnO_Bhav_Database %>% setwd
      
      output_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
        `colnames<-`(output_colnames) -> temp_temp_output_file
      
      relevant_bhav_copy %>% read.csv %>% .[,1:length(temp_FnO_colnames)] %>% 
        `colnames<-`(temp_FnO_colnames) %>% select(SYMBOL) %>% unlist %>% 
        as.character %>% unique -> temp_D_ticker
      
      which(temp_S_A_ticker %in% temp_D_ticker) %>% temp_S_A_ticker[.] -> temp_S_D_A_ticker
      
      temp_S_D_A_ticker %>% c(.,rep(NA,(Output_Col_Nums-1-length(.)))) %>% 
        c(temp_S_A_date,.) -> temp_temp_output_file[1,]
      
      temp_temp_output_file %>% rbind(temp_output_file,.) -> temp_output_file
    }else{
      "Futures bhavcopy file not found for date " %>% paste0(.,temp_S_A_date,sep='') %>% print
    } # End of 'if(exists(relevant_bhav_copy))'
  } # End of 'for (i_2 in 1:nrow(temp_S_A_file))'
  
  Old_file %>% rbind(.,temp_output_file) -> temp_new
  
  # Save the file, delete the old as well
  Survivorship_Derivative_Adjustments_Database %>% setwd
  print(paste0("Overwriting '",Output_filename,"'",sep=''))
  file.remove(Output_filename)
  temp_new %>% write.csv(.,file=Output_filename,na = "",row.names = FALSE)
}else{
  Output_filename %>% paste0("'",.,"' existing file NOT found",sep='') %>% print
  
  Output_Col_Nums %>% {.-1} %>% seq(1,.,1) %>% paste0("Company_",.,sep='') %>% 
    c("Dates_YYYYMMDD",.) -> output_colnames
  
  output_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
    `colnames<-`(output_colnames) -> temp_output_file
  
  Survivorship_Adjustments_Database %>% setwd
  
  list.files() -> Survivorship_Adjustments_Database_Files
  
  Output_filename %>% 
    gsub("Survivor_Derivative_Adjustment_","",.) %>% 
    gsub("_Tickers.csv","",.) -> temp_index
  
  Survivorship_Adjustments_Database_Files %>% grep(temp_index,.) %>% 
    Survivorship_Adjustments_Database_Files[.] -> temp_S_A_filename
  
  if(exists('temp_S_A_filename'))
  {
    Survivorship_Adjustments_Database %>% setwd
    
    temp_S_A_filename %>% read.csv -> temp_S_A_file
    
    # i_1 <- 1
    for (i_1 in 1:nrow(temp_S_A_file))
    {
      "For Index " %>% paste0(.,temp_index,", processing row number ",i_1,
                              " of ",nrow(temp_S_A_file), sep='') %>% print
      
      temp_S_A_file[,-1] %>% .[i_1,] %>% unlist %>% as.character -> temp_S_A_ticker
      
      temp_S_A_file[,1] %>% .[i_1] %>% as.character -> temp_S_A_date
      
      FnO_Bhav_Database %>% setwd
      list.files() -> FnO_Bhav_Database_Files
      
      FnO_Bhav_Database_Files %>% as.Date(format="fo%d%b%Ybhav.csv") %>% format("%Y%m%d") %>% 
        as.character -> FnO_Bhav_Database_Dates
      
      which(FnO_Bhav_Database_Dates == temp_S_A_date) %>% FnO_Bhav_Database_Files[.] -> relevant_bhav_copy
      
      if(exists('relevant_bhav_copy'))
      {
        FnO_Bhav_Database %>% setwd
        
        output_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
          `colnames<-`(output_colnames) -> temp_temp_output_file
        
        relevant_bhav_copy %>% read.csv %>% .[,1:length(temp_FnO_colnames)] %>% 
          `colnames<-`(temp_FnO_colnames) %>% select(SYMBOL) %>% unlist %>% 
          as.character %>% unique -> temp_D_ticker
        
        which(temp_S_A_ticker %in% temp_D_ticker) %>% temp_S_A_ticker[.] -> temp_S_D_A_ticker
        
        temp_S_D_A_ticker %>% c(.,rep(NA,(Output_Col_Nums-1-length(.)))) %>% 
          c(temp_S_A_date,.) -> temp_temp_output_file[1,]
        
        temp_temp_output_file %>% rbind(temp_output_file,.) -> temp_output_file
      }else{
        "Futures bhavcopy file not found for date " %>% paste0(.,temp_S_A_date,sep='') %>% print
      } # End of 'if(exists(relevant_bhav_copy))'
    } # End of 'for (i_1 in 1:nrow(temp_S_A_file))'
    
    # Saving 'temp_output_file'
    if(nrow(temp_output_file) > 0)
    {
      "Saving NEW output for Index '" %>% 
        paste0(temp_index,"', file: '",Output_filename,"'",sep='') %>% print
      
      Survivorship_Derivative_Adjustments_Database %>% setwd
      
      temp_output_file %>% 
        write.csv(.,file=Output_filename,na = "",row.names = FALSE)
    }else{
      "No output could be made/saved for Index '" %>% 
        paste0(temp_index,"'",sep='') %>% print
    } # End of 'if(nrow(temp_output_file) > 0)'
    
  }else{
    "Survivorship Adjusted file NOT found for '" %>% 
      paste0(.,temp_index,"'",sep='') %>% print
  } # End of 'if(exists('temp_S_A_filename'))'
  
} # End of 'if(Output_filename %in% Survivorship_Derivative_Adjustments_Database_Files)'

#################################################################################
#################################################################################

#################################################################################
#################################### Goals ######################################

# Date: 2020-June-26
# Author: Arunabha Sarkar

# Goals: Survivor & Derivative Adjustment Nifty Midcap Next 50
# File Name: Survivor_Derivative_Adjustment_Nifty_Midcap_Next_50

#################################################################################
#################################################################################

#################################################################################
##################### Initializing and loading Libraries ########################

library(dplyr)
library(purrr)

#################################################################################
#################################################################################

#################################################################################
################## Set Directories & other Hyperparameters ######################

Output_Col_Nums <- 51

Output_filename <- 'Survivor_Derivative_Adjustment_Nifty_Midcap_Next_50_Tickers.csv'

"C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% paste0("/NSE FnO Bhav Copies",sep='') -> FnO_Bhav_Database

Central_Database %>% paste0("/Nifty 50 Historical Data Investing dot com",
                            sep='') -> Nifty50_Database  # To find out which day is open & NIFTY Spot

Central_Database %>% 
  paste0("/Survivorship Adjustments Indexes",sep='') -> Survivorship_Adjustments_Database

Central_Database %>% 
  paste0("/Survivorship Derivative Adjustments Indexes",sep='') -> Survivorship_Derivative_Adjustments_Database

Central_Database %>% setwd

if(!file.exists('Survivorship Derivative Adjustments Indexes'))
{
  print("Creating 'Survivorship Derivative Adjustments Indexes' directory.")
  dir.create(file.path(Central_Database, 'Survivorship Derivative Adjustments Indexes'))
}else{
  print("'Survivorship Derivative Adjustments Indexes' directory already exists.")
} # End of 'if(!file.exists('Survivorship Derivative Adjustments Indexes'))'

temp_FnO_colnames <- c("INSTRUMENT","SYMBOL","EXPIRY_DT","STRIKE_PR","OPTIONTYPE",
                       "OPEN","HIGH","LOW","CLOSE","SETTLE_PR","CONTRACTS","VAL_INLAKH",
                       "OPEN_INT","CHG_IN_OI","TIMESTAMP")

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
################### Finding if output file already exists #######################

Survivorship_Derivative_Adjustments_Database %>% setwd

list.files() -> Survivorship_Derivative_Adjustments_Database_Files

if(Output_filename %in% Survivorship_Derivative_Adjustments_Database_Files)
{
  Output_filename %>% paste0("'",.,"' existing file found",sep='') %>% print
  
  Survivorship_Derivative_Adjustments_Database %>% setwd
  Output_filename %>% read.csv -> Old_file
  
  # Deleting last 200 entries from Old_file
  Old_file %>% nrow %>% {.-200} %>% seq(1,.,1) %>% Old_file[.,] -> Old_file
  
  {which(as.numeric(N50_Dates_YYYYMMDD) > max(as.numeric(Old_file$Dates_YYYYMMDD)))} %>% 
    N50_Dates_YYYYMMDD[.] -> Relevant_Dates # This construct will get all the new dates as well
  
  Old_file %>% colnames %>% length %>% matrix(data = NA, nrow = length(Relevant_Dates), ncol = .) %>% 
    data.frame %>% `colnames<-`(colnames(Old_file)) -> temp_temp_new
  
  temp_temp_new$Dates_YYYYMMDD <- Relevant_Dates
  
  Output_filename %>% 
    gsub("Survivor_Derivative_Adjustment_","",.) %>% 
    gsub("_Tickers.csv","",.) -> temp_index
  
  #### Also load 'temp_S_A_file' for dates in 'temp_temp_new'
  Survivorship_Adjustments_Database %>% setwd
  
  list.files() -> Survivorship_Adjustments_Database_Files
  
  Survivorship_Adjustments_Database_Files %>% grep(temp_index,.) %>% 
    Survivorship_Adjustments_Database_Files[.] %>% read.csv -> temp_S_A_file
  
  which(as.numeric(temp_S_A_file$Dates_YYYYMMDD) >= min(Relevant_Dates)) %>% 
    temp_S_A_file[.,] -> temp_S_A_file
  
  temp_S_A_file %>% colnames -> output_colnames
  
  output_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
    `colnames<-`(output_colnames) -> temp_output_file
  
  # i_2 <- 1
  for (i_2 in 1:nrow(temp_temp_new))
  {
    "For Index " %>% paste0(.,temp_index,", processing row number ",i_2,
                            " of ",nrow(temp_temp_new), sep='') %>% print
    
    temp_S_A_file[,-1] %>% .[i_2,] %>% unlist %>% as.character -> temp_S_A_ticker
    
    temp_S_A_file[,1] %>% .[i_2] %>% as.character -> temp_S_A_date
    
    FnO_Bhav_Database %>% setwd
    list.files() -> FnO_Bhav_Database_Files
    
    FnO_Bhav_Database_Files %>% as.Date(format="fo%d%b%Ybhav.csv") %>% format("%Y%m%d") %>% 
      as.character -> FnO_Bhav_Database_Dates
    
    which(FnO_Bhav_Database_Dates == temp_S_A_date) %>% FnO_Bhav_Database_Files[.] -> relevant_bhav_copy
    
    if(exists('relevant_bhav_copy'))
    {
      FnO_Bhav_Database %>% setwd
      
      output_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
        `colnames<-`(output_colnames) -> temp_temp_output_file
      
      relevant_bhav_copy %>% read.csv %>% .[,1:length(temp_FnO_colnames)] %>% 
        `colnames<-`(temp_FnO_colnames) %>% select(SYMBOL) %>% unlist %>% 
        as.character %>% unique -> temp_D_ticker
      
      which(temp_S_A_ticker %in% temp_D_ticker) %>% temp_S_A_ticker[.] -> temp_S_D_A_ticker
      
      temp_S_D_A_ticker %>% c(.,rep(NA,(Output_Col_Nums-1-length(.)))) %>% 
        c(temp_S_A_date,.) -> temp_temp_output_file[1,]
      
      temp_temp_output_file %>% rbind(temp_output_file,.) -> temp_output_file
    }else{
      "Futures bhavcopy file not found for date " %>% paste0(.,temp_S_A_date,sep='') %>% print
    } # End of 'if(exists(relevant_bhav_copy))'
  } # End of 'for (i_2 in 1:nrow(temp_S_A_file))'
  
  Old_file %>% rbind(.,temp_output_file) -> temp_new
  
  # Save the file, delete the old as well
  Survivorship_Derivative_Adjustments_Database %>% setwd
  print(paste0("Overwriting '",Output_filename,"'",sep=''))
  file.remove(Output_filename)
  temp_new %>% write.csv(.,file=Output_filename,na = "",row.names = FALSE)
}else{
  Output_filename %>% paste0("'",.,"' existing file NOT found",sep='') %>% print
  
  Output_Col_Nums %>% {.-1} %>% seq(1,.,1) %>% paste0("Company_",.,sep='') %>% 
    c("Dates_YYYYMMDD",.) -> output_colnames
  
  output_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
    `colnames<-`(output_colnames) -> temp_output_file
  
  Survivorship_Adjustments_Database %>% setwd
  
  list.files() -> Survivorship_Adjustments_Database_Files
  
  Output_filename %>% 
    gsub("Survivor_Derivative_Adjustment_","",.) %>% 
    gsub("_Tickers.csv","",.) -> temp_index
  
  Survivorship_Adjustments_Database_Files %>% grep(temp_index,.) %>% 
    Survivorship_Adjustments_Database_Files[.] -> temp_S_A_filename
  
  if(exists('temp_S_A_filename'))
  {
    Survivorship_Adjustments_Database %>% setwd
    
    temp_S_A_filename %>% read.csv -> temp_S_A_file
    
    # i_1 <- 1
    for (i_1 in 1:nrow(temp_S_A_file))
    {
      "For Index " %>% paste0(.,temp_index,", processing row number ",i_1,
                              " of ",nrow(temp_S_A_file), sep='') %>% print
      
      temp_S_A_file[,-1] %>% .[i_1,] %>% unlist %>% as.character -> temp_S_A_ticker
      
      temp_S_A_file[,1] %>% .[i_1] %>% as.character -> temp_S_A_date
      
      FnO_Bhav_Database %>% setwd
      list.files() -> FnO_Bhav_Database_Files
      
      FnO_Bhav_Database_Files %>% as.Date(format="fo%d%b%Ybhav.csv") %>% format("%Y%m%d") %>% 
        as.character -> FnO_Bhav_Database_Dates
      
      which(FnO_Bhav_Database_Dates == temp_S_A_date) %>% FnO_Bhav_Database_Files[.] -> relevant_bhav_copy
      
      if(exists('relevant_bhav_copy'))
      {
        FnO_Bhav_Database %>% setwd
        
        output_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
          `colnames<-`(output_colnames) -> temp_temp_output_file
        
        relevant_bhav_copy %>% read.csv %>% .[,1:length(temp_FnO_colnames)] %>% 
          `colnames<-`(temp_FnO_colnames) %>% select(SYMBOL) %>% unlist %>% 
          as.character %>% unique -> temp_D_ticker
        
        which(temp_S_A_ticker %in% temp_D_ticker) %>% temp_S_A_ticker[.] -> temp_S_D_A_ticker
        
        temp_S_D_A_ticker %>% c(.,rep(NA,(Output_Col_Nums-1-length(.)))) %>% 
          c(temp_S_A_date,.) -> temp_temp_output_file[1,]
        
        temp_temp_output_file %>% rbind(temp_output_file,.) -> temp_output_file
      }else{
        "Futures bhavcopy file not found for date " %>% paste0(.,temp_S_A_date,sep='') %>% print
      } # End of 'if(exists(relevant_bhav_copy))'
    } # End of 'for (i_1 in 1:nrow(temp_S_A_file))'
    
    # Saving 'temp_output_file'
    if(nrow(temp_output_file) > 0)
    {
      "Saving NEW output for Index '" %>% 
        paste0(temp_index,"', file: '",Output_filename,"'",sep='') %>% print
      
      Survivorship_Derivative_Adjustments_Database %>% setwd
      
      temp_output_file %>% 
        write.csv(.,file=Output_filename,na = "",row.names = FALSE)
    }else{
      "No output could be made/saved for Index '" %>% 
        paste0(temp_index,"'",sep='') %>% print
    } # End of 'if(nrow(temp_output_file) > 0)'
    
  }else{
    "Survivorship Adjusted file NOT found for '" %>% 
      paste0(.,temp_index,"'",sep='') %>% print
  } # End of 'if(exists('temp_S_A_filename'))'
  
} # End of 'if(Output_filename %in% Survivorship_Derivative_Adjustments_Database_Files)'

#################################################################################
#################################################################################

#################################################################################
#################################### Goals ######################################

# Date: 2020-June-26
# Author: Arunabha Sarkar

# Goals: Survivor & Derivative Adjustment Nifty 200
# File Name: Survivor_Derivative_Adjustment_Nifty_200

#################################################################################
#################################################################################

#################################################################################
##################### Initializing and loading Libraries ########################

library(dplyr)
library(purrr)

#################################################################################
#################################################################################

#################################################################################
################## Set Directories & other Hyperparameters ######################

Output_Col_Nums <- 201

Output_filename <- 'Survivor_Derivative_Adjustment_Nifty_200_Tickers.csv'

"C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

Central_Database %>% paste0("/NSE FnO Bhav Copies",sep='') -> FnO_Bhav_Database

Central_Database %>% paste0("/Nifty 50 Historical Data Investing dot com",
                            sep='') -> Nifty50_Database  # To find out which day is open & NIFTY Spot

Central_Database %>% 
  paste0("/Survivorship Adjustments Indexes",sep='') -> Survivorship_Adjustments_Database

Central_Database %>% 
  paste0("/Survivorship Derivative Adjustments Indexes",sep='') -> Survivorship_Derivative_Adjustments_Database

Central_Database %>% setwd

if(!file.exists('Survivorship Derivative Adjustments Indexes'))
{
  print("Creating 'Survivorship Derivative Adjustments Indexes' directory.")
  dir.create(file.path(Central_Database, 'Survivorship Derivative Adjustments Indexes'))
}else{
  print("'Survivorship Derivative Adjustments Indexes' directory already exists.")
} # End of 'if(!file.exists('Survivorship Derivative Adjustments Indexes'))'

temp_FnO_colnames <- c("INSTRUMENT","SYMBOL","EXPIRY_DT","STRIKE_PR","OPTIONTYPE",
                       "OPEN","HIGH","LOW","CLOSE","SETTLE_PR","CONTRACTS","VAL_INLAKH",
                       "OPEN_INT","CHG_IN_OI","TIMESTAMP")

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
################### Finding if output file already exists #######################

Survivorship_Derivative_Adjustments_Database %>% setwd

list.files() -> Survivorship_Derivative_Adjustments_Database_Files

if(Output_filename %in% Survivorship_Derivative_Adjustments_Database_Files)
{
  Output_filename %>% paste0("'",.,"' existing file found",sep='') %>% print
  
  Survivorship_Derivative_Adjustments_Database %>% setwd
  Output_filename %>% read.csv -> Old_file
  
  # Deleting last 200 entries from Old_file
  Old_file %>% nrow %>% {.-200} %>% seq(1,.,1) %>% Old_file[.,] -> Old_file
  
  {which(as.numeric(N50_Dates_YYYYMMDD) > max(as.numeric(Old_file$Dates_YYYYMMDD)))} %>% 
    N50_Dates_YYYYMMDD[.] -> Relevant_Dates # This construct will get all the new dates as well
  
  Old_file %>% colnames %>% length %>% matrix(data = NA, nrow = length(Relevant_Dates), ncol = .) %>% 
    data.frame %>% `colnames<-`(colnames(Old_file)) -> temp_temp_new
  
  temp_temp_new$Dates_YYYYMMDD <- Relevant_Dates
  
  Output_filename %>% 
    gsub("Survivor_Derivative_Adjustment_","",.) %>% 
    gsub("_Tickers.csv","",.) -> temp_index
  
  #### Also load 'temp_S_A_file' for dates in 'temp_temp_new'
  Survivorship_Adjustments_Database %>% setwd
  
  list.files() -> Survivorship_Adjustments_Database_Files
  
  Survivorship_Adjustments_Database_Files %>% grep(temp_index,.) %>% 
    Survivorship_Adjustments_Database_Files[.] %>% read.csv -> temp_S_A_file
  
  which(as.numeric(temp_S_A_file$Dates_YYYYMMDD) >= min(Relevant_Dates)) %>% 
    temp_S_A_file[.,] -> temp_S_A_file
  
  temp_S_A_file %>% colnames -> output_colnames
  
  output_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
    `colnames<-`(output_colnames) -> temp_output_file
  
  # i_2 <- 1
  for (i_2 in 1:nrow(temp_temp_new))
  {
    "For Index " %>% paste0(.,temp_index,", processing row number ",i_2,
                            " of ",nrow(temp_temp_new), sep='') %>% print
    
    temp_S_A_file[,-1] %>% .[i_2,] %>% unlist %>% as.character -> temp_S_A_ticker
    
    temp_S_A_file[,1] %>% .[i_2] %>% as.character -> temp_S_A_date
    
    FnO_Bhav_Database %>% setwd
    list.files() -> FnO_Bhav_Database_Files
    
    FnO_Bhav_Database_Files %>% as.Date(format="fo%d%b%Ybhav.csv") %>% format("%Y%m%d") %>% 
      as.character -> FnO_Bhav_Database_Dates
    
    which(FnO_Bhav_Database_Dates == temp_S_A_date) %>% FnO_Bhav_Database_Files[.] -> relevant_bhav_copy
    
    if(exists('relevant_bhav_copy'))
    {
      FnO_Bhav_Database %>% setwd
      
      output_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
        `colnames<-`(output_colnames) -> temp_temp_output_file
      
      relevant_bhav_copy %>% read.csv %>% .[,1:length(temp_FnO_colnames)] %>% 
        `colnames<-`(temp_FnO_colnames) %>% select(SYMBOL) %>% unlist %>% 
        as.character %>% unique -> temp_D_ticker
      
      which(temp_S_A_ticker %in% temp_D_ticker) %>% temp_S_A_ticker[.] -> temp_S_D_A_ticker
      
      temp_S_D_A_ticker %>% c(.,rep(NA,(Output_Col_Nums-1-length(.)))) %>% 
        c(temp_S_A_date,.) -> temp_temp_output_file[1,]
      
      temp_temp_output_file %>% rbind(temp_output_file,.) -> temp_output_file
    }else{
      "Futures bhavcopy file not found for date " %>% paste0(.,temp_S_A_date,sep='') %>% print
    } # End of 'if(exists(relevant_bhav_copy))'
  } # End of 'for (i_2 in 1:nrow(temp_S_A_file))'
  
  Old_file %>% rbind(.,temp_output_file) -> temp_new
  
  # Save the file, delete the old as well
  Survivorship_Derivative_Adjustments_Database %>% setwd
  print(paste0("Overwriting '",Output_filename,"'",sep=''))
  file.remove(Output_filename)
  temp_new %>% write.csv(.,file=Output_filename,na = "",row.names = FALSE)
}else{
  Output_filename %>% paste0("'",.,"' existing file NOT found",sep='') %>% print
  
  Output_Col_Nums %>% {.-1} %>% seq(1,.,1) %>% paste0("Company_",.,sep='') %>% 
    c("Dates_YYYYMMDD",.) -> output_colnames
  
  output_colnames %>% length %>% matrix(data=NA,nrow=0,ncol=.) %>% data.frame %>% 
    `colnames<-`(output_colnames) -> temp_output_file
  
  Survivorship_Adjustments_Database %>% setwd
  
  list.files() -> Survivorship_Adjustments_Database_Files
  
  Output_filename %>% 
    gsub("Survivor_Derivative_Adjustment_","",.) %>% 
    gsub("_Tickers.csv","",.) -> temp_index
  
  Survivorship_Adjustments_Database_Files %>% grep(temp_index,.) %>% 
    Survivorship_Adjustments_Database_Files[.] -> temp_S_A_filename
  
  if(exists('temp_S_A_filename'))
  {
    Survivorship_Adjustments_Database %>% setwd
    
    temp_S_A_filename %>% read.csv -> temp_S_A_file
    
    # i_1 <- 1
    for (i_1 in 1:nrow(temp_S_A_file))
    {
      "For Index " %>% paste0(.,temp_index,", processing row number ",i_1,
                              " of ",nrow(temp_S_A_file), sep='') %>% print
      
      temp_S_A_file[,-1] %>% .[i_1,] %>% unlist %>% as.character -> temp_S_A_ticker
      
      temp_S_A_file[,1] %>% .[i_1] %>% as.character -> temp_S_A_date
      
      FnO_Bhav_Database %>% setwd
      list.files() -> FnO_Bhav_Database_Files
      
      FnO_Bhav_Database_Files %>% as.Date(format="fo%d%b%Ybhav.csv") %>% format("%Y%m%d") %>% 
        as.character -> FnO_Bhav_Database_Dates
      
      which(FnO_Bhav_Database_Dates == temp_S_A_date) %>% FnO_Bhav_Database_Files[.] -> relevant_bhav_copy
      
      if(exists('relevant_bhav_copy'))
      {
        FnO_Bhav_Database %>% setwd
        
        output_colnames %>% length %>% matrix(data=NA,nrow=1,ncol=.) %>% data.frame %>% 
          `colnames<-`(output_colnames) -> temp_temp_output_file
        
        relevant_bhav_copy %>% read.csv %>% .[,1:length(temp_FnO_colnames)] %>% 
          `colnames<-`(temp_FnO_colnames) %>% select(SYMBOL) %>% unlist %>% 
          as.character %>% unique -> temp_D_ticker
        
        which(temp_S_A_ticker %in% temp_D_ticker) %>% temp_S_A_ticker[.] -> temp_S_D_A_ticker
        
        temp_S_D_A_ticker %>% c(.,rep(NA,(Output_Col_Nums-1-length(.)))) %>% 
          c(temp_S_A_date,.) -> temp_temp_output_file[1,]
        
        temp_temp_output_file %>% rbind(temp_output_file,.) -> temp_output_file
      }else{
        "Futures bhavcopy file not found for date " %>% paste0(.,temp_S_A_date,sep='') %>% print
      } # End of 'if(exists(relevant_bhav_copy))'
    } # End of 'for (i_1 in 1:nrow(temp_S_A_file))'
    
    # Saving 'temp_output_file'
    if(nrow(temp_output_file) > 0)
    {
      "Saving NEW output for Index '" %>% 
        paste0(temp_index,"', file: '",Output_filename,"'",sep='') %>% print
      
      Survivorship_Derivative_Adjustments_Database %>% setwd
      
      temp_output_file %>% 
        write.csv(.,file=Output_filename,na = "",row.names = FALSE)
    }else{
      "No output could be made/saved for Index '" %>% 
        paste0(temp_index,"'",sep='') %>% print
    } # End of 'if(nrow(temp_output_file) > 0)'
    
  }else{
    "Survivorship Adjusted file NOT found for '" %>% 
      paste0(.,temp_index,"'",sep='') %>% print
  } # End of 'if(exists('temp_S_A_filename'))'
  
} # End of 'if(Output_filename %in% Survivorship_Derivative_Adjustments_Database_Files)'

#################################################################################
#################################################################################

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

"C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

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