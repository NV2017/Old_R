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

"C:/Users/aurnabha/Desktop/Central Database" -> Central_Database

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