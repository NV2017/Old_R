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