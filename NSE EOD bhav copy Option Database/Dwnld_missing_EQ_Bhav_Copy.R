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