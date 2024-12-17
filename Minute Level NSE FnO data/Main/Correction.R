library(dplyr)
"C:/Users/aurnabha/Desktop/New folder" -> Project_Folder
setwd(Project_Folder)
Project_Folder %>% paste0(.,"/Input",sep="")->Input_Folder
Project_Folder %>% paste0(.,"/Output",sep="")->Output_Folder
setwd(Input_Folder)
list.files()->Input_Folder_Files
if(length(Input_Folder_Files)>0)
{
  # i_Input_Folder_Files <- 1
  for(i_Input_Folder_Files in 1:length(Input_Folder_Files))
  {
    i_Input_Folder_Files %>% Input_Folder_Files[.] -> temp_input_filename
    "Executing " %>% paste0(.,i_Input_Folder_Files," out of ",
                            length(Input_Folder_Files),sep="") %>% print
    setwd(Output_Folder)
    list.files()->Output_Folder_Files
    if(!temp_input_filename %in% Output_Folder_Files)
    {
      setwd(Input_Folder)
      temp_input_filename %>% read.csv -> temp_input_Df
      temp_input_filename %>% strsplit(split="_") %>% .[[1]] %>% .[1] %>% 
        as.POSIXct(.,format="%Y%m%d%H%M%S") %>% as.character %>% 
        paste0(.," IST",sep="") -> temp_input_Df$DATE_TIME_ZONE
      
      setwd(Output_Folder)
      temp_input_Df %>% write.csv(.,file = temp_input_filename,row.names = F,na="")
    } # End of 'if(!temp_input_filename %in% Output_Folder_Files)'
  } # End of 'for(i_Input_Folder_Files in 1:length(Input_Folder_Files))'
} # End of 'if(length(Input_Folder_Files)>0)'