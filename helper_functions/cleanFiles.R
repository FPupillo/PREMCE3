# create a function to clean the data
# clear the workspace first
rm(list=ls())
# retrieve the function to select the files
source("data_files/selPart.R")

# retrieve one sample dataset for each part
# sel files
cd<-getwd()
setwd("data_files")
files<-selPart(1)
setwd(cd)

# second participant as sample
part1<-read.csv(paste("data_files/", files[2], sep=""))

# check the names of the variables
variables<-names(part1) 

# select the variables of Interest (VoI)
VoI<-c("participant","pract_resp.keys","pract_resp.corr" , "pract_resp.rt",                    
"first_butterfly"       ,      "second_butterfly"      ,      "Cue"   ,                      "left_flower"         ,       
 "top_flower"     ,      "bottom_flower"      ,      "right_flower"         ,       "corr_ans"       ,           
 "obj_file"           ,       "trial_cond"  ,"cuedButter", "task_resp.keys","task_resp.corr", 
"task_resp.rt" , "task_resp_2.keys", "task_resp_2.corr" )

# now clean the datafiles and put them in the new folder (data_files_clean)
for (j in 2: length(files)){
  # get file name
  filename<-paste( files[j], sep="")
  part1<-read.csv(paste("data_files/", filename, sep=""))
  part1<-part1[,VoI]
  write.csv(part1, paste("data_files_clean/",filename, sep=""), row.names = F)
}

# now do for part 2
# sel files
cd<-getwd()
setwd("data_files")
files<-selPart(2)
setwd(cd)

# first participant as sample
part1<-read.csv(paste("data_files/", files[1], sep=""))

# check the names of the variables
variables<-names(part1) 

VoI<-c("participant" , "first_butterfly"  ,    "second_butterfly"  ,  "warmup_resp.keys"  ,   "warmup_resp.corr" ,

       "warmup_resp.rt" ,     
       
       "Cue"           ,       "left_flower"      ,    "top_flower"  ,    "bottom_flower"  ,  "right_flower"    ,    
"corr_ans"         ,    "obj_file"       ,      "trial_cond" , "switch_cond" ,"task_resp_3.keys", "task_resp_3.corr" ,
"task_resp_3.rt"  )

for (j in 1: length(files)){
  # get file name
  filename<-paste( files[j], sep="")
  part1<-read.csv(paste("data_files/", filename, sep=""))
  part1<-part1[,VoI]
  write.csv(part1, paste("data_files_clean/",filename, sep=""), row.names = F)
}

# now do for part 3
# sel files
cd<-getwd()
setwd("data_files")
files<-selPart(3)
setwd(cd)

# first participant as sample
part1<-read.csv(paste("data_files/", files[1], sep=""))

# check the names of the variables
variables<-names(part1) 

VoI<-c("participant"    , "recog_resp.keys"         ,    
 "recog_resp.rt"     ,           "conf_resp.keys"          ,    "conf_resp.rt" ,"images",   "corr_ans"      ,
 "type"   )

for (j in 1: length(files)){
  # get file name
  filename<-paste( files[j], sep="")
  part1<-read.csv(paste("data_files/", filename, sep=""))
  part1<-part1[,VoI]
  write.csv(part1, paste("data_files_clean/",filename, sep=""), row.names = F)
}
