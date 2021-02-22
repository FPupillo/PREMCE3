# rename files
# function that rename the files for participants whose number is less than 10

# set the working directory
cd<-getwd()
setwd("data_files")

files<-list.files(pattern=".csv$")

for (i in 1:length(files)){
  # check if the participant number is less than 10
  partic<-sub("\\_.*", "", files[i])
  if (as.numeric(partic) <10){
    file.rename(files[i], paste("0", partic, substr(files[i], 2, nchar(files[i])), sep=""))
  }
}
