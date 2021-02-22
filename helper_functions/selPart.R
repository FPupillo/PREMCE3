# getFiles function
selPart<- function(part){
  # This function extracts the names of the files that belongs to  part 1, 2, or 3
  #
  # Input: Part
  #   1: select files of part 1
  #   2: select files of part 2
  #   3: select files of part 3
  #
  # Output:
  #   A string file containing the names of the files referring to the part selected (1,2,3)
  # ---------------------
  
  # # get current directory to reset the path later
  # setwd("data_files")
  
  # initialize variables
  part1Files<-vector()
  part2Files<-vector()
  part3Files<-vector()
  
  for (i in 1: length (list.files())){
    # check if the participant number is less than 10
   # partic<-sub("\\_.*", "", list.files()[i])
    
    # # if it is a number
    # if (!is.na(as.numeric(partic))){
    # if (as.numeric(partic) <10){
    # check<-substr((list.files()[i]), 10,23)} else { check<-substr((list.files()[i]), 11,24)}
    # 
    
     check<-substr((list.files()[i]), 15,28)
    
    if(check=="ses-01_part-01"){
      part1Files<- c(part1Files,(list.files()[i]))
    } else if (check=="ses-02_part-01") {
      part2Files<- c(part2Files,(list.files()[i]))
    } else if (check=="ses-02_part-02"){
      part3Files<- c(part3Files,(list.files()[i]))
    }
  }
  
  if (part==1){return(part1Files)
  } else if (part==2) {return(part2Files)
  } else if (part==3){return(part3Files)}
  
}