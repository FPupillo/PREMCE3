# get flower
# get the flower selected by participant

getflow<-function(key){
  # this function take the key pressed by participant
  # and returns the column where the flower select can be found
  
  if (key=="up"){key<-"top"}else if (key=="down") {key<-"bottom"}
  name<-paste(key, "_flower", sep="")
  return(name)
}
