# get correct flower
# get the correct flower for each trial

getcorrFlow<-function(data){
  # this function take the data, look for the correct flower on each trails 
  # and returns the dataset with a new column represeting the correct flower
  data$corr_flower<-NA
  
  for (n in 1:nrow(data)){
    
    if (data$corr_ans[n]=="up"){
      data$corr_flower[n]<- data$top_flower[n]
      # substring it
      data$corr_flower[n]<-substr(data$corr_flower[n], 9, nchar(data$corr_flower[n])-4)
      
    }else if (data$corr_ans[n]=="left") {
      data$corr_flower[n]<- data$left_flower[n]
      # substring it
      data$corr_flower[n]<-substr(data$corr_flower[n], 9, nchar(data$corr_flower[n])-4)
      
    } else if (data$corr_ans[n]=="down") {
      
      data$corr_flower[n]<- data$bottom_flower[n]
      # substring it
      data$corr_flower[n]<-substr(data$corr_flower[n], 9, nchar(data$corr_flower[n])-4)
      
    } else if (data$corr_ans[n]=="right") {
      
      data$corr_flower[n]<- data$right_flower[n]
      # substring it
      data$corr_flower[n]<-substr(data$corr_flower[n], 9, nchar(data$corr_flower[n])-4)
      
    }
    
  }
  
  return(data)
}
