searchGlobal<-function(data, alphaBound, betaBound, startPoints, initialQ){
  # This function tries to find the global maximum of the log likelihood
  # 
  # Input
  #    Data: a long dataset where each row represents a trial.
  #    alphaBound<- a two-element vector with upper and lower boundaries for the alpha parameter     
  #    betaBound<- a two-element vector with upper and lower boundaries for the beta parameter 
  #    startPoints<- number of starting points: it determines how many times the optimization algorithm is initiated 
  #                 with different starting points
  #    initialQ<-initial Q values
  #
  # Output
  #   A list object with: "alpha" = learning rate
  #                       "beta"= inverse temperature
  #                       "logLikel"= log-likelihood for the parameter estimated 
  #                       "BIC"= BIC for the parameter estimated
  # 
  # ------------------------
  
  # create progress bar
  pb<-txtProgressBar(min=0, max=startPoints)
  # create matrix to store value
  
  Matrix<-matrix(NA, nrow = startPoints, ncol=4)
  colnames(Matrix)<-c("alpha", "beta", "logLikel", "BIC")
  # You should definitely parameterize this. I.e., add a variable in 06.Parameter_estimation which allows us to change these values.
  # For debugging purposes, I set it to 1
  for (i in 1:startPoints){
    fit<-fit_RescorlaWagner_feedb(data,alphaBound,betaBound , initialQ)
    Matrix[i,]<-c(fit[[1]][1], fit[[1]][2],fit[[2]], fit[[3]] )
    
    #progress bar
    setTxtProgressBar(pb, i) 
    
  }
  # which is the maximum for the best fitting parameters?
  max<-which(Matrix[,3]==max(Matrix[,3]))
  alpha<-Matrix[max,1][1]
  beta<-Matrix[max,2][1]
  loglik<-Matrix[max,3][1]
  BIC<-Matrix[max, 4][1]
  
  print("Finished parameter estimation")
  
  return(list("alpha"=as.numeric(alpha),"beta"=as.numeric(beta), "logLikel"=as.numeric(loglik), "BIC"=as.numeric(BIC) ))
  
}