library(stringr)
setwd("C:/Users/Master/Desktop/黃三/應用統計線性模型")
rm(list = ls())
data <- read.table("CH09TA01.txt")
colnames(data) <- c("X1","X2","X3","X4","X5","Y","lnY")

n <- dim(data)[1]
P <- 5            # 5 predict variable
# power set
powerset = function(s){
  len = length(s)
  l = vector(mode="list",length=2^len) ; l[[1]]=numeric()
  counter = 1
  for(x in 1:length(s)){
    for(subset in 1:counter){
      counter=counter+1
      l[[counter]] = c(l[[subset]],s[x])
    }
  }
  return(l)
}
powerset(1:P)
#
candidate <- powerset(1:P)[-1]

CV_value <- rep(0 ,length(candidate) )
for (j in 1:length(candidate) ) {
  
  pred.value <- rep(0 ,n)
  for(i in 1:n){
    d <- combn(1:n ,1)[,i] # CV(1)
    data_train <- data[-d ,]
    data_test <- data[ d ,]
    # training
    xnam <- paste0("X", candidate [[j]] )
    fmla <- as.formula(paste("Y ~ ", paste(xnam, collapse= "+")))
    model <- lm(fmla ,data_train) 
    # testing
    pred.value[i] <- as.numeric(
      (data_test$Y - as.matrix(cbind(1 ,data_test[,xnam])) %*% model$coefficients)^2 )
    
  }
  CV_value[j] <- sum(pred.value) / dim(combn(1:n ,1))[2]
  print(xnam)
}
paste0("X",candidate [[which(CV_value == min(CV_value)) ]] )
