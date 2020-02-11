setwd("C:/Users/Master/Desktop/黃三/應用統計線性模型/APLM final")
library(dplyr)
library(stringr)
rm(list = ls())
# read data
data <- read.table("FinalＤata.txt" ,sep = "," )
data <- as.data.frame( t(data) )
colnames(data) <- c( paste0("X" ,1:17) ,"Y")


# linear model using LSE
#not include interpret term, since no investment no return.
xnam <- paste0("X", 1:17)
fmla <- as.formula(paste("Y ~ ", paste(xnam, collapse= "+")))

model.linear <- lm(fmla ,data) #equal to lm(Y ~ . -1 ,data) 
summary(model.linear)
a <- summary(model.linear)

anova(model.linear)

null <- lm(Y ~ -1,data)

as.formula( paste0("Y ~ -1 + ",xnam ))

# Stepwise regression
alpha.enter <- 0.01
alpha.drop  <- 0.05
# null model(without interpret)
now <- c() 
current <- "Y ~ -1 "

step1 <- sapply(paste(current ,xnam ,sep = "+"), as.formula)
res1 <- lapply(step1,
               FUN = function(x){ 
                 mod <- summary(lm(x ,data) ) 
                 t <- mod$coefficients[,"t value"]
                 return(t)                }        )
# who has the largest t-value
if(res1[[ which(as.numeric(res1) == max( as.numeric(res1)) ) ]] > 
  qt(1 - alpha.enter/2 , 2 ,lower.tail = T) ){
  choosed <- paste0("X", which(as.numeric(res1) == max( as.numeric(res1)) ) ) 
  now <- c(now ,choosed)
  current <- paste(current, choosed ,sep = " + ") 
  #delete choosed one from "xnam"
  xnam <- xnam[- which(as.numeric(res1) == max( as.numeric(res1)) )]
  print(now)
}else{
  cat("the procedure stop here")   }

# X14 is included


number.now <- 0
while(number.now < length(now) ){
  number.now <- length(now)
  
  step2 <- sapply(paste(current ,xnam ,sep = " + " ), as.formula)
  res2 <- lapply(step2,
                 FUN = function(x){ 
                   mod.full <- lm(x ,data) 
                   mod.reduce <- lm(current ,data) 
                   SSE.full <- sum(mod.full$residuals^2)
                   SSE.reduce <- sum(mod.reduce$residuals^2)
                   t <- (SSE.reduce - SSE.full) / (SSE.full / (dim(data)[1] - mod.full$rank) )
                   t <- sqrt(t)
                   return(t)
                 }              )
  # who has the largest t-value
  
  if(res2[[ which(as.numeric(res2) == max( as.numeric(res2)) ) ]] > 
     qt(1 - alpha.enter/2 , dim(data)[1] - (length(now)+1) ,lower.tail = T) ){
    step2[which(as.numeric(res2) == max( as.numeric(res2)) ) ]
    choosed <- as.character( step2[which(as.numeric(res2) == max( as.numeric(res2)) ) ] ) %>% 
      word( -1 ,sep = fixed(" + ") )
    now <- c(now ,choosed)
    current <- paste(current, choosed ,sep = " + ") 
    #delete choosed one from "xnam"
    xnam <- xnam[- which(as.numeric(res2) == max( as.numeric(res2)) )]
    cat(now,",and ")
  }else{
    cat("The procedure stop here","\n") 
    break }
  
  # X3 is included
  
  del.candidate <- c(setdiff(now ,choosed)) 
  if(length(del.candidate) == 1){
    
    step3 <- paste("Y ~ -1 ",choosed ,sep = " + ") %>% sapply( as.formula)
    
    res3 <- lapply(step3,
                   FUN = function(x){ 
                     mod.full <- lm(current ,data) 
                     mod.reduce <- lm(x ,data) 
                     SSE.full <- sum(mod.full$residuals^2)
                     SSE.reduce <- sum(mod.reduce$residuals^2)
                     t <- (SSE.reduce - SSE.full) / (SSE.full / (dim(data)[1] - mod.full$rank) )
                     t <- sqrt(t)
                     return(t) }               )
  }else{
    
    step3 <- paste("Y ~ -1 ",choosed ,sep = " + ") %>% 
      paste( apply( matrix(apply(combn(length(del.candidate) ,length(del.candidate)-1) ,2
                                 ,FUN = function(x){ del.candidate[x] }), ncol = length(del.candidate))  
                    , 2, FUN = function(x){ paste(x ,collapse = " + ") } )   ,sep = " + ") %>% 
      sapply( as.formula)
    
    res3 <- lapply(step3,
                   FUN = function(x){ 
                     mod.full <- lm(current ,data) 
                     mod.reduce <- lm(x ,data) 
                     SSE.full <- sum(mod.full$residuals^2)
                     SSE.reduce <- sum(mod.reduce$residuals^2)
                     t <- (SSE.reduce - SSE.full) / (SSE.full / (dim(data)[1] - mod.full$rank) )
                     t <- sqrt(t)
                     return(t) }               )
  }
  # who has the largest t-value
  if(res3[[ which(as.numeric(res3) == max( as.numeric(res3)) ) ]] <= 
     qt(1 - alpha.drop/2 , dim(data)[1] - (length(del.candidate)+1) ,lower.tail = T) ){
    choosed <- paste0("X", which(as.numeric(res3) == max( as.numeric(res3)) ) ) 
    now <- c(now ,choosed)
    current <- paste(current, choosed ,sep = " + ") 
    #delete choosed one from "xnam"
    xnam <- xnam[- which(as.numeric(res3) == max( as.numeric(res3)) )]
    print(now)
  }else{
    cat("No predictor should be dropped now","\n")   }
}
cat("The final model select by Stepwise regression is ",now)

# final model selected by stepwise 
now <- c("X1","X3","X5","X6","X12","X13","X14")
fmla <- as.formula(paste("Y ~ -1 +", paste(now, collapse= "+")))
model.stepwise <- lm(fmla ,data)
summary(model.stepwise)

#
select.data <- data[ , now]

strage1 <- c(20000 ,0 ,10000 ,rep(0 ,4))
strage2 <- c(50000 ,20000 ,30000 ,rep(0 ,4))
# fitted value for 2 strages
Y.hat_str1 <- as.numeric( strage1 %*% model.stepwise$coefficients )
Y.hat_str2 <- as.numeric( strage2 %*% model.stepwise$coefficients )
# 95% CI for Y_strage
alpha <- 0.05
t_alpha <- qt(alpha/2 ,dim(select.data)[1] - dim(select.data)[2] ,lower.tail = F)
MSE <- sum(model.stepwise$residuals^2) / (dim(select.data)[1] - model.stepwise$rank)
X.matrix <- as.matrix(select.data)

upper <- Y.hat_str1 + t_alpha * sqrt(MSE *(1 + strage1 %*% solve(t(X.matrix) %*% X.matrix) %*% strage1) )
lower <- Y.hat_str1 - t_alpha * sqrt(MSE *(1 + strage1 %*% solve(t(X.matrix) %*% X.matrix) %*% strage1) )
CI.for.strage1 <- c(lower, upper)

upper <- Y.hat_str2 + t_alpha * sqrt(MSE *(1 + strage2 %*% solve(t(X.matrix) %*% X.matrix) %*% strage2) )
lower <- Y.hat_str2 - t_alpha * sqrt(MSE *(1 + strage2 %*% solve(t(X.matrix) %*% X.matrix) %*% strage2) )
CI.for.strage2 <- c(lower, upper)
