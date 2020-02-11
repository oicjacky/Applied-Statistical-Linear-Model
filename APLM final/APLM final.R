setwd("C:/Users/Master/Desktop/黃三/應用統計線性模型/APLM final")
rm(list = ls())
# read data
data <- read.table("FinalＤata.txt" ,sep = "," )
data <- as.data.frame( t(data) )
colnames(data) <- c( paste0("X" ,1:17) ,"Y")

sapply(data, range)

# linear model using LSE
#not include interpret term, since no investment no return.
xnam <- paste0("X", 1:17)
fmla <- as.formula(paste("Y ~ -1 +", paste(xnam, collapse= "+")))

model.linear <- lm(fmla ,data) #equal to lm(Y ~ . -1 ,data) 
summary(model.linear)
# # Residual Plot
# # (i) with predictors
# for (i in 1:17) {
#   windows()
#   i=4
#   plot(data[,i], model.linear$residuals ,xlab = paste0("X",i), ylab = "residual",main = paste0("X",i," and residual"))
# }
# # (ii) with fitted.values
# plot( model.linear$fitted.values , model.linear$residuals ,main = "fitted")
# # (iii) with i-th sample
# plot( 1:2000 , model.linear$residuals ,main = "i-th sample and residual",xlab = "i-th",ylab = "residual")
# # (iv) Q-Q plot
# qqnorm(model.linear$residuals)
# qqline(model.linear$residuals)
# # (v) X ~ Y scatter Plot
# for (i in 1:17) {
#   windows()
#   plot(data[,i], data[,18] ,xlab = paste0("X",i), ylab = "Y",main = paste0("X",i," and Y")  )
# }
# #Note: the important predictors: 3, 12 ,13 ,14

anova(model.linear)
# a <- anova(model.linear)
# MSR <- sum(a[1:17,2]) / 16 
# MSE <- a[18,2] / 1983
# Fvalue <- MSR / MSE


#####################
# Stepwise Regression
null <- lm( Y ~ -1 ,data)
full <- lm( Y ~ . ,data)
forward.lm = step(null, 
                  # 從空模型開始，一個一個丟變數，
                  # 最大不會超過完整的線性迴歸
                  # (一定要加上界 upper=full，不可以不加) 
                  scope=list(lower=null, upper=full), 
                  direction="forward")
summary(forward.lm)
both.lm = step(null, 
                  # 從空模型開始，一個一個丟變數，
                  # 最大不會超過完整的線性迴歸
                  # (一定要加上界 upper=full，不可以不加) 
                  scope=list(lower=null, upper=full), 
                  direction="both")
summary(both.lm)

#####################
# LASSO
# least squares with LASSO
LSE_LASSO <- function(b){
  t( y - obs.x %*% b ) %*% ( y - obs.x %*% b ) + N * lambda * sum(abs(b))  }

N <- dim(data)[1]

K_fold.CV.lasso <- function(K ,lambda_n){
  
  # least squares with LASSO
  LSE_LASSO <- function(b){
    t( y - obs.x %*% b ) %*% ( y - obs.x %*% b ) + N * lambda * sum(abs(b))  }
  
  # t1 <- proc.time()
  CV_lambda <- aveg.number_zero <- rep(0, length(lambda_n))
  
  for (j in 1:length(lambda_n)) {
    
    lambda <- lambda_n[j]
    
    pred.error <- number_zero <- rep(0 ,K)
    for (i in 1:K) {
      
      # split data into 10 sub-data
      #(i)Create 10 equally size folds
      folds <- cut(1:N ,breaks= K ,labels= F)
      #Perform 10 fold cross validation
      #Segement your data by fold using the which() function 
      
      testIndexes <- which(folds == i )
      test  <- data[ testIndexes ,]
      train <- data[-testIndexes ,]
      
      y <- train[ ,"Y"]
      y.test <- test[ ,"Y"]
      
      obs.x <- as.matrix( cbind(1 ,train[ ,-18]) )
      obs.x.test <- as.matrix( cbind(1 ,test[ ,-18]) )
      # OLS estimator
      beta.hat <- optim(rep(0 ,18) ,LSE_LASSO ,method = "BFGS")$par
      
      number_zero[i] <- sum( abs(beta.hat) < 10^-3 )
      
      pred.y <- as.numeric( obs.x.test %*% beta.hat )  #predict(model , obs.x.test)
      pred.error[i] <- sum( ( y.test - pred.y )^2 ) / length(y.test)
      
    }
    CV_lambda[j] <- sum(pred.error) / K
    aveg.number_zero[j] <- sum(number_zero) / K
  }
  #choosing lambda with smallest CV_value
  lambda <- lambda_n[which(CV_lambda == min(CV_lambda))]
  # t2 <- proc.time() - t1
  res <- list(lambda = lambda ,
              CV_lambda = CV_lambda,
              aveg.number_zero = aveg.number_zero,
              lambda_n = lambda_n )
  return(res)
}

cv.result.lasso <- K_fold.CV.lasso(
               10 , c(0, exp( seq(log(10^-5), log(1), length.out = 20 ) )) )
                       
lambda <- cv.result.lasso[[1]]
y <- data$Y
obs.x <- as.matrix( cbind(1 ,data[ ,-18]) )
beta.hat.lasso <- optim(rep(0 ,18) ,LSE_LASSO ,method = "BFGS")$par

beta.hat.lasso[ abs(beta.hat.lasso) < 10^-3] <- 0

names(beta.hat.lasso) <- c("1", paste0("X", 1:17) )
beta.hat.lasso <- beta.hat.lasso[ order(abs(beta.hat.lasso) ,decreasing = T) ]
mod.lasso <- names( which(beta.hat.lasso != 0) )

fmla <- as.formula(paste("Y ~ -1 +", paste(mod.lasso, collapse= "+")))
summary( lm(fmla ,data) )

# SCAD
# least squares with SCAD
# SCAD penalty
penalty <- function(x){
  y <- rep(0 ,length(x)) 
  #first
  u1 <- which( abs(x) <= lambda )
  y[u1] <- abs(x[u1]) *lambda
  #second
  u2 <- which( abs(x) > lambda & abs(x) <= 3.7 *lambda )
  y[u2] <- -( abs(x[u2])^2 - 2*3.7*lambda*abs(x[u2]) + lambda^2 ) / ( 2*(3.7-1) )
  #third
  u3 <- which( abs(x) > 3.7 *lambda ) 
  y[u3] <- rep( (3.7+1) *lambda^2 /2 ,length(u3) )
  return(y) 
}
# least squares with SCAD
LSE_SCAD <- function(b){
  t( y - obs.x %*% b ) %*% ( y - obs.x %*% b ) + N * sum(penalty(b))  }

N <- dim(data)[1]

K_fold.CV.scad <- function(K ,lambda_n){
  
  # SCAD penalty
  penalty <- function(x){
    y <- rep(0 ,length(x)) 
    #first
    u1 <- which( abs(x) <= lambda )
    y[u1] <- abs(x[u1]) *lambda
    #second
    u2 <- which( abs(x) > lambda & abs(x) <= 3.7 *lambda )
    y[u2] <- -( abs(x[u2])^2 - 2*3.7*lambda*abs(x[u2]) + lambda^2 ) / ( 2*(3.7-1) )
    #third
    u3 <- which( abs(x) > 3.7 *lambda ) 
    y[u3] <- rep( (3.7+1) *lambda^2 /2 ,length(u3) )
    return(y) 
  }
  # least squares with SCAD
  LSE_SCAD <- function(b){
    t( y - obs.x %*% b ) %*% ( y - obs.x %*% b ) + N * sum(penalty(b))  }
  
  # t1 <- proc.time()
  CV_lambda <- aveg.number_zero <- rep(0, length(lambda_n))
  
  for (j in 1:length(lambda_n)) {
    
    lambda <- lambda_n[j]
    
    pred.error <- number_zero <- rep(0 ,K)
    for (i in 1:K) {
      
      # split data into 10 sub-data
      #(i)Create 10 equally size folds
      folds <- cut(1:N ,breaks= K ,labels= F)
      #Perform 10 fold cross validation
      #Segement your data by fold using the which() function 
      
      testIndexes <- which(folds == i )
      test  <- data[ testIndexes ,]
      train <- data[-testIndexes ,]
      
      y <- train[ ,"Y"]
      y.test <- test[ ,"Y"]
      
      obs.x <- as.matrix( cbind(1 ,train[ ,-18]) )
      obs.x.test <- as.matrix( cbind(1 ,test[ ,-18]) )
      # OLS estimator
      beta.hat <- optim(rep(0 ,18) ,LSE_SCAD ,method = "BFGS")$par
      
      number_zero[i] <- sum( abs(beta.hat) < 10^-3 )
      
      pred.y <- as.numeric( obs.x.test %*% beta.hat )  #predict(model , obs.x.test)
      pred.error[i] <- sum( ( y.test - pred.y )^2 ) / length(y.test)
      
    }
    CV_lambda[j] <- sum(pred.error) / K
    aveg.number_zero[j] <- sum(number_zero) / K
  }
  #choosing lambda with smallest CV_value
  lambda <- lambda_n[which(CV_lambda == min(CV_lambda))]
  # t2 <- proc.time() - t1
  res <- list(lambda = lambda ,
              CV_lambda = CV_lambda,
              aveg.number_zero = aveg.number_zero,
              lambda_n = lambda_n )
  return(res)
}

cv.result.scad <- K_fold.CV.scad(
             10 ,  c(0, exp( seq(log(10^-5), log(1), length.out = 20 ) ))  )

lambda <- cv.result.scad[[1]]
y <- data$Y
obs.x <- as.matrix( cbind(1 ,data[ ,-18]) )

beta.hat.scad <- optim(rep(0 ,18) ,LSE_SCAD ,method = "BFGS")$par

beta.hat.scad[ abs(beta.hat.scad) < 10^-3] <- 0

names(beta.hat.scad) <- c("1", paste0("X", 1:17) )
beta.hat.scad <- beta.hat.scad[ order(abs(beta.hat.scad) ,decreasing = T) ]
mod.scad <- names( which(beta.hat.scad != 0) )

fmla <- as.formula(paste("Y ~ -1 +", paste(mod.scad, collapse= "+")))
summary( lm(fmla ,data) )
