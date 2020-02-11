# 2018.12/30 from "PLSE via SCAD.R" 
#
library(MASS)
rm(list = ls())
#grnerating data
beta1 <- c(3 ,1.5 ,2 ,-7 , 15) 
beta2 <- rep(0 ,5)  
par <- c(beta1 ,beta2) #真實參數

N <- 100 
p <- length(par)

Sigma <- matrix(0.5 ,p ,p ) + diag(0.5 ,p ,p)
set.seed(1)
design.x <- matrix( mvrnorm(N ,rep(0, p) ,Sigma),
                    N ,p )   
# design.x <- matrix( runif(N*p ,-10 ,10) ,N ,p) # matrix
set.seed(2)
error <- rnorm(N ,0 , 3)            # vector
obs.y <- design.x %*% par + error     # model  

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

#penalized least square
P.LSE <- function(b){ t( obs.y - design.x %*% b ) %*% ( obs.y - design.x %*% b ) + N * sum(penalty(b)) }

# K-fold Cross-validation choose lambda
K <- 10

lambda_n <- c(0 ,exp( seq(log(10^-2), log(10), length.out = 20 ) ))

t1 <- Sys.time()
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
    design.x_test  <- design.x[ testIndexes, ]
    design.x_train <- design.x[-testIndexes, ]
    obs.y_test  <- obs.y[ testIndexes ]
    obs.y_train <- obs.y[-testIndexes ]
    
    #penalized least square
    P.LSE <- function(b){ t( obs.y_train - design.x_train %*% b ) %*% ( obs.y_train - design.x_train %*% b ) + N * sum(penalty(b)) }
    
    beta.hat <- optim(rep(0 ,length(par) ) ,P.LSE ,method = "BFGS")$par
    # OLS estimator
    # b <- as.numeric( solve( t(obs.B) %*% obs.B ) %*% t(obs.B) %*% y )
    number_zero[i] <- sum( abs(beta.hat) < 10^-3 )
    
    pred.y <- as.numeric( design.x_test %*% beta.hat )
    pred.error[i] <- sum( ( obs.y_test - pred.y )^2 ) / length(obs.y_test)
    
  }
  CV_lambda[j] <- sum(pred.error) / K
  aveg.number_zero[j] <- sum(number_zero) / K
}
#choosing lambda with smallest CV_value
lambda <- lambda_n[which(CV_lambda == min(CV_lambda))]
t2 <- Sys.time() - t1
res <- list(lambda = lambda ,
            CV_lambda = CV_lambda,
            aveg.number_zero = aveg.number_zero,
            lambda_n = lambda_n )
which(CV_lambda == min(CV_lambda))

#Simulation
#penalized least square
P.LSE <- function(b){ t( obs.y - design.x %*% b ) %*% ( obs.y - design.x %*% b ) + N * sum(penalty(b)) }
lambda_n <- c(0 ,lambda ,2 *lambda ,3 *lambda ,4 *lambda)

t1 <- Sys.time()
DD <- DDsqr <- CC <- matrix(0 ,length(lambda_n) ,p)
c <- matrix(0 ,1 ,p)
for(i in 1:100){
  
  design.x <- matrix( mvrnorm(N ,rep(0, p) ,Sigma),
                      N ,p )   
  error <- rnorm(N ,0 , 3)              # vector
  obs.y <- design.x %*% par + error     # model  
  
  
  
  D <- C <- matrix( 0 ,1 ,p)
  for(j in 1:length(lambda_n)){
    
    lambda <- lambda_n[j]
    
    a <- optim(rep(0,p) ,P.LSE ,method ="BFGS" )$par
    
    D <- rbind(D ,a)
    for(k in 1:p){
      if(abs(a[k]) > 1e-3) c[k]=c[k]+1   }          #counting
    C <- rbind(C,c) 
    c <- matrix(0,1,p)
  }
  D <- D[-1,]
  C <- C[-1,]
  DD <- DD + D 
  CC <- CC + C
  DDsqr <- DDsqr + D^2
}
DDbar <- DD / 100
CCbar <- CC / 100
DDsd <- sqrt( (DDsqr - 100 *DDbar^2 ) / (100-1) )
t2 <- Sys.time()
#
Bias <- DDbar - matrix(par ,length(lambda_n) ,p ,byrow = T)

##############################################################################
# LASSO
rm(list = ls())
#grnerating data
beta1 <- c(3 ,1.5 ,2 ,-7 , 15) 
beta2 <- rep(0 ,5)  
par <- c(beta1 ,beta2) #真實參數

N <- 100 
p <- length(par)

Sigma <- matrix(0.5 ,p ,p ) + diag(0.5 ,p ,p)
set.seed(1)
design.x <- matrix( mvrnorm(N ,rep(0, p) ,Sigma),
                    N ,p )   
# design.x <- matrix( runif(N*p ,-10 ,10) ,N ,p) # matrix
set.seed(2)
error <- rnorm(N ,0 , 3)            # vector
obs.y <- design.x %*% par + error     # model  

#penalized least square
P.LSE <- function(b){ t( obs.y - design.x %*% b ) %*% ( obs.y - design.x %*% b ) + N * lambda * sum(abs(b)) }

# K-fold Cross-validation choose lambda
K <- 10

lambda_n <- c(0 ,exp( seq(log(10^-2), log(10), length.out = 20 ) ))

t1 <- Sys.time()
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
    design.x_test  <- design.x[ testIndexes, ]
    design.x_train <- design.x[-testIndexes, ]
    obs.y_test  <- obs.y[ testIndexes ]
    obs.y_train <- obs.y[-testIndexes ]

    #penalized least square
    P.LSE <- function(b){ t( obs.y_train - design.x_train %*% b ) %*% ( obs.y_train - design.x_train %*% b ) + N * lambda * sum(abs(b)) }

    beta.hat <- optim(rep(0 ,length(par) ) ,P.LSE ,method = "BFGS")$par
    # OLS estimator
    # b <- as.numeric( solve( t(obs.B) %*% obs.B ) %*% t(obs.B) %*% y )
    number_zero[i] <- sum( abs(beta.hat) < 10^-3 )

    pred.y <- as.numeric( design.x_test %*% beta.hat )
    pred.error[i] <- sum( ( obs.y_test - pred.y )^2 ) / length(obs.y_test)

  }
  CV_lambda[j] <- sum(pred.error) / K
  aveg.number_zero[j] <- sum(number_zero) / K
}
#choosing lambda with smallest CV_value
lambda <- lambda_n[which(CV_lambda == min(CV_lambda))]
t2 <- Sys.time() - t1
res <- list(lambda = lambda ,
            CV_lambda = CV_lambda,
            aveg.number_zero = aveg.number_zero,
            lambda_n = lambda_n )
which(CV_lambda == min(CV_lambda))

#Simulation
#penalized least square
P.LSE <- function(b){ t( obs.y - design.x %*% b ) %*% ( obs.y - design.x %*% b ) + N * lambda * sum(abs(b)) }
lambda_n <- c(0 ,lambda ,2 *lambda ,3 *lambda, 4 *lambda)

t1 <- Sys.time()
DD <- DDsqr <- CC <- matrix(0 ,length(lambda_n) ,p)
c <- matrix(0 ,1 ,p)
for(i in 1:100){
  
  design.x <- matrix( mvrnorm(N ,rep(0, p) ,Sigma),
                      N ,p )   
  error <- rnorm(N ,0 , 3)              # vector
  obs.y <- design.x %*% par + error     # model  
  
  
  
  D <- C <- matrix( 0 ,1 ,p)
  for(j in 1:length(lambda_n)){
    
    lambda <- lambda_n[j]
    
    a <- optim(rep(0,p) ,P.LSE ,method ="BFGS" )$par
    
    D <- rbind(D ,a)
    for(k in 1:p){
      if(abs(a[k]) > 1e-3) c[k]=c[k]+1   }          #counting
    C <- rbind(C,c) 
    c <- matrix(0,1,p)
  }
  D <- D[-1,]
  C <- C[-1,]
  DD <- DD + D 
  CC <- CC + C
  DDsqr <- DDsqr + D^2
}
DDbar <- DD / 100
CCbar <- CC / 100
DDsd <- sqrt( (DDsqr - 100 *DDbar^2 ) / (100-1) )
t2 <- Sys.time()
#
Bias <- DDbar - matrix(par ,length(lambda_n) ,p ,byrow = T)
