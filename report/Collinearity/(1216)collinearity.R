# # model 1:
# N <- 100
# set.seed(1)
# x1 <- runif(N ,0 ,2)
# set.seed(2)
# x2 <- runif(N ,-1 ,1)
# design.x <- cbind(x1 ,x2)
# set.seed(3)
# y <- rnorm(100 ,0 ,2)
# 
# summary( lm(y ~ x1 + x2) )
# model 2:
N <- 100
# set.seed(1)
x1 <- runif(N ,0 ,2)
# set.seed(2)
x2 <- x1 + rnorm(N ,0 ,0.001)
# set.seed(3)
error <- rnorm(N, 0, 0.1)
beta <- c(5 , -1 ,3)
y <- cbind(1 ,x1 ,x2) %*% beta + error


cor(x1 ,x2)
model <- lm(y ~ x1 + x2)
model$coefficients 
# summary(model)
#
x1 <- runif(N ,0 ,2)
# set.seed(2)
x2 <- x1 + rnorm(N ,0 ,0.1)
# set.seed(3)
error <- rnorm(N, 0, 0.1)
beta <- c(5 , -1 ,3)
y <- cbind(1 ,x1 ,x2) %*% beta + error
sum(( y - cbind(1 ,x1 ,x2) %*% model$coefficients  )^2 ) 
###################
N <- 2
# set.seed(1)
x1 <- runif(N ,0 ,2)
# set.seed(2)
x2 <- rnorm(N ,0 ,10)
# set.seed(3)
error <- rnorm(N, 0, 0.1)
beta <- c(5 , -1 ,3)
y <- cbind(1 ,x1 ,x2) %*% beta + error


cor(x1 ,x2)
model <- lm(y ~ x1 + x2)
summary(model)
