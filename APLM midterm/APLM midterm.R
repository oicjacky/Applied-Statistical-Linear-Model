data <- read.table("CH01TA01.txt")
plot(data$V1 ,data$V2 )

# with interrupt
mod.1 <- lm(V2 ~ V1  ,data)
summary(mod.1)
abline(a = coefficients(mod.1)[1] , b = coefficients(mod.1)[2] ,col = "red")
# without interrupt
mod.2 <- lm(V2 ~ V1-1  ,data)
summary(mod.2)
abline(a = 0, b = coefficients(mod.2) ,col = "blue")
# residual's plot and sum
plot(data$V1 ,mod.1$residuals) 
plot(data$V1 ,mod.2$residuals) 
sum(mod.1$residuals)
sum(mod.2$residuals)

names(mod.1)

# 
summary(mod.1)
MSE <- sqrt( sum(mod.1$residuals^2 ) /mod.1$df.residual )

alpha <- 0.05
g <- 200
n <- 25
# Bonfernonis
qt(1-alpha/(2*g) ,n-2)
# Sheffe's
sqrt( 2 * qf(1-alpha ,2 ,n-2) )

g <- 1
while(sqrt( 2 * qf(1-alpha ,2 ,n-2) ) > qt(1-alpha/(2*g) ,n-2) ){
  g <- g + 1
}
