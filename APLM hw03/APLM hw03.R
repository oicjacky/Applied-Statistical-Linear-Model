library(stringr)
setwd("C:/Users/Master/Desktop/黃三/應用統計線性模型/APLM hw03")
data <- read.table("CH09TA01.txt")
colnames(data) <- c("X1","X2","X3","X4","X5","Y","lnY")

n <- dim(data)[1]
P <- 5            # 5 predict variable
#
R.square <- function(data , mod){
  
  SSTO <- sum( ( data$Y - mean(data$Y) )^2 )
  SSE <- sum(mod$residuals^2)
  
  return( 1 - (SSE / SSTO) )
}
# R.adjust
R.adjust <- function(data , mod){
  n <- dim(data)[1]
  p <- mod$rank
  
  SSTO <- sum( ( data$Y - mean(data$Y) )^2 )
  SSE <- sum(mod$residuals^2)
  
  return( 1 - ( (n-1) / (n-p) ) * (SSE / SSTO) )
}
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
length(powerset(1:P))
all_possible <- powerset(1:P)
R_adj.square <- R_square <- c()
variable <- c()
for(i in 2: length(all_possible) ) {
  
  if( length(all_possible[[i]]) == 1 ){
    A <- data.frame( data[, all_possible[[i]] ] , 
                     Y = data$Y                         )
    colnames(A)[-2] <- paste0("X",all_possible[[i]])
    a <- R.adjust(A , lm( Y ~ A[,1] ,A) )
    b <- R.square(A , lm( Y ~ A[,1] ,A) )
    print(colnames(A))
  }else if( length(all_possible[[i]]) == 2 ){
    A <- data.frame( data[, all_possible[[i]] ] [ ,1] , 
                     data[, all_possible[[i]] ] [ ,2] ,
                     Y = data$Y                         )
    colnames(A)[-3] <- colnames(data[, all_possible[[i]] ])
    a <- R.adjust(A , lm( Y ~ A[,1] + A[,2] ,A) )
    b <- R.square(A , lm( Y ~ A[,1] + A[,2] ,A) )
    print(colnames(A))
    
  }else if( length(all_possible[[i]]) == 3 ){
    A <- data.frame( data[, all_possible[[i]] ] [ ,1] , 
                     data[, all_possible[[i]] ] [ ,2] ,
                     data[, all_possible[[i]] ] [ ,3] ,
                     Y = data$Y                         )
    colnames(A)[-4] <- colnames(data[, all_possible[[i]] ])
    a <- R.adjust(A , lm( Y ~ A[,1] + A[,2] + A[,3] ,A) )
    b <- R.square(A , lm( Y ~ A[,1] + A[,2] + A[,3] ,A) )
    print(colnames(A))
    
  }else if( length(all_possible[[i]]) == 4 ){
    A <- data.frame( data[, all_possible[[i]] ] [ ,1] , 
                     data[, all_possible[[i]] ] [ ,2] ,
                     data[, all_possible[[i]] ] [ ,3] ,
                     data[, all_possible[[i]] ] [ ,4] ,
                     Y = data$Y                         )
    colnames(A)[-5] <- colnames(data[, all_possible[[i]] ])
    a <- R.adjust(A , lm( Y ~ A[,1] + A[,2] + A[,3] + A[,4] ,A) )
    b <- R.square(A , lm( Y ~ A[,1] + A[,2] + A[,3] + A[,4] ,A) )
    print(colnames(A))
    
  }else if( length(all_possible[[i]]) == 5 ){
    A <- data.frame( data[, all_possible[[i]] ] [ ,1] , 
                     data[, all_possible[[i]] ] [ ,2] ,
                     data[, all_possible[[i]] ] [ ,3] ,
                     data[, all_possible[[i]] ] [ ,4] ,
                     data[, all_possible[[i]] ] [ ,5] ,
                     Y = data$Y                         )
    colnames(A)[-6] <- colnames(data[, all_possible[[i]] ])
    a <- R.adjust(A , lm( Y ~ A[,1] + A[,2] + A[,3] + A[,4] + A[,5] ,A) )
    b <- R.square(A , lm( Y ~ A[,1] + A[,2] + A[,3] + A[,4] + A[,5] ,A) )
    print(colnames(A))
  }
  
  R_adj.square <- rbind(R_adj.square , a )
  R_square <- rbind(R_square , b )
  variable <- c(variable ,str_c(colnames(A) ,collapse = ","))
}
A <- data.frame(R_square = R_square ,
                R_adj.square = R_adj.square ,
                variable = variable )
# 
str_c(colnames(A) ,collapse = ",")

mod1 <- lm( Y ~ X1 ,data )
summary(mod1)
mod2 <- lm( Y ~ X1 + X2 ,data )
summary( lm( Y ~ X1 + X2 + X3 + X5 ,data ) )
summary( lm( Y ~ X1 + X2 + X3  ,data ) )
# 
A[A$R_adj.square == min(A$R_adj.square), ]
A[A$R_adj.square == max(A$R_adj.square), ]
