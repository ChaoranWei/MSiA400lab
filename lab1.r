#Q1
#(a) 
x <- seq(-4,4,0.01)
y = sin(x)
plot(x,y,type='l', col = 'red', xlim=c(-4,5),ylim=c(-1.5,4.5))
#(b)
par(new=TRUE)
x <- seq(-2,2,0.01)
lines(x, x*x, col = 'green',type='p')

#(c)
x = seq(-3,3,0.01)
fun <- function(x) {
  for (i in 1:length(x)) {
    if (x[i] < 0) x[i] = 0
    else x[i] = 1
  }
  return(x)
}

z = fun(x)
lines(x,z,col='blue',type = 'o')
title(main='some graphs')
legend("topright", inset=.01,
       c("sin(x)","x^2","step function"),fill=c('red','green','blue'),cex=0.5)

#Q2
fun <- function(x, labels) { #this assume 0.99 critical value
  m <- matrix(x, nrow = length(x)/length(labels)) #this matrix assume that all data values in the
  #same group are together, for example, c(1,2,3,4,5,6,7,8) -> 1,2 are in group 1, 
  k = length(labels)
  n = nrow(m)
  tm <- gl(k,1, n*k, factor(labels))
  vec <- vector(mode='numeric', length=k)
  for (i in 1:k) {
    vec[i] = mean(m[,i])
  }
  mean <- mean(vec)
  sum = 0
  for (i in 1:k) {
    sum  = sum + n*(vec[i] - mean)^2
  }
  
  df = k-1
  MS = sum / df
  ss = 0
  for (i in 1:k) {
    for (j in 1:n){
      ss = ss+ (m[j,i] - vec[i])^2
    }
  }

    df2 <- k*(n-1)
    MSW <- ss/df2
    f = MS/MSW #f-value calculated
    fvalue <- qf(0.99, df,df2) #f critical value
    if (f > fvalue) {
      return('reject H_0')
    } else {
      return('fail to reject H_0')
    }
    
}
  
t <- c(7,8,15,11,9,10,12,17,13,18,19,15,14,18,19,17,16,18,19,25,22,23,18,20)
n <- c(1,1,1,1,1,1,2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4,4,4)
  
fun(t,c(1,2,3,4))



