install.packages("UsingR")
library("UsingR")
?father.son
summary(father.son)
mean(father.son$sheight)
mean(father.son$sheight[round(father.son$fheight)==71])
X = matrix(1:1000,100,10)
X[25,3]

x=1:10

cbind(x,2*x,3*x, 4*x,5*x )
7+14+21+28+35
X %*% matrix(1,ncol(X) )

x = matrix(c(3,2,1,5,4,2,-1,0,-5,2,5,0,1,-1,-5,1),nrow=4,ncol=4)
y = matrix(c(10,5,7,4),4,1)
solve(x)%*%y

a <- matrix(1:12, nrow=4)
b <- matrix(1:15, nrow=3)
a%*%b
a[3,]%*%b[,2]

X <- matrix(c(1,1,1,1,0,0,1,1),nrow=4)
rownames(X) <- c("a","a","b","b")

beta <- c(5, 2)

X[2,]%*%beta

X <- matrix(c(1,1,1,1,1,1,0,0,1,1,0,0,0,0,0,0,1,1),nrow=6)
rownames(X) <- c("a","a","b","b","c","c")

X
beta <- c(10,3,-3)
X[5,]%*%beta

g = 9.8 ## meters per second

h0 = 56.67

v0 = 0

n = 25

tt = seq(0,3.4,len=n) ##time in secs, t is a base function

a = replicate(100000,{
y = h0 + v0 *tt - 0.5* g*tt^2 + rnorm(n,sd=1)

X = cbind(1,tt,tt^2)

A = solve(crossprod(X))%*%t(X)

-2 * (A %*% y) [3]}
)

sqrt(var(a))






y = father.son$sheight

n = length(y)

N = 50

index = sample(n,N)

sampledat = father.son[index,]

x = sampledat$fheight

y = sampledat$sheight

betahat = lm(y~x)$coef[2]

betahat[2]

a = 1:10000
for(i in 1:10000){
  index = sample(n,N)
  
  sampledat = father.son[index,]
  
  x = sampledat$fheight
  
  y = sampledat$sheight
  
  a[i] = lm(y~x)$coef[2]
}
sqrt(var(a))

x = father.son$fheight

y = father.son$sheight

mean( (y - mean(y))*(x-mean(x) ) )

n = length(y)

N = 50

set.seed(1)

index = sample(n,N)

sampledat = father.son[index,]

x = sampledat$fheight

y = sampledat$sheight

fit = lm(y~x)

summary(fit)

SSR = sum((y-fit$fitted.values)^2)

sigma2 = SSR / 48

X = cbind(rep(1,N), x)

sqrt(diag(solve(t(X)%*%X))*sigma2)
      