babies = read.table("C:\\Users\\Gustavo\\Desktop\\Mestrado Profissional\\EUA\\Preparativos RIT\\RIT\\Classes\\Summer Term\\HarvardX PH525.1x Statistics and R for the Life Sciences\\project1\\babies.txt", header=TRUE)
bwt.nonsmoke = babies$bwt[babies$smoke==0]
bwt.smoke = babies$bwt[babies$smoke==1]
mean(bwt.nonsmoke)-mean(bwt.smoke)
sd(bwt.nonsmoke)
sd(bwt.smoke)

dat.ns = bwt.nonsmoke[1:30]
dat.s = bwt.smoke[1:30]
N=30

X.ns = mean(dat.ns)
sd.ns = sd(dat.ns)
X.s = mean(dat.s)
sd.s = sd(dat.s)
sd.diff = sqrt(sd.ns^2/N+sd.s^2/N)
tval = (X.ns - X.s)/sd.diff

t.test(dat.ns, dat.s)$statistic

bwt.nonsmoke = babies$bwt[babies$smoke==0]
bwt.smoke = babies$bwt[babies$smoke==1]

f <- function(n){
s1 <- sample(bwt.nonsmoke, n)
s2 <- sample(bwt.smoke, n)
mytest <- t.test(s1, s2)
mytest$conf.int
}

mean(replicate(1000, sum(f(30))))

f <- function(n){
  s1 <- sample(bwt.nonsmoke, n)
  s2 <- sample(bwt.smoke, n)
  mytest <- t.test(s1, s2)$conf.int
  popdiff = mean(bwt.nonsmoke) - mean(bwt.smoke)
  ifelse(mytest[1]<=popdiff & mytest[2]>=popdiff,1,0)
}

sum(replicate(1000, f(30)))/1000

N = 15
alpha = 0.1
b = 1000
As = 0.1

power = sapply(As,function(alpha){
  rejections = sapply(1:b,function(i){
    smoke = sample(bwt.smoke,N)
    nonsmoke = sample(bwt.nonsmoke,N)
    t.test(smoke, nonsmoke)$p.value < alpha
  })
return(mean(rejections))
})

plot(As,power)



d = read.csv("C:\\Users\\Gustavo\\Desktop\\Mestrado Profissional\\EUA\\Preparativos RIT\\RIT\\Classes\\Summer Term\\HarvardX PH525.1x Statistics and R for the Life Sciences\\project1\\assoctest.csv", header=TRUE)
tab = table(d$allele,d$case)
row.names(tab) = c("AA or Aa","aa")
colnames(tab) = c("Controls","Cases")

prop.table(tab)
prop.table(tab,1)

ctest <- chisq.test(tab)
ctest

fisher.test(tab)

pop.var = var(bwt.nonsmoke)
sample.size = 2:400
var.estimate = sapply(sample.size, function(n) var(sample(bwt.nonsmoke, n)))
plot(sample.size, var.estimate)
abline(h=pop.var, col="blue")


set.seed(0)
N <- 50
smokers <- sample(babies$bwt[babies$smoke==1], N)
nonsmokers <- sample(babies$bwt[babies$smoke==0], N)
obs <- mean(smokers) - mean(nonsmokers)

avgdiff <- replicate(1000, {
  all <- sample(c(smokers,nonsmokers))
  smokersstar <- all[1:N]
  nonsmokersstar <- all[(N+1):(2*N)]
  return(mean(smokersstar) - mean(nonsmokersstar))
})

mean(abs(avgdiff) > abs(obs))


bwt.nonsmoke = babies$bwt[babies$smoke==0]
pop.var = var(bwt.nonsmoke)

vars = replicate(1000, var(sample(bwt.nonsmoke, 50)))
hist(vars)
abline(v=pop.var)

sample.size = 2:400
var.estimate = sapply(sample.size, function(n) var(sample(bwt.nonsmoke, n)))
plot(sample.size, var.estimate)
abline(h=pop.var, col="blue")


set.seed(0)
N <- 50
smokers <- sample(babies$bwt[babies$smoke==1], N)
nonsmokers <- sample(babies$bwt[babies$smoke==0], N)
obs <- median(smokers) - median(nonsmokers)
avgdiff <- replicate(1000, {
  all <- sample(c(smokers,nonsmokers))
  smokersstar <- all[1:N]
  nonsmokersstar <- all[(N+1):(2*N)]
  return(median(smokersstar) - median(nonsmokersstar))
})
mean(abs(avgdiff) > abs(obs))