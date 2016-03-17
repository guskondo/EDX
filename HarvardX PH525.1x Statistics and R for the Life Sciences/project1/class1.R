tab <- read.csv("msleep_ggplot2.csv")
class(tab)
head(tab)
dim(tab)
?read.csv
colnames(tab)
tab$sleep_total
plot(tab$brainwt, tab$sleep_total)
plot(tab$brainwt, tab$sleep_total, log="x")
summary(tab)
tab[ c(1,2), ]
tab[ tab$sleep_total > 18, ]
tab[ tab$sleep_total > 18, "sleep_total"]
mean(tab[ tab$sleep_total > 18, "sleep_total"])
which(tab$sleep_total > 18)
tab$sleep_total[c(22,37,43,62)]
tab$sleep_total[ which(tab$sleep_total > 18)[1] ]
which(tab$sleep_total > 18&tab$sleep_rem < 3)
sort(tab$sleep_total)
order(tab$sleep_total)
tab$sleep_total[ order(tab$sleep_total) ]
rank(c(1,2,2,3))
rank(tab$sleep_total)
match(c("Cow","Owl monkey","Cheetah"), tab$name)
idx = match(c("Cow","Owl monkey","Cheetah"), tab$name)
tab[idx,]
match(c("Cotton rat"), tab$name)
vec = c("red","blue","red","green","green","yellow","orange")
fac = factor(vec)
fac
levels(fac)
vec == "blue"
table(tab$order, tab$order)
s = split(tab$sleep_total, tab$order)
s
s[["Rodentia"]]
s[[17]]
mean(s[[17]])
lapply(s, mean)
sapply(s, mean)
tapply(tab$sleep_total, tab$order, mean)
tapply(tab$sleep_total, tab$order[["Primates"]], sd)

dat <- read.csv("femaleMiceWeights.csv")
s = split(dat[,2], dat[,1])
stripchart(s, vertical=TRUE, col=1:2)
abline(h=sapply(s, mean), col=1:2)
which(s$hf<mean(s$chow))
which(s$chow>mean(s$hf))
highfat <- s$hf
sample(highfat,6)
sum(highfat > 30)
sum(highfat > 30)/sum(as.numeric(highfat>0))

population <- read.csv("femaleControlsPopulation.csv")
mean(population[,1])
sampleMean = replicate(10000, mean(sample(population[,1],12)))
head(sampleMean)
plot(sampleMean)
null = replicate(10000, mean(sample(population[,1], 12)) - mean(sample(population[,1], 12)))
plot(null)
hist(null)
diff = mean(dat[13:24,2]) - mean(dat[1:12,2])
abline(v=diff, col="red")
abline(v=-diff, col="red")
mean(null > abs(diff))

library(gapminder)
data(gapminder)
head(gapminder)
x <- as.vector(gapminder[gapminder$year == 1952,"lifeExp"])
hist(x)
mean(x <= 40)
mean(x <= 60) - mean(x <= 40)
prop = function(q) {
  mean(x <= q)
}
prop(40)
qs = seq(from=min(x), to=max(x), length=20)
qs
props = sapply(qs, prop)
plot(qs,props)
y <- as.vector(gapminder[gapminder$year == 1952,"pop"])
hist(log10(y))
sd(log10(y))
x <- log10(y)
qqnorm(x)
z <- (x - mean(x)) / sd(x)
qqnorm(z)
abline(0,1)
max(z)
F = function(q) pnorm(q, mean=mean(x), sd=sd(x))
n = length(x)
(F(7) - F(6)) * n
sum(x > 6 & x <= 7)
head(pnorm(z))
qqnorm(x)
ps = ((1:n) - 0.5)/n
qqnorm(ps)
sort(x)
plot(qnorm(ps), sort(x))
qqnorm(x)
qnorm(ps[1])