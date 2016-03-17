load("C:\\Users\\Gustavo\\Desktop\\Mestrado Profissional\\EUA\\Preparativos RIT\\RIT\\Classes\\Summer Term\\HarvardX PH525.1x Statistics and R for the Life Sciences\\project1\\skew.RData")
dim(dat)
par(mfrow = c(1,1))
for (i in 1:9) {
  qqnorm(dat[,i])
  qqline(dat[,i])
}
hist(dat[,4])
hist(dat[,9])

head(InsectSprays)
boxplot(split(InsectSprays[,1], InsectSprays[,2]))
boxplot(InsectSprays[,1]~ InsectSprays[,2])

install.packages("UsingR")
library(UsingR)
data(father.son)
plot(father.son$fheight, father.son$sheight)
cor(father.son$fheight, father.son$sheight)
identify(father.son$fheight, father.son$sheight)
x = father.son$fheight
y = father.son$sheight
n = nrow(father.son)
plot(scale(x), scale(y))
abline(h=0, v=0)
sum(scale(x) * scale(y)) / (n - 1)

data(nym.2002)
time = sort(nym.2002$time)
head(nym.2002)
time[1]/median(time)
time[1000]/median(time)

plot(time/median(time), ylim=c(1/4,4))
abline(h=c(1/2,1,2))

plot(log2(time/median(time)),ylim=c(-2,2))
abline(h=-1:1)


msleep = read.csv(file = "C:\\Users\\Gustavo\\Desktop\\Mestrado Profissional\\EUA\\Preparativos RIT\\RIT\\Classes\\Summer Term\\HarvardX PH525.1x Statistics and R for the Life Sciences\\project1\\msleep.csv", header = T)

msleep %>%
  mutate(prop_rem = sleep_rem / sleep_total) %>%
  group_by(order) %>%
  select(order,prop_rem) %>%
  summarize(med_rem =median(prop_rem)) %>%
  arrange(med_rem) %>%
  head
  
data(ChickWeight)
plot(ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)
head(ChickWeight)
chick = reshape(ChickWeight,idvar=c("Chick","Diet"),timevar="Time",direction="wide")
head(chick)
chick = na.omit(chick)
chick1 = chick[,c(1,2,5)]
head(chick1)
chick2 = chick1
chick2[46,] = c(1,1,3000)
chick2
avg1 = sd(chick1$weight.4)
avg1
avg2 = sd(chick2$weight.4)
avg2/avg1

chick3 = chick[,c(1,2,5,14)]
chick4 = chick3
chick4[46,] = c(1,1,3000,3000)
chick4
cor1 = cor(chick3$weight.4,chick3$weight.21)
cor2 = cor(chick4$weight.4,chick4$weight.21)
cor2/cor1

cor1 = cor(chick3$weight.4,chick3$weight.21, method = "spearman")
cor2 = cor(chick4$weight.4,chick4$weight.21, method = "spearman")
cor2/cor1

stripchart(chick$weight.4 ~ chick$Diet, method="jitter", vertical=TRUE)

x =chick[,c(2,5)]
x =  filter(x, chick$Diet==1)
x = x[,2]
y =chick[,c(2,5)]
y =  filter(y, chick$Diet==4)
y = y[,2]
t.test(x,y)
wilcox.test(x,y)

x[17] = 200
t.test(x,y)
wilcox.test(x,y)

x = x[-17]
par(mfrow=c(1,3))
boxplot(x,y)
boxplot(x,y+10)
boxplot(x,y+100)

t.test(x,y+10)$p.value - t.test(x,y+100)$p.value
wilcox.test(x,y+10)$p.value - wilcox.test(x,y+100)$p.value

wilcox.test(c(1,2,3),c(4,5,6))
wilcox.test(c(1,2,3),c(400,500,600))
