set.seed(8212)
N<- 500
x1 <- round(rnorm(N, 1, 20))
x2 <- round(runif(N, 5, 10))
x3 <- round(runif(N, 1,4), 1)
x4 <- round(runif(N, 5, 50))
x5<- rpois(N, 5)

set.seed(1)
sample(1:6, 10, replace = TRUE)
set.seed(123)
index <- sample(1:nrow(iris), 5)
index
iris[index,]
library(tools)
library(dplyr)
library(tidyverse)
#d1 <- 
strat_smp <- d1 %>% group_by("A") %>% sample_n(size=10)
strat_smp

# Let's take a 10% sample from only 'AA' and 'BB' groups from -A- in dat1
strat_smp <- d1 %>% group_by("A", "B") %>% sample_frac(size=.10)
strat_smp

# Let's take 7 samples from all -D- groups in d1, specified by column# number
strat_smp <- d1 %>% group_by(!!!5)%>% sample_n(size=7)
strat_smp

# Use a two-column strata: -E- and -D- -E- varies more slowly, so it is# better to put that first
strat_smp <- d1 %>% group_by("E", "D") %>% sample_frac(size= 0.20)

# Use a two-column strata (-E- and -D-) but only interested in cases where# -E- == 'F’
strat_smp <- d1 %>% group_by("E", "D") %>% sample_frac(size= 0.15, weight=(E=="F"))
strat_smp

corMat <- cor(x = iris[,-5], method = "pearson")
round(corMat, 2)
#levineTest()
#Define Sample 1
smp2014 <- c(222,823,1092,400,948,836)
#Define Sample 2
smp2019 <- c(910,650,700,892,229,1051)

#Two sample T-test
t.test(smp2014, smp2019, var.equal=FALSE)

xhat1 <-mean(smp2014)
xhat2 <-mean(smp2019)
s1<-sd(smp2014)
s2<-sd(smp2019)
n1<-length(smp2014)
n2<-length(smp2019)
diff_mean<- xhat1-xhat2
SE_diff_mean <- sqrt(s1^2/n1 + s2^2/n2)
t_stat<-diff_mean/SE_diff_mean
t_stat
pvalue = 2*pt(t_stat,df=n1+n2-2)
pvalue


temp=rbind(data.frame(x=smp2014,group=1),data.frame(x=smp2019,group=2))
temp$group=as.factor(temp$group)
leveneTest(temp$x,group=temp$group,center=mean)
result <- aov(Sepal.Length~Species,data=iris)

