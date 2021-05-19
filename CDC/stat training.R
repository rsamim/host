##-- DESCROPTIVE STATISTICS------
library(tidyverse)
logical_vector <- c(1:8) #tf, can use operators
logical_vector>3 & logical_vector<5
logical_vector[(logical_vector>3) & (logical_vector<5)])]

w<-c(11:15)
v<-c(1:5)
v+w
mean(v)
sqrt(v)
log(v)
vec <- c(1, 1, 1, 5, 1, 1, 10, 10,10)
table(vec)
table(vec)/sum(table(vec)) #percentage counts
unique(vec)
head(iris)
tail(iris)
data(cars)
head(cars)
summary(cars$speed)

quiz<-data.frame("q1" = c(0, 0, 1, 0, 1), 
                 "q2" = c(0, 1, 0, 0, 1),
                 "q3" = c(0, 1, 0, 1, 1),
                 "q4" = c(1, 1, 1, 1, 1),
                 "q5" = c(1, 0, 1, 0, 1))
rowMeans(quiz)
rowSums(quiz)
colMeans(quiz)
colSums(quiz)
apply(quiz, 1, mean, na.rm = TRUE)

set.seed(231)
mat<-matrix(rnorm(20), nrow = 4, ncol = 5)
mat
mean(mat[,2]) #column
mean(mat[2,]) #row
apply(mat, 2, median)
install.packages("Deducer")

rnorm(20, mean = 4, sd = 1)
rbinom(7, 150, prob = 0.5)

set.seed(123)
X = data.frame(k1 = sample(100:1000, 1000, replace = TRUE),
              k2 = sample(10:100, 1000, replace = TRUE))
X
x.scaled = scale(X, center = TRUE)
x.scaled

colMeans(x.scaled)
var(x.scaled)

C.scaled = scale(cars$dist, center = TRUE, scale = TRUE)
C.scaled

dbinom(5, size = 10, prob = 0.5)
pbinom(5, 10, 0.5)
#pbinom vs dbimon: dbinom probaboloty of getting resilt for that specific point on binomial dist.

summary(C.scaled)
#when to do standardization and normalization

##---EXPLORATORY DATA ANALYSIS----
str(cars)
set.seed(125)
x <- rnorm(30)
y <- x+rnorm(30)
barplot(x)
boxplot(x)
plot(density(x))
hist(x)
qqplot(x, y) #if the points fall on a 45 deg angle, they come from the same dist
plot(x, y)
x <- c(21, 62, 10, 53)
labels <- c("London", "NY", "J", "E")
pie(x, labels)
hist(x, xlab = "cities", col = "yellow", border = "blue")

plot(ecdf(x))
##LOOK AT PPT FOR REST

##-----INFERENTIAL STATISTICS-----