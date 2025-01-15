45 ^2
1^3 + 2^3 + 3^3 + 4^3 + 5^3 + 6^3 + 7^3 + 8^3 + 9^3
sum((1:9)^3)
sum(1:9)^2
str(as.Date("2025-01-01"))
?seq
dates = seq(as.Date("2025-01-01"), as.Date("2025-01-03"), by = "1 day")
dates <- seq(as.Date("2025-01-01"), as.Date("2025-01-03"), by = "1 day")
weekdays(dates)
substr(weekdays(dates), 1, 1)

dates[1]


pi
pi *4
letters
LETTERS[1:5]
letters[seq(1, 10, by=2)]

x<- 2
hist(runif(10000))
?runif

?plot
 ## draw a sine wave between 0 and pi*2
x<- seq(0, pi*2, by = 0.01)
plot(x, sin(x), type= "1")

curve(sin, to = pi*2)     
curve(cos, add = TRUE, col = "green")

## random walk
x <- 0
set.seed(42)
for (i in 1:100) {
  if (runif(1) < 0.5){
    x <- x-1
  } else{
    x <- x+1
  }
}
x

runif(100) <0.5
as.integer(runif(100) <0.5)
as.integer(runif(100) <0.5) * 2 -1
set.seed(42)
sum((runif(100) <0.5) * 2 -1)
cumsum((runif(100) <0.5) * 2 -1)
set.seed(42)
plot(1:100, cumsum((runif(100) <0.5) * 2 -1), type = "s")
sum((runif(100) <0.5) * 2 -1)


for (i in 1:100) {
  x <- c(x, sum((runif(100)<0.5) *2 -1))
}

x <- round(runif(1e4)<0.5) *2 -1
m <- matrix(x, nrow = 100)
hist(apply(m,1,sum))

h <- c(174, 170, 160)
w <- c(90, 80, 70)
plot(h, w, xlab = "Height (cm)", ylab = "Weight (kg)", main = "Height and Weight of Persons")

min(h)
max(h)
range(h)
diff(range(h))
mean(h)
median(h)
sum(h)
summary(h)
str(summary(h))

cor(w, h)
lm(w ~ h)

fit <- lm(w ~ h)
predict(fit)
predict(fit, newdata = list(h = 172))

##BMI
BMI <- w/(h/100)^2
df <- data.frame(height = h, weight = w)
str(df)
df
df[1, 2]
df$weight
df[1, 2]
df$weight[1]
df$bmi <- BMI
df$bmi <- df$weight / (df$height/100)^2

df <- read.csv('http://bit.ly/CEU-R-heights')
## compute BMI
df$bmi <- (df$weightLb * 0.45) / (df$heightIn * 2.54/100)^2
df$weightLb <- NULL
df$heightIn <- NULL
height <- df$heightIn * 2.54
weight <- df$weightLb * 0.45
plot(df)
summary(df)

## parisD3
install.packages("pairsD3")
library(pairsD3)
pairsD3(df)

install.packages("GGally")
library("GGally")
ggpairs(df)

library(ggplot2) # 
ggplot(df, aes(x = height)) + geom_histogram()
ggplot(df, aes(x = height)) + geom_histogram(color = "red", fill = "green") + theme_bw()

p <-  ggplot(df, aes(x = weight))
p + geom_histogram()
p + geom_density() + theme_bw()

p <- p + geom_histogram()
p + theme_bw() + scale_y_log10()

ggplot(df, aes(x = sex)) + geom_bar()
ggplot(df, aes(x = height, y =weight)) + geom_point() + theme_bw() + geom_smooth()
ggplot(df, aes(x = height, y =weight)) + geom_point() + theme_bw() + geom_smooth(method = "lm") #linear regression
ggplot(df, aes(x = height, y =weight)) + geom_point() + theme_bw() + geom_smooth(method = "lm", se = FALSE, col = "pink") #linear regression
ggplot(df, aes(x = height, y =weight, col = sex)) + geom_point() + theme_bw() + geom_smooth(method = "lm", se = FALSE) #gender group

ggplot(df, aes(x = height, y = weight)) +
  geom_point(aes(color = sex)) + theme_bw() +
  geom_smooth(method = "lm", se = FALSE, col = "black") +
  geom_smooth(aes(color = sex), method = "lm", se = FALSE)

ggplot(df, aes(x = height)) + geom_boxplot()
ggplot(df, aes(x = height)) + geom_boxplot(aes(color = sex))
ggplot(df, aes(x= sex, y = height)) + geom_boxplot()
# facet_wrap
ggplot(df, aes(y = height)) + geom_boxplot() + facet_wrap(~sex)
ggplot(df, aes(x = height, y = weight)) + geom_smooth(method = "lm") + geom_point() + facet_wrap(~sex)

ggplot(df, aes(x= sex, y = height)) + geom_boxplot()
ggplot(df, aes(x= sex, y = height)) + geom_violin() + geom_boxplot() +geom_jitter()

ggplot(df, aes(x = height)) + geom_density()
ggplot(df, aes(x = height, fill = sex)) + geom_density()
ggplot(df, aes(x = height, fill = sex)) + geom_density(alpha = 0.25)
ggplot(df, aes(x = height, fill = sex)) + geom_density(alpha = 0.25) +theme_bw() + ggtitle("Height of girls and boys") + theme()
ggplot(df, aes(x = height, fill = sex)) + geom_density(alpha = 0.25) +theme_bw() + ggtitle("Height of girls and boys") + 
  theme(legend.position = "top") + xlab("Height (cm)")
# higher than 160
df$height <- df$heightIn * 2.54
df$weight <- df$weightLb * 0.45
df$height160 <- df$height < 160
df$height160 <- cut(df$height, breaks = c(0, 160, Inf), labels = c("low", "tall"))
ggplot(df, aes(x = sex, fill = height160)) + geom_bar()


## Data Transformation
## mean of height by gender
str(df)
df[df$sex == "m", ]
?subset
subset(df, sex == "m" )
mean(df[df$sex == "m", "height"])
subset(df, sex == "m" )$height
subset(df, sex == "m" )[,"height"]
aggregate(height ~sex, FUN = mean, data = df)

install.packages("data.table")
library(data.table)
dt <- data.table(df)
dt
#dt[i]
dt[sex == "f"]
dt[ageYear == min(ageYear)]
dt[ageYear == min(ageYear)][order(bmi)]
#dt[i, j]
dt[sex == "f", mean(height)]
dt[sex == "f", hist(height)]
#dt[i, j, by]
dt[,mean(height), by = sex]
dt[,mean(height), by = sex][sex == "f"]

#count girls and boy above/below 160cm
dt[height<160, .N, by = sex]
dt[height<160, leng(height), by = sex]
dt[height<160, list(.N), by = sex]
dt[height<160, list(.N), by = .(gender = sex)]
dt[height160 == "low", (count = .N), by = .(gender = sex)]
dt[, (count = .N), by = .(gender = sex, height < 160)]
dt[, .(min = min(height), .N, max = max(height)), by = .(gender = sex, height < 160)]



## count the number of folks below/above 12 yrs
dt[ageYear < 12, .N]
dt[ageYear > 12, .N]
df$agecat <- cut(df$ageYear, breaks = c(0, 12, Inf))
dt[, .N, by = agecat]
## show the average weight of high BMI (25) folks
dt[bmi > 25, mean(weight)]
## categorize folks to underweight(<18.5)/normal/overweight
df$bmigroup <- cut(df$bmi, breaks = c(0, 18.5, 25, Inf), labels = c("underweight", "normal", "overweight"))
dt[, bmigroup := cut(df$bmi, breaks = c(0, 18.5, 25, Inf), labels = c("underweight", "normal", "overweight"))]
## stacked bar chart for BMI categorization split by gender
install.packages("ggplot2")
library(ggplot2)
ggplot(df, aes(x = sex, fill = bmigroup)) + geom_bar()
ggplot(dt, aes(x = sex, fill = bmigroup)) + geom_bar()
library(data.table)
dtagg <- dt[, .N, by = .(sex, bmigroup)]
ggplot(dtagg, aes(x = sex, fill = bmigroup, y = N)) + geom_bar(stat = "")
ggplot(dtagg, aes(x = sex, fill = bmigroup, y = N)) + geom_col()

# 第二周
ls()
rm(list = ls())  #清除emvironment

source("http://bit.ly/CEU-R-heights-2018")
## compute the mean of height
mean(heights, na.rm = TRUE)
## TODO do some dataviz
hist(heights)
ggplot(data.frame(heights), aes(heights))+ geom_histogram()

rm(list = ls())
ls(all.names = TRUE)
.secret

library(data.table)
bookings <- fread('http://bit.ly/CEU-R-hotels-2018-prices')
features <- fread('http://bit.ly/CEU-R-hotels-2018-features')
