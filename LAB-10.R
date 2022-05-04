
library(readr)
library(ggplot2)
library(forecast)
library(fpp2)
library(TTR)
library(dplyr)
dat <- read_csv("UNRATE.csv")
spec(dat)
glimpse(dat)
dat_train = subset(dat, class = 'Train')
dat_test = subset(dat, class = 'Test')

nrow(dat_train); nrow(dat_test)
dat_ts <- ts(dat_train[, 6], start = c(1968, 1), end = c(2013, 12), frequency = 12)

mape <- function(actual,pred){
  mape <- mean(abs((actual - pred)/actual))*100
  return (mape)
}

naive_mod <- naive(dat_ts , h = 12)
summary(naive_mod)
dat_test$naive = 10376
mape(dat_test$unemploy, dat_test$naive)
se_model <- ses(dat_ts, h = 12)
summary(se_model)se_model <- ses(dat_ts, h = 12)
summary(se_model)
df_fc = as.data.frame(se_model)
dat_test$simplexp = df_fc$`Point Forecast`
mape(dat_test$unemploy, dat_test$simplexp) 
holt_model <- holt(dat_ts, h = 12)
summary(holt_model)

df_holt = as.data.frame(holt_model)
dat_test$holt = df_holt$`Point Forecast`
mape(dat_test$unemploy, dat_test$holt) 
arima_model <- auto.arima(dat_ts)
summary(arima_model)

fore_arima = forecast::forecast(arima_model, h=12)
df_arima = as.data.frame(fore_arima)
dat_test$arima = df_arima$`Point Forecast`
mape(dat_test$unemploy, dat_test$arima)  

#EX1
input <- mtcars[,c("mpg","disp","hp","wt")]
print(head(input))
model <- lm(mpg~disp+hp+wt, data = input) 
print(model) 
cat("# # # # The Coefficient Values # # # ","\n")
a <- coef(model)[1] 
print(a) 
bDisp <- coef(model)[2] 
bHp <- coef(model)[3] 
bWt <- coef(model)[4] 
print(bDisp) 
print(bHp) 
print(bWt) 

#Ex-2
input <- mtcars[,c("am","cyl","hp","wt")] 
print(head(input))
am.data = glm(formula = am ~ cyl + hp + wt, data = input, family = binomial)
print(summary(am.data))


#Q-1

#a
data("marketing", package = "datarium")
head(marketing, 4)
cor(marketing$sales, marketing$youtube)
model <- lm(sales ~ youtube, data = marketing)
print(model)
summary(model)
confint(model)
bDisp <- coef(model)[2] 
bHp <- coef(model)[3] 
bWt <- coef(model)[4] 
print(bDisp) 
print(bHp) 
print(bWt)
mae(USArrests$Murder , USArrests$Assault)
#mse
mean(model_summ$Assault)
#holt
holt_model <- holt(USArrests, h = 12)
summary(holt_model)



#b
library(readr)
library(ggplot2)
library(forecast)
library(fpp2)
library(TTR)
library(dplyr)
install.packages("forecast")
install.packages("fpp2")
install.packages("TTR")
install.packages("dplyr")
View(USArrests)
data("USArrests")
head(USArrests , 4)
cor(USArrests$Murder , USArrests$Assault)
model <- lm(Murder  ~ Assault, data = USArrests)
print(model)
model_summ <- summary(model)
confint(model)
bDisp <- coef(model)[2] 
bHp <- coef(model)[3] 
bWt <- coef(model)[4] 
print(bDisp) 
print(bHp) 
print(bWt)
library("stats")
mae(USArrests$Murder , USArrests$Assault)
#mse
mean(model_summ$Assault)
#holt
holt_model <- holt(USArrests, h = 12)
summary(holt_model)



#c
data(trees)
results <- lm(Volume ~ Girth, data = trees)
summary(results)
confint(results)
bDisp <- coef(results)[2] 
bHp <- coef(results)[3] 
bWt <- coef(results)[4] 
print(bDisp) 
print(bHp) 
print(bWt)
#d
data(iris)
results <- lm(Sepal.Length ~ Sepal.Width, data = iris)
summary(results)
confint(results)
bDisp <- coef(results)[2] 
bHp <- coef(results)[3] 
bWt <- coef(results)[4] 
print(bDisp) 
print(bHp) 
print(bWt)
mae(USArrests$Murder , USArrests$Assault)
#mse
mean(model_summ$Assault)
#holt
holt_model <- holt(USArrests, h = 12)
summary(holt_model)


#Q-2

#a
View(EuStockMarkets)
data(EuStockMarkets)
input <-EuStockMarkets[,c("DAX","CAC","SMI","FTSE")] 
print(input)
dim(EuStockMarkets)
head(EuStockMarkets)
am.data = glm(formula = DAX ~ CAC + SMI +  FTSE ,data = input, family = binomial)
print(summary(am.data))

mae(USArrests$Murder , USArrests$Assault)
#mse
mean(model_summ$Assault)
#holt
holt_model <- holt(USArrests, h = 12)
summary(holt_model)



#b
library(tidyverse)
library(caret)
theme_set(theme_bw())
data("PimaIndiansDiabetes2", package = "mlbench")
PimaIndiansDiabetes2 <- na.omit(PimaIndiansDiabetes2)
sample_n(PimaIndiansDiabetes2, 3)
set.seed(123)
training.samples <- PimaIndiansDiabetes2$diabetes %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- PimaIndiansDiabetes2[training.samples, ]
test.data <- PimaIndiansDiabetes2[-training.samples, ]
model <- glm( diabetes ~ glucose, data = train.data, family = binomial)
summary(model)$coef


mae(USArrests$Murder , USArrests$Assault)
#mse
mean(model_summ$Assault)
#holt
holt_model <- holt(USArrests, h = 12)
summary(holt_model)

stars = c()

#3
#A
dataset = read.csv('data2.csv')
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))
dataset$State
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
regressor = lm(formula = Profit ~ .,data = training_set)
y_pred = predict(regressor, newdata = test_set)
print(regressor)
print(y_pred)
print(test_set)

#B

data("marketing", package = "datarium")
head(marketing, 4)
model <- lm(sales ~ youtube + facebook + newspaper, data = marketing)
summary(model)
summary(model)$coefficient
confint(model)

#C
data(package = "carData")
glimpse(Salaries)
levels(Salaries$sex)
lm1 <- lm(salary~sex, data = Salaries)
summary(lm1)
Salaries$sex <- relevel(Salaries$sex, ref = "Male")
levels(Salaries$sex)
lm2 <- lm(salary~sex, data = Salaries)
summary(lm2)
lm_total <- lm(salary~., data = Salaries)
summary(lm_total)
lm4 <- lm(formula = salary ~ rank + discipline + yrs.since.phd + sex + service_time_cat, data = Salaries_mod)
summary(lm4)
lm5 <- lm(formula = salary ~ rank + discipline + yrs.since.phd + service_time_cat, 
          data = Salaries_mod)
summary(lm5)

#Q-4
data(iris)
str(iris)
install.packages("e1071")
install.packages("caTools")
install.packages("caret")
library(e1071)
library(caTools)
library(caret)

split <- sample.split(iris, SplitRatio = 0.7)
train_cl <- subset(iris, split == "TRUE")
test_cl <- subset(iris, split == "FALSE")

train_scale <- scale(train_cl[, 1:4])
test_scale <- scale(test_cl[, 1:4])

set.seed(120)  
classifier_cl <- naiveBayes(Species ~ ., data = train_cl)
classifier_cl

y_pred <- predict(classifier_cl, newdata = test_cl)

cm <- table(test_cl$Species, y_pred)
cm
confusionMatrix(cm)

#Q-5
dataset = read.csv('UNRATE.csv')
dataset = dataset[0:2]
dataset$UNEMPLOY = factor(dataset$UNEMPLOY, levels = c(0, 1))
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])
classifier = svm(formula = UNEMPLOY ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')
y_pred = predict(classifier, newdata = test_set[-3])
cm = table(test_set[, 3], y_pred)
print(y_pred)
print(cm)






#sdd

data1.649<-read.csv("income.data.csv")
summary(data1.649)
hist(data1.649$happiness)
plot(data1.649$happiness~data1.649$income,data = data1.649)
happy.649<-lm(data1.649$happiness~data1.649$income,data = data1.649)
h_summ<-summary(happy.649)
mse<-mean(h_summ$residuals^2)
print(mse)
mae<-mean(h_summ$residuals)
print(mae)
rmse<-sqrt(mse)
print(rmse)

#2nd DATASET:

library(fpp2) 
library(dplyr) 
dat <- read_csv("timeseriesdata.csv") 
glimpse(dat) dat_train = subset(dat, Class == 'Train') 
dat_test = subset(dat, Class == 'Test') 
nrow(dat_train); 
nrow(dat_test) 
dat_ts <- ts(dat_train[, 6], start = c(1968, 1), end = c(2013, 12), frequency = 12) 
mape <- function(actual,pred){ mape <- mean(abs((actual - pred)/actual))*100 return (mape) }
naive_mod <- naive(dat_ts, h = 12) 
summary(naive_mod) dat_test$naive = 10376 
mape(dat_test$unemploy, dat_test$naive) 
se_model <- ses(dat_ts, h = 12) 
summary(se_model) df_fc = as.data.frame(se_model) dat_test$simplexp = df_fc$`Point Forecast` 
mape(dat_test$unemploy, dat_test$simplexp) holt_model <- holt(dat_ts, h = 12) 
summary(holt_model)



