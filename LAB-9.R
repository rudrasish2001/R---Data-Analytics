#Q4
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

pairs(iris[1:4],main='\n\tIris Data(red=setosa,green=versicolor\n ,blue=virginica)', pch=21, bg=c('red','green3','blue')[unclass(iris$Species)])

set.seed(120)  
classifier_cl <- naiveBayes(Species ~ ., data = train_cl)
classifier_cl

y_pred <- predict(classifier_cl, newdata = test_cl)

cm <- table(test_cl$Species, y_pred)
cm

confusionMatrix(cm)

plot(function(x) dnorm(x, 1.462, 0.1736640), 0, 8,col='red', main='Petal length distribution \n for the 3 different species')
curve(dnorm(x, 4.260, 0.4699110), add=TRUE, col='blue')
curve(dnorm(x, 5.552, 0.5518947), add=TRUE, col='green')

#Q-2
data("freeny")
plot(freeny, col="navy", main="Matrix Scatterplot")
model <- lm(market.potential ~ price.index + income.level, data = freeny) 
model
model <- lm(market.potential ~ price.index + income.level, data = freeny)
model
summary(model)

#Q-1
actual <- c(35, 36, 43, 47, 48, 49, 46, 43, 42, 37, 36, 40)
predicted <- c(37, 37, 43, 46, 46, 50, 45, 44, 43, 41, 32, 42)

# Calculate error
error <- actual - predicted

# Example of invocation of functions
print(paste("Root Mean Squared Error:",rmse(error)))
print(paste("Mean Squared Error:",mse(error)))

print(paste("Mean Absolute Error:",mae(error)))
print(paste("Mean Absolute Percentage Error:",mape(error)))
library("ggplot2")
data_649<-read.csv("income.data.csv")
summary(data_649)
hist(data_649$happiness)
plot(data_649$happiness~data_649$income,data = data_649)
happy<-lm(data_649$happiness~data_649$income,data = data_649)
summary(happy)
income.graph<-ggplot(data_649, aes(x=income, y=happiness))+geom_point(alpha=0.1, color="blue")
income.graph
income.graph <- income.graph + geom_smooth(method="lm", col="black")
income.graph

#Q1
summary(InsectSprays)
a_649 <- lm(count~spray,data=InsectSprays)
plot(InsectSprays$count,InsectSprays$spray)
summary(Loblolly)
b_649 <- lm(height~seed,data=Loblolly)
plot(Loblolly$height,Loblolly$age)
View(CO2)
summary(CO2)
c_649 <- lm(conc~uptake,data=CO2)
plot(CO2$conc,CO2$uptake)

#Q4
dataset = read.csv('social.csv')
dataset = dataset[3:5]
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))
install.packages('caTools')
library(caTools)

set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)

training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])
library(e1071)

classifier = svm(formula = Purchased ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')
y_pred = predict(classifier, newdata = test_set[-3])
cm = table(test_set[, 3], y_pred)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)

plot(set[, -3],
     main = 'SVM (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))

contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)

points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'coral1', 'aquamarine'))

points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)

grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)

plot(set[, -3], main = 'SVM (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))

contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)

points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'coral1', 'aquamarine'))

points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))