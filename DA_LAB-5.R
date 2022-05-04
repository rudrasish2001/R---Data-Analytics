?datasets
library(help = "datasets")
data() 
data(package = .packages(all.available = TRUE)) 

#EX-DS:EuStockMarkets

data(EuStockMarkets) 
summary(EuStockMarkets)
View(EuStockMarkets)
format
str(EuStockMarkets) 
head(EuStockMarkets, 10) 
nrow(EuStockMarkets) 
ncol(EuStockMarkets) 

#5 DATASETS
#UCBAdmissions           Student Admissions at UC Berkeley
data(UCBAdmissions) 
summary(UCBAdmissions)
View(UCBAdmissions)
str(UCBAdmissions) 
head(UCBAdmissions, 25) 
nrow(UCBAdmissions) 
ncol(UCBAdmissions)


#USArrests
data(USArrests) 
summary(USArrests)
View(USArrests)
str(USArrests) 
head(USArrests, 25) 
nrow(USArrests) 
ncol(USArrests)



installed.packages()

#1.LINE PLOT

#6.SCATTER PLOT
#base R
plot(x = mtcars$wt, y = mtcars$mpg)
#using qplot
library(ggplot2)
qplot(x = mtcars$wt, y = mtcars$mpg)
# using only x and y vectors
qplot(x = mtcars$wt, y = mtcars$mpg)

# using x and y vectors from a data frame
qplot(x = wt, y = mpg, data = mtcars)

# using ggplot syntax
ggplot(data = mtcars, aes(x = wt, y = mpg)) +
  geom_point()




#Draw a Bubble Chart using ggplot2 package
library(ggplot2)
library(dplyr)
library(gapminder)
data <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)

ggplot(data, aes(x=gdpPercap, y=lifeExp, size = pop)) +
  geom_point(alpha=0.7)


#List and count the number of packages installed in the workspace
str(allPackage <- installed.packages(.Library, priority = "high"))
allPackage [, c(1,3:5)]