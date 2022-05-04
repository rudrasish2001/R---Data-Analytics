?datasets
library(help='datasets')
data() 
data(package = .packages(all.available = TRUE))
#IRIS
data(iris)
summary(iris)
View(iris)
str(iris) 
head(iris, 25) 
nrow(iris) 
ncol(iris)
#DS:- EU Stock Market
data(EuStockMarkets) 
summary(EuStockMarkets)
View(EuStockMarkets)
str(EuStockMarkets) 
head(EuStockMarkets, 25) 
nrow(EuStockMarkets) 
ncol(EuStockMarkets) 

#installing packages
install.packages(c("readr", "ggplot2"))
install.packages("devtools")

installed.packages()


#LIne Chart
cars <- c(1, 3, 6, 4, 9) 
plot(cars) 
plot(cars, type = "o")
plot(cars, type="o", col="blue")
plot(cars,type = "o", col = "black", xlab = "Month", ylab = "Unit Produced")
trucks <- c(1, 5, 7, 5, 5)
lines(trucks, type = "o", col = "blue")

#Q-1

#DS-!>UCBAdmissions:Student Admissions at UC Berkeley
data(UCBAdmissions) 
summary(UCBAdmissions)
View(UCBAdmissions)
str(UCBAdmissions) 
head(UCBAdmissions, 5) 
nrow(UCBAdmissions) 
ncol(UCBAdmissions)


#DS-2>USArrests:Reasons for which criminals are arrested in the US
data(USArrests) 
summary(USArrests)
View(USArrests)
str(USArrests) 
head(USArrests, 5) 
nrow(USArrests) 
ncol(USArrests)

nrow(iris)
head(iris,15)
View(iris)
#DS_3>UKLungDeaths:Monthly Deaths from Lung Diseases in the UK
data(Titanic) 
summary(Titanic)
View(Titanic)
str(Titanic) 
head(Titanic , 5) 
nrow(Titanic) 
ncol(Titanic)

#DS_4>UKgas: UK Quarterly Gas Consumption
data(UKgas) 
summary(UKgas)
View(UKgas)
str(UKgas) 
head(UKgas , 5) 
nrow(UKgas) 
ncol(UKgas)

#DS_5>USAccDeaths:Accidental Deaths in the US 1973-1978
data(USAccDeaths) 
summary(USAccDeaths)
View(USAccDeaths)
str(USAccDeaths) 
head(USAccDeaths , 5) 
nrow(USAccDeaths) 
ncol(USAccDeaths)


#LIne Chart
cars_649 <- c(1, 3, 6, 4, 9) 
plot(cars_649) 
plot(cars_649, type = "o")
plot(cars_649, type="o", col="blue")
plot(cars_649,type = "o", col = "black", xlab = "Month", ylab = "Unit Produced")
trucks_649 <- c(1, 5, 7, 5, 5)
lines(trucks_649, type = "o", col = "blue")

# Kernel Density Plot
den_649 = density(mtcars$mpg)
plot(den_649,main=" Kernel Density of Miles Per Gallon")
polygon(den_649, col="red", border="blue")

#Scatter plot
# Plot the chart for cars with weight between 2.5 to 5 and mileage between 10 and 25.
  
input_649 <- mtcars[, c('wt', 'mpg')]
plot(x_649 = input_649$wt, y_649 = input_649$mpg,
     x_649lab = "Weight",
     y_649lab = "Milage",
     x_649lim = c(1.5, 4),
     y_649lim = c(10, 25),       
     main = "Weight vs Milage"
)                           
pairs(~wt + mpg + disp + cyl, data = mtcars,main = "Scatterplot Matrix")
#BOX PLOT
# data set "mtcars" with the columns "mpg" and "cyl"
input_649 <- mtcars[,c('mpg','cyl')]
print(head(input_649))
boxplot(mpg ~ cyl, data = mtcars, xlab = "Number of Cylinders",
        ylab = "Miles Per Gallon", main = "Mileage Data")
#BARPLOT
x <- c(7, 15, 23, 12, 44, 56, 32)

png(file = "barplot.png")

barplot(x, xlab = "Developers",
        ylab = "Count", col = "white",
        col.axis = "darkgreen",
        col.lab = "darkgreen")
par(mfrow=c(1,2)) # To display 2 graphs in one row 
library(vcd)
counts_649 <- table(Arthritis$Improved)
print(head(counts_649))
barplot(counts_649, 
        main="Simple Bar Plot",
        xlab="Improvement", ylab="Frequency")


#PIE DIAGRAM
city_649 <- c(23, 56, 20, 63)
label_649 <- c("FRANCE", "USA", "CHINA", "INDIA")
piepercent<- round(100 * city_649 / sum(city_649), 1)
pie(city_649, label_649 = piepercent,
    main = "City pie chart", col = rainbow(length(city_649)))
legend("topright", c("FRANCE", "USA", "CHINA", "INDIA"),
       cex = 0.5, fill = rainbow(length(city_649)))
#BAR PLOT
A_649 <- c(17, 2, 8, 13, 1, 22)
B_649 <- c("Jan", "feb", "Mar", "Apr", "May", "Jun")
barplot(A_649, names.arg = B_649, xlab ="Month", 
        ylab ="Articles", col ="green", 
        main ="MONTH VS ARTICLES")
#DOT PLOT
month_649 <- month.name
expected_649 <- c(15, 16, 20, 31, 11, 6,
              17, 22, 32, 12, 19, 20)
sold_649 <- c(8, 18, 12, 10, 41, 2,
          19, 26, 14, 16, 9, 13)
quarter_649 <- c(rep(1, 3), rep(2, 3), rep(3, 3), rep(4, 3))
data_649 <- data.frame(month_649, expected_649, sold_649, quarter_649)
dotchart(data_649$sold, labels = data_649$month, pch = 21, bg = "green", pt.cex = 1.5)


#Q-3
packinfo_649 <- installed.packages ()
print(packinfo_649)
cat("The number of packages installed in workspace are:",nrow(packinfo_649))
ncol(packinfo_649)


#Q-4library(ggplot2)
library(ggplot2)
library(dplyr)
library(gapminder)
data_649 <- gapminder %>% filter(year=="2007") %>% dplyr::select(-year)
ggplot(data_649, aes(x=gdpPercap, y=lifeExp, size = pop)) +
  geom_point(alpha=0.7)
