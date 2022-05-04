data(package = .packages(all.available = TRUE))

#IRIS DATASET
View(iris)
nrow(iris)
ncol(iris)
dim(iris)
names(iris)et
str(iris)
attributes(iris)
head(iris, 10) 
tail(iris, 10) 
iris$Sepal.Length[1:10]
summary(iris)
mean(iris$Sepal.Length)
median(iris$Sepal.Length)
range(iris$Sepal.Length)
quantile(iris$Sepal.Length)
quantile(iris$Sepal.Length, c(.1, .3, .65))

var(iris$Sepal.Length)
hist(iris$Sepal.Length)
plot(density(iris$Sepal.Length))

table(iris$Species)
pie(table(iris$Species))
barplot(table(iris$Species))
cov(iris$Sepal.Length, iris$Petal.Length)
cov(iris[,1:4])
cor(iris$Sepal.Length, iris$Petal.Length)
cor(iris[,1:4])
aggregate(Sepal.Length ~ Species, summary, data=iris)
#BOXPLOT
boxplot(Sepal.Length~Species, data=iris)
#SCATTERPLOT
with(iris, plot(Sepal.Length, Sepal.Width, col=Species,pch=as.numeric(Species)))
plot(jitter(iris$Sepal.Length), jitter(iris$Sepal.Width))
pairs(iris)
#3DSCATTERPLOT 
install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(iris$Petal.Width, iris$Sepal.Length, iris$Sepal.Width)
#HEATMAP
distMatrix <- as.matrix(dist(iris[,1:4]))
heatmap(distMatrix)

install.packages("lattice")
library(lattice)
levelplot(Petal.Width~Sepal.Length*Sepal.Width, iris, cuts=9, col.regions=grey.colors(10)[10:1])




#visualising multiple dimensions
install.packages("MASS")
library(MASS)
parcoord(iris[1:4], col = iris$Species)
#Parallel Coordinates
library(lattice)
parallelplot(~iris[1:4] | Species, data = iris)
#complex graphics using Package ggplot2  
library(ggplot2)
qplot(Sepal.Length, Sepal.Width, data=iris, facets=Species ~.)



#Cars93 dataset

View(Cars93)
nrow(Cars93)
ncol(Cars93)
dim(Cars93)
names(Cars93)et
str(Cars93)
attributes(Cars93)
head(Cars93, 10) 
tail(Cars93, 10) 
Cars93$Weight[1:10]
# EXPLORING ALL VARIABLES
summary(Cars93)
mean(Cars93$Weight)
median(Cars93$Weight)
range(Cars93$Weight)
#PERCENTAGE
quantile(Cars93$Weight)
quantile(Cars93$Weight, c(.1, .3, .65))

var(Cars93$Weight)
hist(Cars93$Weight)
plot(density(Cars93$Weight))

table(Cars93$Type)
pie(table(Cars93$Type))
barplot(table(Cars93$Type))

# covariance
cov(Cars93$Price, Cars93$Weight)
cov(Cars93[,4:8])
# correlation
cor(Cars93$Price, Cars93$Weight)
#aggregate
aggregate(Cars93$Price ~ Type, summary, data=Cars93)
#boxplot
boxplot(Cars93$Price ~ Type, data=Cars93)
#scatterplot
with(Cars93, plot(Price,Weight, col=Type,pch=as.numeric(Type)))
plot(jitter(Cars93$Price), jitter(Cars93$Weight))


pairs(Cars93)
#3Dscatterplot 
library(scatterplot3d)
scatterplot3d(Cars93$Price, Cars93$Weight,Cars93$Horsepower)
#heatmap
distMatrix <- as.matrix(dist(Cars93[,4:8]))
heatmap(distMatrix)

library(lattice)
levelplot(Price~Weight*Horsepower, Cars93, cuts=9, col.regions=grey.colors(10)[10:1])
#A 3D scatter plot can be produced with package scatterplot3d
library(scatterplot3d)
scatterplot3d(Cars93$Price, Cars93$Weight,Cars93$Horsepower)
#heatmap() 
distMatrix <- as.matrix(dist(Cars93[,4:8]))
heatmap(distMatrix)
#visualising multiple dimensions
library(MASS)
parcoord(Cars93[4:8], col = Cars93$Type)
#Parallel Coordinates
library(lattice)
parallelplot(~Cars93[4:8] | Type, data = Cars93)
#complex graphics using Package ggplot2  
library(ggplot2)
qplot(Weight,Horsepower, data=Cars93, facets=Type ~.)


#CO2 Dataset

View(CO2)
nrow(CO2)
ncol(CO2)
dim(CO2)
names(CO2)et
str(CO2)
attributes(CO2)
head(CO2, 10) 
tail(CO2, 10) 
CO2$conc[1:10]
# EXPLORING ALL VARIABLES
summary(CO2)
mean(CO2$conc)
median(CO2$conc)
range(CO2$conc)
#PERCENTAGE
quantile(CO2$conc)
quantile(CO2$conc, c(.1, .3, .65))

var(CO2$conc)
hist(CO2$conc)
plot(density(CO2$conc))

table(CO2$Plant)
pie(table(CO2$Plant))
barplot(table(CO2$Plant))

# covariance
cov(CO2$conc, CO2$uptake)
cov(CO2[,4:5])
# correlation
cor(CO2$conc, CO2$uptake)
#aggregate
aggregate(CO2$conc ~ Plant, summary, data=CO2)
#boxplot
boxplot(CO2$conc ~ Plant, summary, data=CO2)
#scatterplot
with(CO2, plot(conc,uptake, col=Type,pch=as.numeric(Type)))
plot(jitter(CO2$conc), jitter(CO2$uptake))
distMatrix <- as.matrix(dist(CO2[,4]))
heatmap(distMatrix)

pairs(Cars93)
#3Dscatterplot 
library(scatterplot3d)
scatterplot3d(Cars93$Price, Cars93$Weight,Cars93$Horsepower)
#heatmap


library(lattice)
levelplot(Price~Weight*Horsepower, Cars93, cuts=9, col.regions=grey.colors(10)[10:1])
#A 3D scatter plot can be produced with package scatterplot3d
library(scatterplot3d)
scatterplot3d(Cars93$Price, Cars93$Weight,Cars93$Horsepower)
#heatmap() 
distMatrix <- as.matrix(dist(Cars93[,4:8]))
heatmap(distMatrix)
#visualising multiple dimensions
library(MASS)
parcoord(Cars93[4:8], col = Cars93$Type)
#Parallel Coordinates
library(lattice)
parallelplot(~Cars93[4:8] | Type, data = Cars93)
#complex graphics using Package ggplot2  
library(ggplot2)
qplot(Weight,Horsepower, data=Cars93, facets=Type ~.)


#mtcars  dataset
data("OrchardSprays")
View(OrchardSprays)
nrow(OrchardSprays)
ncol(OrchardSprays)
dim(OrchardSprays)
names(OrchardSprays)
str(UKDriverDeaths)
attributes(OrchardSprays)
head(OrchardSprays , 10) 
tail(OrchardSprays , 10) 
OrchardSprays$decrease[1:10]
# EXPLORING ALL VARIABLES
summary(OrchardSprays)
mean(OrchardSprays$decrease)
median(OrchardSprays$decrease)
range(OrchardSprays$decrease)
#PERCENTAGE
quantile(OrchardSprays$decrease)
quantile(OrchardSprays$decrease, c(.1, .3, .65))

var(OrchardSprays$decrease)
hist(OrchardSprays$decrease)
plot(density(OrchardSprays$decrease))

cov(OrchardSprays$decrease, OrchardSprays$colpos)
cov(OrchardSprays[,0:3])
# correlation
cor(OrchardSprays$decrease, OrchardSprays$colpos)
#aggregate
aggregate(OrchardSprays$decrease ~ colpos, summary, data=OrchardSprays)
#boxplot
boxplot(OrchardSprays$decrease ~ colpos, summary, data=OrchardSprays)

library(lattice)
levelplot(decrease~colpos*rowpos, OrchardSprays, cuts=9, col.regions=grey.colors(10)[10:1])
#A 3D scatter plot can be produced with package scatterplot3d
library(scatterplot3d)
scatterplot3d(OrchardSprays$decrease, OrchardSprays$colpos , OrchardSprays$rowpos)
#heatmap() 
distMatrix <- as.matrix(dist(OrchardSprays[,0:3]))
heatmap(distMatrix)
#visualising multiple dimensions
library(MASS)
parcoord(OrchardSprays[0:3], col = OrchardSprays$decrease)
#Parallel Coordinates
library(lattice)
parallelplot(~OrchardSprays[2:3] | treatment, data = OrchardSprays)




#QUAKES


View(quakes)
data("quakes")
nrow(quakes)
ncol(quakes)
dim(quakes)
names(quakes)
str(quakes)
attributes(quakes)
head(quakes , 10) 
tail(quakes , 10) 
quakes$stations[1:10]
# EXPLORING ALL VARIABLES
summary(quakes)
mean(quakes$stations)
median(quakes$stations)
range(quakes$stations)
#PERCENTAGE
quantile(quakes$stations)
quantile(quakes$stations, c(.1, .3, .65))

var(quakes$stations)
hist(quakes$stations)
plot(density(quakes$stations))

cov(quakes$stations, quakes$depth)
cov(quakes[,0:3])
# correlation
cor(quakes$stations, quakes$depth)
#aggregate
aggregate(quakes$stations ~ depth, summary, data=quakes)
#boxplot
boxplot(quakes$stations ~ depth, summary, data=quakes)

library(lattice)
levelplot(depth~mag*stations, quakes, cuts=7, col.regions=grey.colors(10)[10:1])
#A 3D scatter plot can be produced with package scatterplot3d
library(scatterplot3d)
scatterplot3d(quakes$stations, quakes$mag , quakes$depth)
#heatmap() 
distMatrix <- as.matrix(dist(quakes[,0:3]))
heatmap(distMatrix)
#visualising multiple dimensions
library(MASS)
parcoord(quakes[0:3], col = quakes$stations)
#Parallel Coordinates
library(lattice)
parallelplot(~quakes[2:3] | depth, data = quakes)



#PlantGrowth


View(EuStockMarkets)
data("EuStockMarkets")
nrow(EuStockMarkets)
ncol(EuStockMarkets)
dim(EuStockMarkets)
names(EuStockMarkets)
str(EuStockMarkets)
attributes(EuStockMarkets)
head(EuStockMarkets , 10) 
tail(EuStockMarkets , 10) 
EuStockMarkets$FTSE[1:10]
# EXPLORING ALL VARIABLES
summary(EuStockMarkets)
mean(Titanic$Survived)
median(UCBAdmissions$Freq)
range(Titanic$Survived)
#PERCENTAGE
quantile(Titanic$Survived)
quantile(Titanic$Survived, c(.1, .3, .65))

var(quakes$stations)
hist(quakes$stations)
plot(density(quakes$stations))

cov(quakes$stations, quakes$depth)
cov(quakes[,0:3])
# correlation
cor(quakes$stations, quakes$depth)
#aggregate
aggregate(quakes$stations ~ depth, summary, data=quakes)
#boxplot
boxplot(quakes$stations ~ depth, summary, data=quakes)

library(lattice)
levelplot(depth~mag*stations, quakes, cuts=7, col.regions=grey.colors(10)[10:1])
#A 3D scatter plot can be produced with package scatterplot3d
library(scatterplot3d)
scatterplot3d(quakes$stations, quakes$mag , quakes$depth)
#heatmap() 
distMatrix <- as.matrix(dist(quakes[,0:3]))
heatmap(distMatrix)
#visualising multiple dimensions
library(MASS)
parcoord(quakes[0:3], col = quakes$stations)


#IRIS3

View(HairEyeColor)
nrow(HairEyeColor)
ncol(HairEyeColor)
dim(HairEyeColor)
names(HairEyeColor)
str(HairEyeColor)
attributes(HairEyeColor)
head(HairEyeColor, 10) 
tail(HairEyeColor, 10)
HairEyeColor[1:10]
summary(HairEyeColor)
mean(HairEyeColor)
median(HairEyeColor)
range(HairEyeColor)
quantile(HairEyeColor)
quantile(HairEyeColor, c(.1, .3, .65))

var(HairEyeColor)
hist(HairEyeColor)
plot(density(HairEyeColor))

table(HairEyeColor)
pie(table(HairEyeColor))
barplot(table(HairEyeColor))

aggregate(Sex ~ Freq, summary, data=HairEyeColor)
#BOXPLOT
boxplot(Sex ~ Freq, data=HairEyeColor)
#SCATTERPLOT
with(HairEyeColor, plot(Eye, Freq, col=Eye,pch=as.numeric(Eye)))
plot(jitter(HairEyeColor), jitter(HairEyeColor))
pairs(HairEyeColor)
#3DSCATTERPLOT 
install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(HairEyeColor$Hair, HairEyeColor$Freq,HairEyeColor$Sex)
#HEATMAP
distMatrix <- as.matrix(dist(HairEyeColor[,0:3]))
heatmap(distMatrix)






#visualising multiple dimensions
install.packages("MASS")
library(MASS)
parcoord(iris[1:4], col = iris$Species)
#Parallel Coordinates
library(lattice)
parallelplot(~iris[1:4] | Species, data = iris)
#complex graphics using Package ggplot2  
library(ggplot2)
qplot(Sepal.Length, Sepal.Width, data=iris, facets=Species ~.)
