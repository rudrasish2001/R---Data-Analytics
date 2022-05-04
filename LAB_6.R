print(getwd())
setwd("D:\\Users\\rudrasishmishra\\Desktop\\DA LAB")
print(getwd())

data<- read.csv("one.csv")
print(class(data))
data
info <- subset(data, salary > 600 & dept == "IT")
print(info)

maxsalary <- subset(data, select=c(dept, salary), subset(dept == "Operations" && salary == max(salary)))
print(maxsalary)
print(getwd())


#Q-1

