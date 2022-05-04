csv_data_649 <- read.csv(file = 'Student.csv')
print(csv_data_649)
print (ncol(csv_data_649))
print(nrow(csv_data_649))  
maxpercentage_649 <- subset(csv_data_649, PERCENTAGE == max(PERCENTAGE))
print (maxpercentage_649)
branch_649 <- subset(csv_data_649, BRANCH == "IT"|BRANCH == "CSE")
print (branch_649)
branchper_649 <- subset(csv_data_649, BRANCH == "CSE"& PERCENTAGE>=80)
print (branchper_649)
date_649 <- subset(csv_data_649, DOA>= as.Date("2017-05-01"))
print (date_649)
View(mtcars)

install.packages("xlsx")
any(grepl("xlsx",installed.packages()))
library("xlsx")
data_649 <- read.xlsx("sheet1.xlsx", sheetIndex = 1)
print(data_649)
data1_649 <- read.xlsx("sheet2.xlsx", sheetIndex = 1)
print(data1_649)

Data3_649 <- merge(data_649, data1_649, all.x = TRUE, all.y = TRUE)
print(Data3_649)


install.packages("writexl")
library(writexl)
write_xlsx(Data3_649, "Student.xlsx")
student_649 <- read.xlsx("Student.xlsx", sheetIndex = 1)
print(student_649)


na.omit(student_649) 
na.fail(student_649)
View(student_649)

write.xlsx("sheet2", "Student.xlsx", sheetName="sheet1", col.names=TRUE, row.names=TRUE, append=FALSE)
write.xlsx2("sheet2", "Student.xlsx", sheetName="sheet1", col.names=TRUE, row.names=TRUE, append=FALSE)










