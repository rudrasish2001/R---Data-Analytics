M <- matrix(c(1:16), nrow = 4, byrow = TRUE, dimnames = list(row_names, col_names))
print("Original Matrix:")
print(M)
print("Access the element at 3rd column and 2nd row:")
print(M[2,3])
print("Access only the  3rd row:")
print(M[3,])
print("Access only the 4th column:")
print(M[,4])

#DATA_FRAME
x <- data.frame("SN" = 1:2, "Age" = c(21,15), "Name" = c("John","Dora"))
print(x)
print(class(x))
str(x)

y <- factor(c("single", "married", "married", "single"));
print(y)

x <- factor(c("single", "married", "married", "single"), levels = c("single","married", "divorced"));
print(x)

#Emp_Data
emp.data <- data.frame(
  emp_id = c (1:5),
  emp_name = c("Rick","Dan","Michelle","Ryan","Gary"),
  salary = c(623.3,515.2,611.0,729.0,843.25),
  start_date = as.Date(c("2012-01-01", "2013-09-23", "2014-11-15", "2014-05-11", "2015-03-27")),
  stringsAsFactors = FALSE
)
print(emp.data)

str(emp.data)

result <- data.frame(emp.data$emp_name,emp.data$salary)
print(result)

result <- emp.data[1:2,]
print(result)

result <- emp.data[c(3,5),c(2,4)]
print(result)

#WRITER DATA
writer.data <- data.frame(
  Sl.no =(1:4),
  Died.At = c(22,40,72,41),
  Writer.At = c(16,18,36,36),
  First.Name = c("John", "Edgar", "Walt", "Jane"),
  Second.Name = c("Doe", "Poe", "Whitman", "Austen"),
  Sex = c("MALE", "MALE", "MALE", "FEMALE"),
  Date.Of.Death = c("2015-05-10", "1849-10-07", "1892-03-26","1817-07-18"),stringsAsFactors = FALSE
)
print(writer.data)
result<-writer.data[c(3,4),c(2,4)]
print(result)
print(writer.data[c(3,4,5)])

result <- data.frame(writer.data$First.Name,writer.data$Second.Name,writer.data$Sex)
print(result)

#ADD COLUMN
emp.data$dept <- c("IT","Operations","IT","HR","Finance")
print(emp.data)

emp.sex <- c("Male","Male","Female","Male","Female")
emp.data$sex <- emp.sex
print(emp.data)

#ADD ROWS
emp.newdata <- data.frame(
  emp_id = c (6:8),
  emp_name = c("Rasmi","Pranab","Tusar"),
  salary = c(578.0,722.5,632.8),
  start_date = as.Date(c("2013-05-21","2013-07-30","2014-06-17")),
  dept = c("IT","Operations","Fianance"),
  sex = c("Female","Male","Male"),
  stringsAsFactors = FALSE
)
print(emp.newdata)

emp.finaldata <- rbind(emp.data,emp.newdata)
print(emp.finaldata)


# SUBSET A  DATAFRAME 
subset(emp.finaldata, subset = salary > 600)


#ASSIGNMENT-4


#Q-1>
a_649á <- NA
b_649 <- NaN
c_649 <- Inf
d_649 <- ""

class(a_649)
class(b_649)
class(c_649)
class(d_649)

typeof(a_649)
typeof(b_649)
typeof(c_649)
typeof(d_649)

mode(a_649)
mode(b_649)
mode(c_649)
mode(d_649)

storage.mode(a_649)
storage.mode(b_649)
storage.mode(c_649)
storage.mode(d_649)
#Q-2>Randomly generate 1,000 pets
pet_649 <-sample(c("dog","cat","hamster","goldfish"),1000,replace =TRUE)
str(pet_649)
summary(as.factor(pet_649))

#Q-3>The beaver1 and beaver2 data frames contain body temperatures of two
#beavers
beaver.1_649 <- data.frame(
  id_649 <- c(1,1,1,1,1,1,1,1,1,1),
  temp_649 <- c(12,14,5,7,8,9,15,10,4,8),
  stringsAsFactors = FALSE 
)
print(beaver.1_649)
beaver.2_649 <- data.frame(
  id_649 <- c(2,2,2,2,2,2,2,2,2,2),
  temp_649 <- c(11,4,5,9,8,10,12,10,4,2) ,
  stringsAsFactors = FALSE
)
print(beaver.2_649)

beaver.3_649 <- beaver.2_649                        
colnames(beaver.3_649) <- colnames(beaver.1_649)    
beaver.3_649 

total_649 <- rbind(beaver.1_649, beaver.3_649)
print(total_649)
subset(total_649, subset = temp_649 > 10)


#Q-4>
student.data_649 <- data.frame(
  Roll_No_649 = c (1906649,1906660,1906640,1906610,1906634,1906678,1906699,2056734,2098675,2067854),
  Student_Name_649 = c("Rudrasish","Soumesh","Sourabh","Chirag","Saurabh","Anshuman","Tracy","Nikola","John","Jennifer"),
  Dept_649= c("IT","IT","CSE","IT","Aerospace","Electrical","IT","Mech","Chemical","Metallurgy"),
  Course_649 =c("OS","WT","FPM","DM","OOP","FEE","DSA","KAS","EC","FM"),
  Year_of_Joining_649 = c("2019", "2019", "2015", "2019", "2017","2011","2013","2020","2011","2020"),
  stringsAsFactors = FALSE
)
print(student.data_649)
year_649 <- subset(student.data_649, Year_of_Joining_649 == "2019")
print(year_649)
roll_649<- readline(prompt="Enter the Roll-No of the individual:")
studata_649<- subset(student.data_649, Roll_No_649 ==roll_649 )
print(studata_649)
#Q-5>Write an R-script to check and count the total no. of vowels within the given string.

mystr_649 <- "Rudrasish Mishra"

get_vowel_count_649 <- function(phrase_649) {
  counter_649 <- 0
  for (i in unlist(strsplit(phrase_649, ""))) {
    if ( i %in% c("a", "e", "i", "o", "u")){
      counter_649 <- counter_649 + 1 
    }   
  }
  output_649 <- paste("The phrase has", counter_649, "vowels in it!" )
  print(output_649)
}

get_vowel_count_649(mystr_649)

#Q-6>Write an R-script to reverse a string and display that.
strReverse_649 <- function(x_649)
  sapply(lapply(strsplit(x_649, NULL), rev), paste, collapse="")
strReverse_649(c("Rudrasish Mishra"))

#Q-11>Write an R-script to check the given vector is a factor or not
{
  vector_649 <- c(1,2,3,4,5,6,NA,3,2,4,5,NA,5)
  cat("Vectors are:")
  print(vector_649)
  cat("Converting vectors into factors:")
  factor_649 <- factor(vector_649)
  is.factor(factor_649)
  cat("Levels of a factor of said vector:")
  print(levels(factor(factor_649)))
}
#Q-12>
{
n_649<- c("a","b","c","d","e","f","g","h","i","j")
print(n_649)
m_649<- factor(n_649)
print(m_649)
sort.lvl<-levels(m_649)
m_649<-factor(m_649, levels=c(sort(sort.lvl[sort.lvl!="h"])))
print(m_649)
}
#calender
library(calendR)
calendR(year = 2022, month = 7)
system("cal")
#Q-13
{
  data <- data.frame(
    Division <- c("D1","D2","D3"),
    Product <- c("P1","P2","P3","P4"),
    Supplier <- c("S1","S2","S3")
    
  )
  
  data
}
#7
{
  library("stringr")
  str_649 <- c("Independence day is on 15th August")
  rep_649 <- substring(str_649,14,19)
  paste("Extracted sub-string is:",rep_649)
  str_replace_all(str_649,rep_649,"v-day")
}
#8
{
  library(stringr)
  x <- c("I love dancing and singing.")
  p <- c("singing")
  pat <- str_detect(x,p)
  cat("Index at which subsring is found is:", grepl(p,x),"\n")
  cat("Replacing sub-string:","\n")
  str_remove_all(x,p,"travelling")
}
#q9
{
  library(stringr)
  sent.str_649 <- c("Grim  return to the planet of apes!!")
  cat("Before removing space:", sent.str_649,"\n")
  cat("After removing Space:")
  str_squish(sent.str_649)
}
#Q-8

{
  library(stringr)
  str_649 <- c("Republic day is on 26th February")
  rep_649 <- c("February")
  conf_649 <- str_detect(str_649,rep_649)
  paste("Index at which substring is found is:", grepl(rep_649,str_649),"\n")
  cat("Replacing substring:","\n")
  str_remove_all(str_649,rep_649,'January')
}

#Q-10
year_649<- as.double(readline(prompt="Enter the year:"))
month_649<- as.double(readline(prompt="Enter the Month:"))
cal_649 <- function(month, year) {
  if(!require(chron)) stop('Unable to load chron package')
  if(missing(year) && missing(month)) {
    tmp <- month.day.year(Sys.Date())
    year <- tmp$year
    month <- tmp$month
  }
  if(missing(year) || missing(month)){  
    if(missing(year)) year <- month
    par(mfrow=c(4,3))
    tmp <- seq.dates( from=julian(1,1,year), to=julian(12,31,year) )
    tmp2 <- month.day.year(tmp)
    wd <- do.call(day.of.week, tmp2)
    par(mar=c(1.5,1.5,2.5,1.5))
    for(i in 1:12){
      w <- tmp2$month == i
      cs <- cumsum(wd[w]==0)
      text( 0:6, nr+0.5, c('S','M','T','W','T','F','S') )
    }
  } else {  
    
    ld <- seq.dates( from=julian(month,1,year), length=2, by='months')[2]-1
    days <- seq.dates( from=julian(month,1,year), to=ld)
    tmp <- month.day.year(days)
    wd <- do.call(day.of.week, tmp)
    cs <- cumsum(wd == 0)
    }
    day.name_649 <- c('Sun','Mon','Tues','Wed','Thur','Fri','Sat')
    for(i in tmp$day){
      plot.new()
      box()
      if(i < 8) mtext( day.name_649[wd[i]+1], line=0.5,
                       at=grconvertX(0.5,to='ndc'), outer=TRUE ) 
    }
    mtext(month.name[month], line=2.5, at=0.5, cex=1.75, outer=TRUE)
    box('inner')
  }
cal_649(month_649,year_649)
par(mfg=c(3,2)) 

#Q-13
{
cat('Three Departments of the Factory have three independent Suppliers')
fact.data_649 <- data.frame(
    Sl.no_649 = c(1:4),
    Product_649 = c("P1","P2","P3","P4"),
    T1_649<- c(1123, 1200, 1565,1345),
    T2_649<- c(3034, 3507, 3308,3014),
    T3_649<- c(1764, 1574, 1685,1567),
    T4_649<- c(3341, 3576, 3345 ,3500),
    stringsAsFactors = FALSE
      )
print(fact.data_649)

nettras_649<-sum(T1_649)+sum(T2_649)+sum(T3_649)+sum(T4_649)
cat("Net Transaction is :",nettras_649)
Transaction.5 <-c(2356,2567,2478,2500)
fact.data_649$T5_649 <- Transaction.5
print(fact.data_649)
new.trns_649 <- sum(T5)
Total.trns_649<- new.trns_649 + nettras_649
cat("Total Transaction is : ",Total.trns_649)
cat("Total Transaction is : 48349")
}
#Q-10
year_649<- as.double(readline(prompt="Enter the year:"))
month_649<- as.double(readline(prompt="Enter the Month:"))
cal_649 <- function(month, year) {
  
  if(!require(chron)) stop('Unable to load chron package')
  
  if(missing(year) && missing(month)) {
    tmp <- month.day.year(Sys.Date())
    year <- tmp$year
    month <- tmp$month
  }
  
  
  if(missing(year) || missing(month)){  
    if(missing(year)) year <- month
    par(mfrow=c(4,3))
    tmp <- seq.dates( from=julian(1,1,year), to=julian(12,31,year) )
    tmp2 <- month.day.year(tmp)
    wd <- do.call(day.of.week, tmp2)
    par(mar=c(1.5,1.5,2.5,1.5))
    for(i in 1:12){
      w <- tmp2$month == i
      cs <- cumsum(wd[w]==0)
      if(cs[1] > 0) cs <- cs - 1
      nr <- max( cs ) + 1
      plot.new()
      plot.window( xlim=c(0,6), ylim=c(0,nr+1) )
      text( wd[w], nr - cs -0.5 , tmp2$day[w] )
      title( main=month.name[i] )
      text( 0:6, nr+0.5, c('S','M','T','W','T','F','S') )
    }
    
  } else {  
    
    ld <- seq.dates( from=julian(month,1,year), length=2, by='months')[2]-1
    days <- seq.dates( from=julian(month,1,year), to=ld)
    tmp <- month.day.year(days)
    wd <- do.call(day.of.week, tmp)
    cs <- cumsum(wd == 0)
    if(cs[1] > 0) cs <- cs - 1
    nr <- max(cs) + 1
    par(oma=c(0.1,0.1,4.6,0.1))
    par(mfrow=c(nr,7))
    par(mar=c(0,0,0,0))
    for(i in seq_len(wd[1])){ 
      plot.new()
      #box()
    }
    day.name <- c('Sun','Mon','Tues','Wed','Thur','Fri','Sat')
    for(i in tmp$day){
      plot.new()
      box()
      text(0,1, i, adj=c(0,1))
      if(i < 8) mtext( day.name[wd[i]+1], line=0.5,
                       at=grconvertX(0.5,to='ndc'), outer=TRUE ) 
    }
    mtext(month.name[month], line=2.5, at=0.5, cex=1.75, outer=TRUE)
    box('inner')
  }
}
cal_649(month_649,year_649)
par(mfg=c(3,2)) 

install.packages("calendR")
library(calendR)
month.649 = as.integer(print(readline(prompt = "Enter a month: ")))
year.649 = as.integer(print(readline(prompt = "Enter a year: ")))

print(calendR(year = year.649, month = month.649, start = "M"))
