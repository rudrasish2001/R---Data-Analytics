#Q-1>
num_649 <- as.double(readline(prompt="Enter the number:"))
{
  if(num_649 > 0)
  {
    print("Number is POSITIVE");
  }
  if(num_649 < 0)
  {
    print("Number is NEGATIVE");
  }
  if(num_649 == 0)
  {
    print("Number is ZERO");
  }
}
#Q-2>Write an R-script to check whether the given number is positive or not
#using if…else statement.
num_649 = as.double(readline(prompt="Enter a number: "))
if(num_649 > 0) {
  print("Positive number")
} else {
  if(num_649 == 0) {
    print("Zero")
  } else {
    print("Negative number")
  }
}
#Q-5>palindrome
{
  n_649 = as.integer(readline(prompt = "Enter a number :"))
  
  rev_649 = 0
  num_649 = n_649
  
  while (n_649 > 0) {
    r_649 = n_649 %% 10
    rev_649 = rev_649 * 10 + r_649
    n_649 = n_649 %/% 10
  }
  
  if (rev_649 == num_649)
  {
    print(paste("Number is palindrome :", rev_649))
  }else
  {
    print(paste("Number is not palindrome :", rev_649))
  }
}

#Q-3>
year_649 = as.integer(readline(prompt="Enter a year: "))
if((year_649 %% 4) == 0) {
  if((year_649 %% 100) == 0) {
    if((year_649 %% 400) == 0) {
      print(paste(year_649,"is a leap year"))
    } else {
      print(paste(year_649,"is not a leap year"))
    }
  } else {
    print(paste(year_649,"is a leap year"))
  }
} else {
  print(paste(year_649,"is not a leap year"))
}

#Q-4>Write an R-script to enter two" numbers and find out the biggest one.
a_649<- as.double(readline(prompt="Enter the first number:"))
b_649<- as.double(readline(prompt="Enter the second number:"))
{
  if(a_649 > b_649){
    print(a,"is greater")
  }else if(b_649 > a_649){
    print(b_649,"is greater")
  }else{
    print("Both are equal")
  }
}
#Q-6>mark calculate 
mark.1_649 <-as.double(readline(prompt="Enter the mark for first subject OS:"))
mark.2_649 <-as.double(readline(prompt="Enter the mark for second subject AFL:"))
mark.3_649 <-as.double(readline(prompt="Enter the mark for thiurd subject WT:"))

tot_no_sub_649_649<-3
percentage_649<-(mark.1_649+mark.2_649+mark.3_649)/tot_no_sub_649
print(percentage)
if (percentage_649 >= 90){
  print("YOUR GRADE = O")
}else if (percentage_649 >= 80 & percentage_649 <= 89){
  print("YOUR GRADE = E")
}else if (percentage_649 >= 70 & percentage_649 <= 79){
  print("YOUR GRADE = A")
}else if (percentage_649 >= 60 & percentage_649 <= 69){
  print("YOUR GRADE + B")
}else if (percentage_649 >= 50 & percentage_649 <= 59){
  print("YOUR GRADE + C")
}else if (percentage_649 >= 40 & percentage_649 <= 49){
  print("YOUR GRADE + D")
}else if (percentage_649 >= 30 & percentage_649 <= 39){
  print("YOUR GRADE + E")
}else{
  print("YOU HAVE FAILED WITH F")
}
#Q-7>
{
  x_649 <- readline(prompt="Enter your choice :")
  x_649 <- as.integer(x_649)
  circle_649 <- function(){
    r_649 <- readline(prompt="Enter radius of circle :")
    r_649 <- as.integer(r)
    radius_649 <- 2*3.14*r_649
    print(radius_649)
  }
  rectangle_649 <- function(){
    l_649 <- readline(prompt="Enter length :")
    b_649 <- readline(prompt="Enter breadth :")
    l_649 <- as.integer(l_649)
    b_649 <- as.integer(b_649)
    area_649 <- l_649*b_649
    print(area_649)
  }
  triangle_649 <- function(){
    a_649 <- readline(prompt="Enter a :")
    b_649 <- readline(prompt="Enter b :")
    c_649 <- readline(prompt="Enter c :")
    a_649 <- as.integer(l_649)
    b_649 <- as.integer(b_649)
    c_649 <- as.integer(c_649)
    s_649 <- (a_649+b_649+c_649)/2
    area_649 <- sqrt(s_649*(s_649-a_649)(s_649-b_649)(s_649-c_649))
    print(paste("Area of triangle:", area_649))
  }
  switch(x_649,"1"= circle_649(),"2"= rectangle_649(),"3"= triangle_649())
}

#Q-8>

{
  color_649 <- function(v_649){
    switch(v_649,"R" = "red", "G" = "green","B"="blue", "length"= 5)  
  }
  
  c_649 <- readline(prompt="Enter the color number :")
  c_649 = as.character(c_649)
  
  color_649(c_649)
}

#Q-9>Write an R-script to generate the number series as follows using while
#loop- 1 4 9…………n2
n_649 = as.double(readline(prompt="Enter a Range: "))
x_649 <- 0
while (x_649 <= n_649) {
  print(x_649^2)      
  x_649 <- x_649 + 1    
}
#Q-10>
num_649 = as.integer(readline(prompt="Enter a number: "))
factorial_649 = 1
if(num_649 < 0) {
  print("Sorry, factorial does not exist for negative numbers")
} else if(num_649 == 0) {
  print("The factorial of 0 is 1")
} else {
  for(i in 1:num_649) {
    factorial_649 = factorial_649 * i
  }
  print(paste("The factorial of", num_649 ,"is",factorial_649))
}

#Q-11>Write an R-script to generate the Fibonacci series up to n terms.

nterms_649 <- as.integer(readline(prompt="How many terms? "))
n1_649 = 0
n2_649 = 1
count_649 = 2
if(nterms_649 <= 0) {
  print("Plese enter a positive integer")
} else {
  if(nterms_649 == 1) {
    print("Fibonacci sequence:")
    print(n1)
  } else {
    print("Fibonacci sequence:")
    print(n1_649)
    print(n2_649)
    while(count_649 < nterms_649) {
      nth_649 = n1_649 + n2_649
      print(nth_649)
      n1_649 = n2_649
      n2_649 = nth_649
      count_649 = count_649 + 1
    }
  }
}

#Q-12>Write an R-script to check whether a number n is prime number or not

number_649 = as.integer(readline(prompt="Enter a number: "))
flag_649 = 0
if(number_649 > 1) {
  flag_649 = 1
  for(i in 2:(number_649-1)) {
    if ((number_649 %% i) == 0) {
      flag_649 = 0
      break
    }
  }
} 
if(number_649 == 2)    flag_649 = 1
if(flag_649 == 1) {
  print(paste(number_649,"is a prime number"))
} else {
  print(paste(number_649,"is not a prime number"))
}

#Q-13>Write an R-script to check whether an input integer is perfect number or not

n_649 <- as.integer(readline(prompt = "Enter a number :"))
i_649 = 1
s_649 = 0
while (i_649 < n_649) {
  if (n_649 %% i_649 == 0) {
    s_649 = s_649 + i_649
  }
  i_649 = i_649 + 1
}

if (s_649 == n_649) {
  print(paste("The number is perfect :", n_649))
} else{
  print(paste("The number is not perfect :", n_649))
}

#Q-15>Write an R-script to reverse the number

num_649 = as.integer(readline(prompt = "Enter a number :"))
rev_649 = 0
while (num_649 > 0) 
{
  r_649 = num_649 %% 10
  rev_649 = rev_649 * 10 + r_649
  num_649 = num_649 %/% 10
}
print(paste("Reverse number is :", rev_649))

#Q-16>Write an R-script to check whether an integer number is an Armstrong
#number or not. If sum of cubes of each digit of the number is equal to the
#number itself, then the number is called an Armstrong number. For
#example, 153 = ( 1 * 1 * 1 ) + ( 5 * 5 * 5 ) + ( 3 * 3 * 3 )

num_649 = as.integer(readline(prompt="Enter a number: "))
sum_649 = 0
temp_649 = num_649
while(temp_649 > 0) {
  digit_649 = temp_649 %% 10
  sum_649 = sum_649 + (digit_649 ^ 3)
  temp_649 = floor(temp_649 / 10)
}
if(num_649 == sum_649) {
  print(paste(num_649, "is an Armstrong number"))
} else {
  print(paste(num_649, "is not an Armstrong number"))
}

#Q-18>Write an R-script to evaluate average of 3 numbers using function
avg.numbers_649 <- function(a_649, b_649, c_649)
{
  a_649 <- readline(prompt="Enter a Value: ")
  b_649 <- readline(prompt="Enter b Value: ")
  c_649 <- readline(prompt="Enter c Value: ")
  
  a_649 <- as.integer(a_649)
  b_649 <- as.integer(b_649)
  c_649 <- as.integer(c_649)
  
  Sum_649 = a_649 + b_649 + c_649
  Average_649 = Sum_649/3
  
  print(paste("Average of ",a_649, ",", b_649, ",", c_649, "is = ", Average_649))
}
avg.numbers_649(a_649, b_649, c_649)

#Q-19>Write an R-script to find out the factorial of a number using function
fact.num_649 <- function()
{
  n_649 <-  as.integer( readline(" Input a number to find factorial : "))
  fact_649 <- 1
  if(n_649 < 0) {
    print(" The number is negative the factorial does not exist. ")
  } else if(n_649 == 0) {
    print(" The factorial result is 1 ")
  } else {
    for( i in 1:n_649) {
      fact_649 = fact_649 * i
    }
    print(paste(" The factorial result is ", n_649 ,"is", fact_649 ))
  }
}
fact.num_649()

#Q-20>
num1_649 = as.integer(readline(prompt = "Enter first number: "))
num2_649 = as.integer(readline(prompt = "Enter second number: "))

hcf_649 <- function(x_649, y_649) 
{
  
  if(x_649 > y_649) {
    smaller_649 = y_649
  } else {
    smaller_649 = x_649
  }
  for(i in 1:smaller_649) {
    if((x_649 %% i == 0) && (y_649 %% i == 0)) {
      hcf_649 = i
    }
  }
  return(hcf_649)
}
print(paste("The H.C.F. of", num1_649,"and", num2_649,"is", hcf_649(num1_649, num2_649)))

lcm_649 <- function(x_649, y_649) {
  if(x_649 > y_649) {
    greater_649 = x_649
  } else {
    greater_649 = y_649
  }
  while(TRUE) {
    if((greater_649 %% x_649 == 0) && (greater_649 %% y_649 == 0)) {
      lcm_649 = greater_649
      break
    }
    greater_649 = greater_649 + 1
  }
  return(lcm_649)
}

print(paste("The L.C.M. of", num1_649,"and", num2_649,"is", lcm_649(num1_649, num2_649)))

#Q-21>Write an R-script to evaluate sum of the following series using recursive
#function 1+2+3+………………. +N

calculate_sum_649 <- function(n_649) {
  if(n_649 <= 1) {
    return(n_649)
  } else {
    return(n_649 + calculate_sum_649(n_649-1))
  }
}
calculate_sum_649(13)

#Q-14>

num_649 = as.integer(readline(prompt="Enter a range: "))
{
  sum_649 = 0
  sum_649.series_649 <- function(n_649)
  {
    for(i in 1:n_649)
    {
      for(j in 1:i)
      {
        sum_649 <- sum_649 + j
      }
    }
    return(sum_649)
  }
  output_649 = sum_649.series_649(num_649)
  print(output_649)
}

#Q-17>

{
  n_649 <- as.double(readline(prompt="Enter the range:"))
  num_649 = c()
  for(i in 1:n_649)
  {
    for(j in 1:i+1)
    {
      num_649 = c(num_649,i)
    }
    if( i%%2 == 0)
      print(num_649)
    else{
      for(n_649 in i:i-1)
        print(n_649)
    }
  }
}

#Q-22>
{
  sum_649 = 0
  rem_649 = c()
  num_649.rev <- function(n_649)
  {
    if(n_649>0)
    {
      rem_649 = n_649%/%10
      sum_649 = sum_649*10 + rem_649
      num_649.rev(n_649/10)
    }
    else
    {
      return(sum_649)
    }
    return(sum_649)
  }
  output_649 = num_649.rev(256)
  print(output_649)
}

#Q-23>

p_649<-as.double(readline(prompt="Enter the Amount:"))
n_649<-as.double(readline(prompt="Enter the n value:"))
{
  f_649 <- function(r=5,p_649,n_649)
  {
    interest_649 = (p_649*r_649*n_649)/100
  }
  result_649 <- f_649(r_649,p_649,n_649)
  cat("the simple interest is:",result_649)
}

#q24
{
  cat("Converted binary number:")
  decTObin_649 <- function(n_649)
  {
    if(n_649>1)
    {
      decTObin_649(as.integer(n_649/2))
    }
    cat(n_649 %% 2)
  }
  decTObin_649(52)
}

#q25
{
  a_649<-as.double(readline(prompt="Enter the range:"))
  fact_649 <- function(n_649)
  {
    if(n_649 <= 1)
    {
      return(1)
    }
    else
    {
      return(n_649 * fact_649(n_649-1))
    }
  }
  output_649 = fact_649(a_649)
  cat("The factorial of a number:",output_649)
}

#26
{
  sum.recur_649 <- function(n_649)
  {
    if(i==1)
    {
      return(1)
    }
    else
    {
      return(n_649^n_649 + sum.recur_649(n_649-1))
    }
  }
  s_649 = sum.recur_649(3)
  cat("The sum of squares of first n terms:",s_649)
}

#26
{
  sum.recur <- function(n)
  {
    if(i==1)
    {
      return(1)
    }
    else
    {
      return(n*n + sum.recur(n-1))
    }
  }
  s = sum.recur(3)
  cat("The sum of squares of first n terms:",s)
}

#q27
{
  sum_649 = 0
  avg_649 = c()
  oper_649 <- function(a,b,c,d,e,f)
  {
    sum_649 <- a + b + c + d + e + f
    avg_649 <- sum_649 /5
    x_649 <- c(a,b,c,d,e,f)
    sd(x_649)
    cat("The sum of given numbers:",sum_649)
    cat("The average of given number:",avg_649)
    cat("The standard deviation:",sd(x_649))
  }
  oper_649(1,2,3,4,5)
}

#28

{
  list_649 = c(2,40,2,507,177,7,9)
  cat("The mean of given numbers:",mean(list_649),"\n")
  cat("The median of a given numbers :",median(list_649),"\n")
  cat("The variance of given numbers:",var(list_649),"\n")
  cat("The standard deviation:",sd(list_649),"\n")
  cat("The scale of a vector:",scale(list_649),"\n")
  cat("Summary of numbers:",summary(list_649),"\n")
  cat("The rank of numbers:",rank(list_649),"\n")
  cat("The quantile:",quantile(list_649))
}



row_names_650 = c("row1", "row2", "row3", "row4")
col_names_650 = c("col1", "col2", "col3", "col4")
M_650 = matrix(c(1:16), nrow = 4, byrow = TRUE, dimnames = list(row_names, col_names))
print("Original Matrix:")
print(M_650)
print("Access only the  1st row:")
print(M_650[1,])
print("Access only the  3rd row:")
print(M_650[3,])
print("Access only the 2nd column:")
print(M_650[,2])


getRoot_650 <- function(x_650){
res_650 <- as.vector(x_650)
if(res_650 < 0) {
  cat("not Possible")
}else
  return(sqrt(res_650))
}

getRoot_650( c(4,-4,9,-16,121))
























