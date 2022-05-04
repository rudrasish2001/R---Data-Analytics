Root_649 <- function(x_649){
  res_649 <- as.vector(x)
  if(res_649 < 0){
    cat("not possible ")
    return(NaN)
  }else{
    cat(sqrt(res_649))
    cat(" ")
    return(sqrt(res_649))
  } 
}

#Q-1
Root_649(c(4, -4, 9, 16, 121))


getRoot_649 <- function(x_649){
  res_649 <- as.vector(x_649)
  if(res_649 < 0) {
    cat("not Possible")
  }else return(sqrt(res_649))
}

getRoot_649( c(4,-4,9,-16,121))

#Q2
a_649 <- c("A", "B", "C", "D","M","Q","N")
b_649 <- c("A", "C", "C", "E","N","Q","S")
E_649 <- (a_649 == b_649)
print(E_649)
#Q3

vec1_649<-c(1,0,0,1,1,0,0,1,1,1,0,0,0)
vec2_649<-c(0,0,1,1,1,0,0,1,1,0,1,0,1)
print(vec1_649)
print(vec2_649)
c_649<-vec1_649 & vec2_649
print(c_649)
#Q4
print("Enter the elements to be sorted:")
Elements_649<-scan()
print(Elements_649)
s_649<-sort(Elements_649)
print("The elements in ascending order are:",s_649)
#Q6
x_649 = c(10, 20, 30, 25, 9, 26)
print("Original Vectors:")
print(x_649)
print("Maximum value of the above Vector:")
print(max(x_649))
print("Minimum value of the above Vector:")
print(min(x_649))
print("Sum of the above Vector:")
print(sum(x_649))

#Q-5
X_649 <- c(5, 2, 5, 1, 51, 2)
Y_649 <- c(7, 9, 1, 5, 2, 1)

Z_649 <- X_649 ^ Y_649
print('1st vector raised to the power of 2nd vector:')
print(Z_649)

#Q7
a_649 <- c(3, 2, -7, -3, 5, 2)
n_649<-as.numeric(readline(prompt="Enter the number to be searched:"))
b_649 <- (a_649==n_649)  
c_649 <- which(a_649==n_649)
print(b_649)
print("Index of the element is ",c_649)

#Q-12

row_names_649 = c("row1", "row2", "row3", "row4")
col_names_649 = c("col1", "col2", "col3", "col4")
M_649 = matrix(c(1:16), nrow = 4, byrow = TRUE, dimnames = list(row_names, col_names))
print("Original Matrix:")
print(M_649)
print("Access only the  1st row:")
print(M_649[1,])
print("Access only the  3rd row:")
print(M_649[3,])
print("Access only the 2nd column:")
print(M_649[,2])

#Q-13

m1_649 = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
print("Matrix-1:")
print(m1_649)
m2_649 = matrix(c(0, 1, 2, 3, 0, 2), nrow = 2)
print("Matrix-2:")
print(m2_649)

result1 = m1_649 + m2_649
print("Result of addition:")
print(result1)

result2 = m1_649 - m2_649
print("Result of subtraction:")
print(result2)

result3 = m1_649 * m2_649
print("Result of multiplication:")
print(result3)

result4 = m1_649 / m2_649
print("Result of division::")
print(result4)

#Q-17
x_649 <- matrix(rep(2:10), 3, 3)
print(x_649)
rowSum_649 <-rowSums(x_649)
print("Sum of the elements row wise is :",rowSum_649)

#Q-18
print("Two vectors of different lengths are:")
v1_649 =  c(1,3,4,5)
v2_649 =  c(10,11,12,13,14,15)
print(v1_649)
print(v2_649)
result_649 = array(c(v1_649,v2_649),dim = c(3,3,3))
print("New 3D array is:")
print(result_649)
print("Now retrieve the elements of 2nd row of 3rd matrix.")
print(result_649[2,,3])
#q8
{
  vec <- 1:5
  mat <- matrix(c(1:9, ncol = 5))
  
  df <- EuStockMarkets[1:10,]
  my_list <- list(vec,mat,df)
  my_list
}
#q9
{
  vec <- c(1,2,4,8,10)
  m <- matrix(c(3:14),nrow = 4,byrow = TRUE)
  list1 <- list("Red","Green",c(21,32,11),TRUE,51.23)
  list_data <- list(vec,m,list1)
  print(list_data(2))
}

#q10
{
  list1 <- list(c("Red","Green"),c(21,32,11), matrix(c(3,9,5,1,-2,8),nrow = 2))
  print(list1)
  list1[4] <- "New Element"
  print(list1[4])
  list1[2] <- NULL
  print(list1)
}

#q11
{
  lits1 <- list(1,2,3,4,5)
  list2 <- list("January","feburary","March","April","May")
  merged.list <- c(list1,list2)
  print(merged.list)
}

#q14
{
  m <- matrix(c(3:12),nrow = 3,ncol=3, byrow = TRUE)
  m <- m[1:3,1:3] + 4
  print(m)
}
#q15
{
  m <- matrix(c(3:12),nrow = 3,ncol=3, byrow = TRUE)
  m[m<5] <- 0
  print(m)
}
#q16
{
  x1 <- matrix(c(1:9),nrow = 3,ncol = 3)
  cat("The given matrix is symmetric",isSymmetric(x1),"\n")
  x2 <- diag(3)
  print(x2)
  cat("The above matrix is symmetric",isSymmetric(x2))
}
#q19
{
  vec1_649 <- c(5,9,3)
  vec2_649 <- c(4,8,1,13,14,19)
  column.names_649 <- c("c1","c2","c3")
  row.names_649 <- c("r1","r2","r3")
  matrix.names_649 <- c("M1","M2","M3")
  result_649 <- array(c(vec1_649,vec2_649),dim = c(3,3,3),dimnames = list(row.names_649,column.names_649,matrix.names_649))
  print(result_649)
  cat("Sum of rows across all the matrices",rowSums(result_649))
}
#20
#21
{
  vec_649 <- c(10:0,0:11)
  m_649 <- diag(vec_649,21,21)
  print(m_649)
}
#22
{
diagonal_649 <- diag(rep.int(1, 6), 6, 7) 
below_the_diagonal_649 <- rbind(0, diagonal_649) 
diagonal_lower_649 <- diag(rep.int(1, 6), 7, 6) 
above_the_diagonal_649 <- cbind(0, diagonal_lower_649) 
on_the_diagonal_649 <- diag(abs(seq.int(-3, 3))) 
wilkinson_649 <- below_the_diagonal_649 + above_the_diagonal_649 + on_the_diagonal_649
print(wilkinson_649)
print("Eigenvalues of the wilkinson matrix are:")
eigen(wilkinson_649)$values 
}
#Q16
{
  x1_649 <- matrix(c(1:9),nrow = 3,ncol = 3)
  cat("The given matrix is symmetric",isSymmetric(x1_649),"\n")
  x2_649 <- diag(3)
  print(x2_649)
  cat("The above matrix is symmetric",isSymmetric(x2_649))
}

#Q15
{
  m <- matrix(c(3:12),nrow = 3,ncol=3, byrow = TRUE)
  m[m<5] <- 0
  print(m)
}
#Q14
{
  m_649 <- matrix(c(3:12),nrow = 3,ncol=3, byrow = TRUE)
  m_649 <- m_649[1:3,1:3] + 4
  print(m_649)
}

#q9
{
  vec <- c(1,2,4,8,10)
  m <- matrix(c(3:14),nrow = 4,byrow = TRUE)
  list1 <- list("Red","Green",c(21,32,11),TRUE,51.23)
  list_data <- list(vec,m,list1)
  print(list_data(2))
}

#q10
{
  list1_649 <- list(c("ALPHA","BETA"),c(21,32,11), matrix(c(3,9,5,1,-2,8),nrow = 2))
  print(list1_649)
  list1_649[4] <- "New Element"
  print(list1_649[4])
  list1_649[2] <- NULL
  print(list1_649)
}

#q11
{
  lits1_649 <- list(1,2,3,4,5)
  list2_649 <- list("January","feburary","March","April","May")
  merged.list_649 <- c(list1_649,list2_649)
  print(merged.list_649)
}


#q8
{
  vec_649 <- 1:5
  mat_649 <- matrix(c(1:9, ncol = 5))
  
  df_649 <- EuStockMarkets[1:10,]
  my_list_649 <- list(vec_649,mat_649,df_649)
  my_list_649
}
#q9
{
  vec_649 <- c(1,2,4,8,10)
  m_649 <- matrix(c(3:14),nrow = 4,byrow = TRUE)
  list1_649 <- list("Red","Green",c(21,32,11),TRUE,51.23)
  list_data_649 <- list(vec_649,m_649,list1_649)
  print(list_data_649(2))
}
m <- matrix(c(3:14),nrow = 4,byrow = TRUE)
M
#Q20
n_649 - seq(from = 1, to = 20, by = 1)
print(n_649)
vector_649<-c()
name_649 <- head(letters, 20)
name_649
for(m_649 in n_649){
  vector_649<-append(vector_649,m_649*(m_649+1)/2)
  names(name_649)<-vector_649
}
print(vector_649)
name_649
vowels_649 <- c("a","e","i","o","u")
vowel_649 <- c(1,5,9,15)
names(name_649(vowel_649))
                
#Q22

(a <- wilkinson(7))
eig(a)