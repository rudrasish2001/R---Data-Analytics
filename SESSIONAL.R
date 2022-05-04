data(package = .packages(all.available = TRUE))
help(PlantGrowth)
View(PlantGrowth)
nrow(PlantGrowth)
ncol(PlantGrowth)
dim(PlantGrowth)
names(PlantGrowth)
str(PlantGrowth)
attributes(PlantGrowth)
head(PlantGrowth, 10) 
tail(PlantGrowth, 10) 
pairs(PlantGrowth[1:4],main='Data Distribution', pch=21, bg=c('red','green3','blue')[unclass(PlantGrowth$weight)])

#2
sum_649 <- function(mat){
  for (r in 1:nrow(A)){
    for(c in 1:ncol(A)){
      if(r>c){
        d2=d2+A[r,c]
      }
    }
  }
  print(d2)
}


A = matrix(
   
  c(1, 2, 3, 4, 5, 6, 7, 8, 9),
  
  nrow = 3,  
  
  ncol = 3,        
  
  byrow = TRUE         
)
d2=0
rownames(A) = c("a", "b", "c")

colnames(A) = c("c", "d", "e")

cat("The 3x3 matrix:\n")
print(A)

sum_649(A)
#5

A = matrix(
   
  c(1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14,15,16),
  
  nrow = 4,  
  
  ncol = 4,        
  
  byrow = TRUE         
)

rownames(A) = c("a", "b", "c", "d")

colnames(A) = c("c", "d", "e", "f")

cat("The 3x3 matrix:\n")
print(A)

diag(A) = 0

print(A)

for (r in 1:nrow(A)){
  for(c in 1:ncol(A)){
    if(r>c){
      A[r,c] = -1
    }
    else if(r<c){
      A[r,c] = 1
    }
  }
}
print(A)

data("PlantGrowth")
head(PlantGrowth,6)
subset(PlantGrowth,weight>=3 & weight < 5.5)

mat_649 <- matrix(1:9, 3, 3)
mat_649[lower.tri(mat_649)] <- -1
print(mat_649)
mat_649[upper.tri(mat_649)]<- 1
print(mat_649)




wt_lbs.6375 <- subset(PlantGrowth, subset = weight >= 3 & weight < 5.5)
boxplot(wt_lbs, main = "Box Plot By Roll Number 1906518", xlab = "Weight", ylab = "", col = "pink", border = "brown", horizontal = TRUE, notch = TRUE
)
