library ('data.table')
data_table <- data.table(Blood_group = sample(c('A', 'O', 'A', 'B', 'B', 'AB', 'B', 'B', 'O', 'A', 'O', 'O', 'O', 'AB', 'B', 'AB', 'AB', 'A', 'O', 'A'),replace = TRUE))
print ("Original DataFrame")
print (data_table)
freq <- table(data_table$Blood_group)

print ("Modified Frequency Table")
print (freq)

print ("Cumulative Frequency Table")
cumsum <- cumsum(freq)
print (cumsum)

install.packages("readr")
library(readr)
data_649<-read_csv("Q2-1906649 - Sheet1.csv",show_col_types = FALSE)
View(data_649)
mean(data_649$Frequency)
median(data_649$Frequency)
sd(data_649$Frequency)
install.packages("moments")
library(moments)

skewness(data_649$Frequency)


print ("Relative Frequency Table",`show_col_types = FALSE`)
prob <- prop.table(freq)
print (prob*100)


#Q-1
bloodgroup_649 <- c("A", "O", "A", "B", "B", "AB", "B", "B", "O", "A", "O", "O", "O", "AB", "B", "AB", "AB", "A", "O", "A")
freqtable_649 <- table(bloodgroup_649)
proportion_649 <- prop.table(freqtable_649)
percentagetable.511 <- proportion_649*100
cumulativefreqtable_649 <- cumsum(freqtable_649)

bgframe_649 <- data.frame(freqtable_649,proportion_649 , proportion_649 , cumulativefreqtable_649 )
colnames(bgframe_649) <- c("data","frequency","data1","proportion","data2","percentage(%)","cumulative_frequency")
print(bgframe_649)
bgframe_649= subset(bgframe_649, select = -c(data, data1, data2) )
cat("Frequency Distribution Table:-")
bgframe_649

#q2
distribution_649 <- data.frame(
  Class_Interval = c("0-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80"),
  Frequency = c(6, 12, 22, 48, 56, 32, 18, 6)
)
distribution_649
median_649 = median(distribution_649$Frequency)
print(median_649)
mean_649 = mean(distribution_649$Frequency)
print(mean_649)
stddeviation_649 = sd(distribution_649$Frequency)
print(stddeviation_649)

skewness_649 = 3*(mean_649-median_649)/stddeviation_649
print(skewness_649)
print("The data is average skewed as skewness is between -1 and – 0.5 or between 0.5 and 1 ")

#q3
natdist_649 <- data.frame(
  x = c(14,14,14,14,14),
  y = c(11,12,14,16,17),
  z = c(1,3,6,8,42)
)
natdist_649
print("statistical summary of data:-")
print(summary(natdist_649))
print(class(natdist_649))
print(paste("x is",class(natdist_649$x)))
print(paste("y is",class(natdist_649$y)))
print(paste("z is",class(natdist_649$z)))


#q4
Before_Dispute_649 <- data.frame(
  Mean_wages = 850,
  Median_wages = 820,
  Number_employed = 600,
  Standard_distribution = 30,
  First_quartile = 750,
  Third_quartile = 920,
  Modal_wages = 760
)
After_Dispute_649 <- data.frame(
  Mean_wages = 900,
  Median_wages = 800,
  Number_employed = 550,
  Standard_distribution = 110,
  First_quartile = 750,
  Third_quartile = 950,
  Modal_wages = 600
)
print(paste("Numbers of Employers decreased = ",Before_Dispute_649$Number_employed-After_Dispute_649$Number_employed))
twagebd_649 = Before_Dispute_649$Number_employed*Before_Dispute_649$Mean_wages
print(paste("Wages before Dispute = Rs.",twagebd_649))
twagead_649 = After_Dispute_649$Number_employed*After_Dispute_649$Mean_wages
print(paste("Wages after Dispute = Rs.",twagead_649))
print(paste("Total decreased wages = Rs.", twagebd_649 - twagead_649))

print(paste("The median & modal wage have decreased ",Before_Dispute_649$Median_wages-After_Dispute_649$Median_wages,"and ",Before_Dispute_649$Modal_wages-After_Dispute_649$Modal_wages))
print(paste("Q1 has not changed but  Q2  has decreased slightly also  Q3 has increased ",After_Dispute_649$Third_quartile-Before_Dispute_649$Third_quartile,"."))
cvbefored_649 = Before_Dispute_649$Standard_distribution/Before_Dispute_649$Mean_wages*100
print(paste("C.V.  before dispute = ",cvbefored_649,"%"))
cvafterd_649 = After_Dispute_649$Standard_distribution/After_Dispute_649$Mean_wages*100
print(paste("C.V. after dispute) = ",cvafterd_649,"%"))

print(paste("Measure of skewness are:"))
pearsonmbefored_649 = (Before_Dispute_649$Mean_wages-Before_Dispute_649$Modal_wages)/Before_Dispute_649$Standard_distribution
pearsonmafterd_649 = (After_Dispute_649$Mean_wages-After_Dispute_649$Modal_wages)/After_Dispute_649$Standard_distribution
pearsonmafterd_649 = (Before_Dispute_649$Third_quartile-(2*Before_Dispute_649$Median_wages)+Before_Dispute_649$First_quartile)/(Before_Dispute_649$Third_quartile-Before_Dispute_649$First_quartile)
bowleymafterd_649 = (After_Dispute_649$Third_quartile-(2*After_Dispute_649$Median_wages)+After_Dispute_649$First_quartile)/(After_Dispute_649$Third_quartile-After_Dispute_649$First_quartile)
print(paste("Pearson's Measure before dispute = ",pearsonmbefored_649))
print(paste("Pearson's Measure after dispute = ",pearsonmafterd_649))
print(paste("Bowley's Measure before dispute = ",pearsonmafterd_649))
print(paste("Bowley's Measure after dispute = ",bowleymafterd_649))

#q5
M_649 <- data.frame(
  Size_of_items = c("10-12","12-14","14-16","16-18","18-20"),
  Frequency = c(27,20,12,6,3)
)
N_649 <- data.frame(
  Size_of_items = c("10-12","12-14","14-16","16-18","18-20"),
  Frequency = c(3,6,12,20,27)
)
print(summary(M_649))
print(summary(N_649))
skewness1_649 = -3*(mean(M_649$Frequency)-median(M_649$Frequency))/sd(M_649$Frequency)
print(skewness1_649)
plot(M_649$Frequency, main = "Negatively skewed")
lines(M_649$Frequency)
skewness2_649 = 3*(mean(N_649$Frequency)-median(N_649$Frequency))/sd(N_649$Frequency)
print(skewness2_649)
plot(N_649$Frequency, main = "Positively skewed")
lines(N_649$Frequency)
#q6
marks_649 <- c(55,75,65,30,90,55,40,50,60,80,80,76,95,75,55,45,65,80,30,50,75,85,80,90,75,75,90,65,78,72,82,52,62,67,66,65,88,45,70)
freq_649 <- table(marks_649)
print(freq_649)

#measures of central tendency
print(paste("Mean =",mean(marks_649)))
print(paste("Median =",median(marks_649)))
getMode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
print(paste("Mode =",getMode(marks_649)))

#measures of Dispersion or Variation
print(paste("Range =",(max(marks_649)-min(marks_649))))
print(paste("Interquartile Range =",IQR(marks_649)))
print(paste("Variance =", var(marks_649)))
print(paste("Standard Deviation =",sd(marks_649)))

#measures of Position
res_649 <- quantile(marks_649, probs = c(0,0.25,0.5,0.75,1))
print("Quantile =")
print(res_649)
print("Five Number Summary =")
print(summary(marks_649))
print("Z-Score =")
print((marks_649-mean(marks_649))/sd(marks_649))

#q7
install.packages("moments")
library(moments)
kurtosis_649 <- c(0, 3, 4, 1, 2, 3, 0, 2, 1, 3, 2, 0, 2, 2, 3, 2, 5, 2, 3, 999)
print(kurtosis(kurtosis_649))
print(paste("Excess Kurtosis =",(kurtosis(kurtosis_649)-3),". It gives positive value and It is Leptokurtic Distribution."))

#q8
infant_649<- data.frame(
  Infant_ID = c(1:17),
  Gestational_Age_weeks = c(34.7, 36, 29.3, 40.1, 35.7, 42.4, 40.3, 37.3, 40.9, 38.3, 38.5, 41.4, 39.7, 39.7, 41.1, 38.0, 38.7),
  Birth_weight_gm = c(1895,2030,1440,2835,3090,3827,3260,2690,3285,2920,3430,3657,3685,3345,3260,2680,2005)
)
print(infant_649)
print(paste("Correlation =",cor(infant_649$Gestational_Age_weeks, infant_649$Birth_weight_gm)))
print(paste("Covariance =",cov(infant_649$Gestational_Age_weeks, infant_649$Birth_weight_gm)))
plot(infant_649$Gestational_Age_weeks,infant_649$Birth_weight_gm, main = "Birth Weight vs. Gestational Age", pch = 19, frame = FALSE)
abline(lm(infant_649$Birth_weight_gm ~ infant_649$Gestational_Age_weeks), col = "blue")
print("Two variables tend to move in same direction. So it indicates positive covariance and highly positively correlated.")

#q9
df_649 <- data.frame(
  Subject = c('A','B','C','D','E','F','G','H','I'),
  Normal = c(56,56,65,65,50,25,87,44,35),
  Hypervent = c(87,91,85,91,75,28,122,66,58)
)
print(df_649)
print(paste("Correlation =",cor(df_649$Normal,df_649$Hypervent)))
print(paste("Covariance =",cov(df_649$Normal,df_649$Hypervent)))
plot(df_649$Normal,df_649$Hypervent, main = "Hypervent vs. Normal", pch = 19, frame = FALSE)
abline(lm(df_649$Hypervent~df_649$Normal), col = "blue")
print("Two variables tend to move in same direction. So it indicates positive covariance and highly positively correlated.")

#q10
occurance_649 <- c(4, 1, 2, 1, 3, 2, 1, 1)
value_649 <- c(0:7)
barplot(occurance_649, names.arg = value_649, xlab ="649_Values", ylab ="649_Occurences", col ="blue")
res_649 <- quantile(occurance_649, probs = c(0,0.25,0.5,0.75,1))
print(res_649)
print(summary(occurance_649))
zscore_649 <- (occurance_649 - mean(occurance_649))/sd(occurance_649)
data_649<- data.frame(value_649, occurance_649, zscore_649)
colnames(data_649) <- c("Values_649","Occurences_649","Standard Score_649")
print(data_649)

#q11
stock_649 <- data.frame(
  Month = c(1:5),
  Return_of_stock_A = c(2.3,2.5,1.9,2.4,2.1),
  Return_of_Market_Index = c(1.3,5.0,0.8,1.9,1.1)
)
print(stock_649 )
print(paste("Correlation Coefficient=",cor(stock_649 $Return_of_stock_A,stock_649 $Return_of_Market_Index)))
print(paste("Covariance =",cov(stock_649 $Return_of_stock_A,stock_649 $Return_of_Market_Index)))

#q12
data()
data(faithful)
View(faithful)
summary(faithful)
print(paste("Correlation Coefficient=",cor(faithful$eruptions,faithful$waiting)))
print(paste("Covariance =",cov(faithful$eruptions,faithful$waiting)))
plot(faithful$eruptions,faithful$waiting,col = "blue",main = "Eruptions & Waiting Regression",pch = 19,frame = FALSE,xlab = "Faithful$Eruptions",ylab = "Faithful$Waiting")
abline(lm(faithful$waiting~faithful$eruptions),col = "red")
print("Linear Relation =")
association_649 <- lm(faithful$eruptions~faithful$waiting)
print(association_649)
print(summary(association_649))