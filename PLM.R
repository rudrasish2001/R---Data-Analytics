set.seed(1)
df <- data.frame(hours = runif(50, 5, 15), score=50)
df$score = df$score + df$hours^3/150 + df$hours*runif(50, 1, 2)
head(data)
library(ggplot2)
ggplot(df, aes(x=hours, y=score)) +
  geom_point()
df.shuffled <- df[sample(nrow(df)),]
K <- 10 
degree <- 5
folds <- cut(seq(1,nrow(df.shuffled)),breaks=K,labels=FALSE)
mse = matrix(data=NA,nrow=K,ncol=degree)
for(i in 1:K){
  
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData <- df.shuffled[testIndexes, ]
  trainData <- df.shuffled[-testIndexes, ]
  for (j in 1:degree){
    fit.train = lm(score ~ poly(hours,j), data=trainData)
    fit.test = predict(fit.train, newdata=testData)
    mse[i,j] = mean((fit.test-testData$score)^2) 
  }
}
colMeans(mse)
best = lm(score ~ poly(hours,2, raw=T), data=df)
summary(best)