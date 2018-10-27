library(randomForest)

data <- read.csv('mushrooms.csv')
names <- names(data)
cat('Number of rows: ', nrow(data))
cat('Number of cols: ', ncol(data))

for (i in 1:length(names)){
  cat(names[i],'\n')
}

## split a training set

data$class <- as.factor(data$class)
partition <- sample(nrow(data), 0.7 * nrow(data))
train <- data[partition,]
# test <- data[-partition,]

# test various numbers of trees

ntrees <- 100
df <- data.frame(ntrees=as.numeric(),
                 accuracy=as.numeric())
cat("done!")

for (i in 1:10){
  RFModel <- randomForest(
    train[,-14],
    train[,14],
    xtest=test[,-14],
    ytest=test[,14],
    ntree=ntrees,
    mtry = 10,
    proximity=TRUE
  )
  # Test the RF model for this run
  preds <- levels(train[,14])[RFModel$test$predicted]
  # compute accuracy
  auc <- (sum(preds==test[,14])/nrow(testing))*100
  df <- rbind(df, data.frame(NTrees=ntrees,Accuracy=auc))
  ntrees <- ntrees + 100
}

library(ggplot2)
p <- ggplot(data, aes(x=NTrees, y=Accuracy))
p <- p + geom_line() + geom_point()
p <- p + xlim(100,1000)
p <- p + theme_bw()
p


