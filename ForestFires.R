install.packages("e1071")
library("e1071")

#read and store csv file
ds = read.csv("C:/Program Files/RStudio/DataMining/dataSet.csv")

#copy of dataset
ds2 = ds

#removing unneeded columns (note : we remove discount_price_amount because 100% of filtered data are 455)
ds2 = subset(ds2, select = c('id','title','url','is_paid','num_subscribers','rating','num_published_lectures','published_time','price_detail__amount'))

#Checking for missing values
sapply(ds2, function(x) sum(is.na(x)))

#replace NAs values in a col_name as
ds2$price_detail__amount[is.na(ds2$price_detail__amount)] <- 0

#remove observations which has 0 published lectures
ds2 <- subset(ds2, ds2$num_published_lectures > 0)

#extract month from the published date and cluster it into summer and winter
ds2$published_time <- as.Date(ds2$published_time, format = "%Y-%m-%d")
ds2$published_time <- format(ds2$published_time, "%m")
ds2$published_time = as.numeric(ds2$published_time)
ds2$published_time = ifelse(ds2$published_time<6,"winter",ifelse(ds2$published_time>10,"winter","summer"))

#change zeros in discount_price_amount with NA when isPaid is False

#string to logical

#remove outliers from num_subscribers
q1 = as.numeric(summary(ds2$num_subscribers)[2])
q3 = as.numeric(summary(ds2$num_subscribers)[5])
iqr = q3 - q1
ds2 <- subset(ds2 , ds2$num_subscribers <= q3 + iqr*1.5)
ds2 <- subset( ds2 , ds2$num_subscribers >= q1 - (iqr*1.5))

#remove outliers from rating
q1 = as.numeric(summary(ds2$rating)[2])
q3 = as.numeric(summary(ds2$rating)[5])
iqr = q3 - q1
ds2 <- subset(ds2 , ds2$rating <= q3 + iqr*1.5)
ds2 <- subset(ds2 , ds2$rating >= q1 - (iqr*1.5))

#remove outliers from price_detail amount
q1 = as.numeric(summary(ds2$price_detail__amount)[2])
q3 = as.numeric(summary(ds2$price_detail__amount)[5])
iqr = q3 - q1
ds2 <- subset(ds2 , ds2$price_detail__amount <= q3 + iqr*1.5)
ds2 <- subset(ds2 , ds2$price_detail__amount >= q1 - (iqr*1.5))


#convert rating to high,medium,low according to five numbers 
ds2$rating = ifelse(ds2$rating<3.889,"low",ifelse(ds2$rating>=4.478,"high","medium"))

#convert num_subscribers to high,medium,low according to five numbers
ds2$num_subscribers = ifelse(ds2$num_subscribers<70,"low",ifelse(ds2$num_subscribers>=1536,"high","medium"))

#convert price_detail__amount to high,medium,low according to five numbers
ds2$price_detail__amount = ifelse(ds2$price_detail__amount<1600,"low",ifelse(ds2$price_detail__amount>=8320,"high","medium"))

#factoring nedded data
ds2$published_time = as.factor(ds2$published_time)
ds2$rating = as.factor(ds2$rating)
ds2$num_subscribers = as.factor(ds2$num_subscribers)
ds2$price_detail__amount = as.factor(ds2$price_detail__amount)

#devide data into trainging and testing
TrainIndex <- sample(nrow(ds2), nrow(ds2)*0.7)
TrainingData <- ds2[TrainIndex,]
TestingData<-ds2[-TrainIndex,]

#we applyed naiveBays method on published_time as class
nb <- naiveBayes(published_time~., ds2, laplace = 1)

#build prediction table
predictionTable <- table(predict(nb, TestingData),TestingData$published_time)
table(predict(nb,TrainingData),TrainingData$published_time)
table(predict(nb,TestingData),TestingData$published_time)

#calculate accuracy for our calculations
misclassification = 1 - sum(diag(predictionTable)) / sum(predictionTable)
accuracy = (1-misclassification)*100


#another naiveBays accoring to num_subscribers
TrainIndex <- sample(nrow(ds2), nrow(ds2)*0.7)
TrainingData <- ds2[TrainIndex,]
TestingData<-ds2[-TrainIndex,]

nb <- naiveBayes(num_subscribers~., ds2, laplace = 1)
predictionTable <- table(predict(nb, TestingData),TestingData$num_subscribers)
table(predict(nb,TrainingData),TrainingData$num_subscribers)
table(predict(nb,TestingData),TestingData$num_subscribers)

#calculate accuracy
misclassification = 1 - sum(diag(predictionTable)) / sum(predictionTable)
accuracy = (1-misclassification)*100