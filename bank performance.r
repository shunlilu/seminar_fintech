data = read.csv('./Rproj/Seminar/cleaned2.csv')

summary(data)

library(randomForest)
set.seed(1)
train = sample(1:nrow(data),nrow(data)/2)

head(train)
names(data)
is.na(data)
na.omit(data)
is.na(data['trueint'])


summary(train)
bank.test = data[-train,"intincy"]
summary(bank.test)

set.seed(1)
bag.loan = randomForest(intincy~.,data = data,subset = train,mtry = 13, importance = TRUE)
bag.loan

yhat.bag = predict(bag.loan,newdata = data[-train,])

plot(yhat.bag,bank.test)
mean((yhat.bag-bank.test)^2)
varImpPlot(bag.loan)
importance(bag.loan)
summary(data1["BorrowerState"])