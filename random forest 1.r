data1 = read.csv('./Rproj/Seminar/prosperLoanData_clean_new_drop1.csv')
head(data1)

library(randomForest)
set.seed(1)
train = sample(1:nrow(data1),nrow(data1)/2)

head(train)
names(data1)
train.summary
summary(train)
loan.test = data1[-train,"BorrowerRate"]
summary(loan.test)

set.seed(1)
bag.loan = randomForest(BorrowerRate~.,data = data1,subset = train,mtry = 13, importance = TRUE)
bag.loan

yhat.bag = predict(bag.loan,newdata = data1[-train,])

plot(yhat.bag,loan.test)
mean((yhat.bag-loan.test)^2)
varImpPlot(bag.loan)
importance(bag.loan)
summary(data1["BorrowerState"])

aggregate(x = data1,by=list("BorrowerState","Term"),FUN = "mean",na.action = na.omit())

data2 = group_by(data1,"BorrowerState",add = FALSE)
print(data2)
head(data2)

datalibrary(dplyr)
n_distinct(data1["BorrowerState"],na.rm = FALSE)
n_distinct(data1["Occupation"],na.rm = FALSE)
n_distinct(data1["LoanOriginationQuarter"],na.rm = FALSE)
n_distinct(data1["BorrowerState"])
