library(readr)
data <- read_delim("data/data.csv", ";", escape_double = FALSE, trim_ws = TRUE)
library(pamr)
RNGversion("3.5.1")

n=dim(data)[1] 
set.seed(12345) 
id=sample(1:n, floor(n*0.7)) 
train=data[id,] 
test=data[-id,]

x<-t(train[,-4703])

y<-t(train[[4703]])


x_test<-t(train[,-4703])

y_test<-train[[4703]]

my_data<-list(x=x,y=as.factor(y),geneid=as.character(1:nrow(x)),genenames=rownames(x))
my_data_test<-list(x=x_test,y=as.factor(y_test),geneid=as.character(1:nrow(x_test)),genenames=rownames(x_test))

mod<-pamr.train(my_data,threshold = seq(0,4,0.1))
cvmodel<-pamr.cv(mod,my_data)
cvmodel$error
thr<-cvmodel$threshold[which.min(cvmodel$error)]
thr
pamr.plotcv(cvmodel)
pamr.predict(mod,my_data_test$x,threshold = thr)
res$yhat
pamr.plotcen(mod,my_data,thr)
listgene<-pamr.listgenes(mod,my_data,thr)[1:10]
my_data$genenames[as.numeric(listgene)]
library(glmnet)

x<-train[,-4703]
y<-train[[4703]]

x_test<-test[,-4703]
y_test<-test[[4703]]

mod<-cv.glmnet(as.matrix(x),y,alpha=0.5,family="binomial")
mod$lambda.1se
penalty_min<-mod$lambda.min
penalty_min
real_mod<-glmnet(as.matrix(x),y,alpha=0.5,lambda = penalty_min,family="binomial")
predict(real_mod,as.matrix(x_test),type="class")


library(kernlab)

data <- read_delim("data/data.csv", ";", escape_double = FALSE, trim_ws = TRUE)
data<-read.csv("data/data.csv")
data
x=data[,-4703]
y=data[[4703]]
y
fit<-ksvm(x,y,kernel="vanilladot")

fit

library(e1071)
mod<-svm(Conference~.,data=train,kernel="vanilladot")
summary(mod)
pred<-predict(mod,test[,-4703])
pred


t<-t.test(Conference~.,data=train)

