library(readr)
data <- read_delim("data/data.csv", ";", escape_double = FALSE, trim_ws = TRUE)
#data<-read.csv2("data/data.csv")
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
penalty_min<-mod$lambda.min
penalty_min
real_mod<-glmnet(as.matrix(x),y,alpha=0.5,lambda = penalty_min,family="binomial")
pred<-predict(real_mod,as.matrix(x_test),type="class")
cft<-table(pred,y_test)
cft

library(kernlab)

#data <- read_delim("data/data.csv", ";", escape_double = FALSE, trim_ws = TRUE)
data<-read.csv2("data/data.csv",check.names = FALSE)
names(data)<-iconv(names(data),to="ASCII")
data
x=data[,-4703]
x=as.matrix(x)
y=data[[4703]]


fit<-ksvm(x,y,data=train,kernel="vanilladot",type="C-svc",scale=FALSE)
pred<-predict(fit,x_test,type="response")
pred
cft<-table(pred,y_test)
cft
mis_rate<-1-(cft[1,1]+cft[2,2])/sum(cft)
mis_rate

x=as.matrix(data[,-4703])
y=as.factor(data[[4703]])

df<-data.frame(name=c(),pvalue=c())

for(i in 1:ncol(x)){
  tmpv<-t.test(x[,i]~y,alternative="two.sided",conf.level=0.95)$p.value
  tdf<-data.frame(name=colnames(x)[i],pvalue=tmpv)
  df<-rbind(df,tdf)
}
df<-df[order(df$pvalue),] 

a=0.05
max_i=1
for(i in 1:length(df$pvalue)){
  if(df$pvalue[i]<=a*i/length(df$pvalue)){
    max_i=i
  }
}
max_i


#df[1:10,]
length(which(df$pvalue>0.05))
df$pvalue<-p.adjust(df$pvalue,method="BH")
length(which(df$pvalue<0.05))
new_pvalue<-p.adjust(df$pvalue,method = "BH")
new_pvalue
length(which(new_pvalue>0.05))

n=length(df$pvalue)
lp=length(df$pvalue)

i <- lp:1L
o<-order(df$pvalue,decreasing = TRUE)
ro<-order(o)

o <- order(p, decreasing = TRUE)

ro <- order(o)

pmin(1, cummin( n / i * df$pvalue[o] ))[ro]


