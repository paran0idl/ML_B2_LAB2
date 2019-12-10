library(ggplot2)
library(readxl)
data<-read_xlsx("data/influenza.xlsx")
ggplot(data=data)+geom_line(aes(x=Time,y=Mortality),col="red")+
  geom_line(aes(x=Time,y=Influenza),col="blue")

# Q2
library(mgcv)
res=gam(Mortality~Year+s(Week,k=length(unique(data$Week))),method ="GCV.Cp" ,data=data)
res$sp
summary(res)
#plot(res)

pred=predict.gam(res,data)

ggplot()+geom_line(aes(x=data$Time,y=pred),col="blue")+geom_line(aes(x=data$Time,y=data$Mortality),col="red")

# Q3
res=gam(Mortality~Year+s(Year,k=length(unique(data$Year)))+s(Week,k=length(unique(data$Week))),method ="GCV.Cp" ,data=data)
summary(res)

plot(res)

# Q4
res=gam(Mortality~Year+s(Week,k=length(unique(data$Week)),sp=1000),method ="GCV.Cp" ,data=data)
plot(res)
res=gam(Mortality~Year+s(Week,k=length(unique(data$Week)),sp=0.00000001),method ="GCV.Cp" ,data=data)
plot(res)


# Q5
res=gam(Mortality~Year+s(Week,k=length(unique(data$Week))),method ="GCV.Cp" ,data=data)
pred=predict.gam(res,data)
res=data$Mortality-pred

ggplot()+geom_point(aes(x=data$Year,y=res),col="red")+geom_point(aes(x=data$Year,y=data$Influenza),col="blue")


# Q6
res=gam(Mortality~s(Year,k=length(unique(data$Year)))+s(Week,k=length(unique(data$Week)))+s(Influenza,k=length(unique(data$Influenza))),method ="GCV.Cp" ,data=data)
summary(res)
pred=predict.gam(res,data)

ggplot()+geom_line(aes(x=data$Time,y=pred),col="blue")+geom_line(aes(x=data$Time,y=data$Mortality),col="red")


