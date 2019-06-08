hybrid<-read.csv("hybrid_reg.csv")
hybriddf <- data.frame(hybrid)
hybriddf$mpgmpge <- cut(hybriddf$mpgmpge, breaks=c(10,20,30,40,50,60), right = FALSE )

t(aggregate(accelrate~mpgmpge,data=hybriddf,summary))

attach(hybrid)
par(mfrow=c(1,3))
plot(mpgmpge,accelrate, xlab="mpgmpge",ylab="accelrate")
new<-data.frame(mpgmpge=seq(10,100,1))
lines(seq(10,100,1),predict(lm(accelrate~mpgmpge),newdata=new),col="red",lwd=4)

plot(as.factor(carclass), accelrate,xlab="car class",ylab="accelerate",title="Auto Data Set",col=c(7,32,57,82,107))

plot(msrp,accelrate)
new2<-data.frame(msrp=seq(10000,150000,1))
lines(seq(10000,150000,1),predict(lm(accelrate~msrp),newdata=new2),col="red",lwd=4)

pairs(hybrid[,-c(1,2,9)], col=carclass)

library(car)     #where vif function lives
Hybrid2<-hybrid[,-c(1,2,9)]  #removing the car name
full.model<-lm(accelrate~.,data=Hybrid2)  # . means all variable not mpg
vif(full.model)[,3]^2

par(mfrow=c(1,3))
plot(mpgmpge,accelrate, xlab="mpgmpge",ylab="accelrate")
new<-data.frame(mpgmpge=seq(10,110,.1))
mpgmpge.model<-lm(accelrate~mpgmpge)
lines(seq(10,110,.1),predict(mpgmpge.model,newdata=new),col="red",lwd=4)
plot(mpgmpge.model$fitted.values,mpgmpge.model$residuals,xlab="Fitted Values",ylab="Residuals")
plot(mpgmpge,mpgmpge.model$residuals,xlab="mpgmpge",ylab="Residuals")

par(mfrow=c(1,3))
plot(mpgmpge,accelrate, xlab="mpgmpge",ylab="accelrate")
new<-data.frame(mpgmpge=seq(10,110,.1))
mpgmpge.model2<-lm(accelrate~mpgmpge+I(mpgmpge^2))
lines(seq(10,110,.1),predict(mpgmpge.model2,newdata=new),col="red",lwd=4)
plot(mpgmpge.model2$fitted.values,mpgmpge.model$residuals,xlab="Fitted Values",ylab="Residuals")
plot(mpgmpge,mpgmpge.model2$residuals,xlab="mpgmpge",ylab="Residuals")

par(mfrow=c(1,3))
plot(mpgmpge,accelrate, xlab="mpgmpge",ylab="accelrate", col=carclass)
new<-data.frame(mpgmpge=seq(10,110,.1))
mpgmpge.model2<-lm(accelrate~mpgmpge+I(mpgmpge^2))
lines(seq(10,110,.1),predict(mpgmpge.model2,newdata=new),col="red",lwd=4)
plot(mpgmpge.model2$fitted.values,mpgmpge.model$residuals,xlab="Fitted Values",ylab="Residuals")
plot(mpgmpge,mpgmpge.model2$residuals,xlab="mpgmpge",ylab="Residuals")

par(mfrow=c(2,2))
plot(mpgmpge.model2)

my.model<-lm(log(accelrate)~mpgmpge+I(mpgmpge^2))
par(mfrow=c(1,3))
plot(my.model$fitted.values,my.model$residuals,xlab="Fitted Values",ylab="Residuals")
plot(mpgmpge,my.model$residuals,xlab="mpgmpge",ylab="Residuals")
qqnorm(my.model$residuals)
qqline(my.model$residuals)
summary(my.model)


library(leaps)
hybrid3<-hybrid[,-c(1,2,9)]
reg.fwd=regsubsets(log(accelrate)~.,data=hybrid3,method="forward",nvmax=20)
summary(reg.fwd)$adjr2
summary(reg.fwd)$rss
summary(reg.fwd)$bic
par(mfrow=c(1,3))
bics<-summary(reg.fwd)$bic
plot(1:10,bics,type="l",ylab="BIC",xlab="# of predictors")
index<-which(bics==min(bics))
points(index,bics[index],col="red",pch=10)

adjr2<-summary(reg.fwd)$adjr2
plot(1:10,adjr2,type="l",ylab="Adjusted R-squared",xlab="# of predictors")
index<-which(adjr2==max(adjr2))
points(index,adjr2[index],col="red",pch=10)

rss<-summary(reg.fwd)$rss
plot(1:10,rss,type="l",ylab="train RSS",xlab="# of predictors")
index<-which(rss==min(rss))
points(index,rss[index],col="red",pch=10)

set.seed(1234)
hybrid4<-hybrid[,-c(1,2,8)]
index<-sample(1:dim(hybrid4)[1],100,replace=F)
train<-hybrid4[index,]
test<-hybrid4[-index,]
reg.fwd=regsubsets(log(accelrate)~.,data=train,method="forward",nvmax=6)
reg.fwd
#Really handy predict function
predict.regsubsets =function (object , newdata ,id ,...){
  form=as.formula (object$call [[2]])
  mat=model.matrix(form ,newdata )
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}


reg.final=regsubsets(log(accelrate)~.,data=hybrid3,method="forward",nvmax=4)
coef(reg.final,3)

final.model<-lm(log(accelrate)~year+msrp+mpgmpge,data=hybrid3)
summary(final.model)

plot(exp(final.model$fitted.values),hybrid3$accelrate,xlab="Predicted",ylab="accelrate",xlim=c(0,30),ylim=c(0,30))
lines(c(0,30),c(0,30),col="red")

attach(hybrid)
mysummary<-function(x){
  result<-c(length(x),mean(x),sd(x),sd(x)/length(x))
  names(result)<-c("N","Mean","SD","SE")
  return(result)
}
sumstats<-aggregate(accelrate~mpgmpge*carclass,data=hybrid,mysummary)
sumstats<-cbind(sumstats[,1:2],sumstats[,-(1:2)])
sumstats

library(ggplot2)
ggplot(sumstats,aes(x=mpgmpge,y=Mean,group=carclass,colour=carclass))+
  ylab("Accelrate")+
  geom_line()+
  geom_point()+
  geom_errorbar(aes(ymin=Mean-SE,ymax=Mean+SE),width=.1)

model.fit<-aov(accelrate~mpgmpge+carclass+mpgmpge:carclass,data=hybrid)
par(mfrow=c(1,2))
plot(model.fit$fitted.values,model.fit$residuals,ylab="Resdiduals",xlab="Fitted")
qqnorm(model.fit$residuals)

library(gridExtra)
myfits<-data.frame(fitted.values=model.fit$fitted.values,residuals=model.fit$residuals)

#Residual vs Fitted
plot1<-ggplot(myfits,aes(x=fitted.values,y=residuals))+ylab("Residuals")+
  xlab("Predicted")+geom_point()

#QQ plot of residuals  #Note the diagonal abline is only good for qqplots of normal data.
plot2<-ggplot(myfits,aes(sample=residuals))+
  stat_qq()+geom_abline(intercept=mean(myfits$residuals), slope = sd(myfits$residuals))

#Histogram of residuals
plot3<-ggplot(myfits, aes(x=residuals)) + 
  geom_histogram(aes(y=..density..),binwidth=1,color="black", fill="gray")+
  geom_density(alpha=.1, fill="red")

grid.arrange(plot1, plot2,plot3, ncol=3)

library(car)
Anova(model.fit,type=3)