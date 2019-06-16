#Load dataset
hybrid<-read.csv("hybrid_reg.csv")
summary(hybrid)
## accelrate with mpg and carclass - no interaction
mod.mpg.class.nointer = lm(accelrate ~ mpg + carclass,data=hybrid)
par(mfrow=c(2,2))
plot(mod.mpg.class.nointer)
summary(mod.mpg.class.nointer)
par(mfrow=c(1,1))
plot(mpg[carclass=="C"], accelrate[carclass=="C"], col=1, pch=16,
     xlab="mpg", ylab="accelrate", 
     main="accelrate vs mpg,carclass",
     ylim=c(0,40), xlim=c(10,80))
points(mpg[carclass=="L"], accelrate[carclass=="L"], 
       col=2,pch=16)
points(mpg[carclass=="M"], accelrate[carclass=="M"], 
       col=3,pch=16)
points(mpg[carclass=="MV"], accelrate[carclass=="MV"], 
       col=4,pch=16)
points(mpg[carclass=="PT"], accelrate[carclass=="PT"], 
       col=5,pch=16)
points(mpg[carclass=="SUV"], accelrate[carclass=="SUV"], 
       col=6,pch=16)
points(mpg[carclass=="TS"], accelrate[carclass=="TS"], 
       col=8,pch=16)
abline(a=14.74030, b=-0.11259, col=1, lwd=3) #C
abline(a=(14.74030+4.16459), b=-0.11259, 
       col=2, lwd=3) #L
abline(a=(14.74030+1.92070), b=-0.11259, 
       col=3, lwd=3) #M
abline(a=(14.74030-1.34222), b=-0.11259,
       col=4, lwd=3) #MV
abline(a=(14.74030-1.61568), b=-0.11259,
       col=5, lwd=3) #PT
abline(a=(14.74030+1.10864), b=-0.11259,
       col=6, lwd=3) #SUV
abline(a=(14.74030+0.59464), b=-0.11259,
       col=8, lwd=3) #TS
legend(15,35, legend=
         c("C", "L", "M", "MV", "PT", "SUV", "TS"), 
       col=c(1,2,3,4,5,6,8), 
       pch=c(16,16,16,16,16,16,16), 
       bty = "n",horiz=TRUE)

## accelrate with mpg and carclass - interaction
mod.mpg.class.inter = lm(accelrate ~ mpg + carclass + 
                           mpg:carclass,
                         data=hybrid)
par(mfrow=c(2,2))
plot(mod.mpg.class.inter)
summary(mod.mpg.class.inter)
par(mfrow=c(1,1))
plot(mpg[carclass=="C"], accelrate[carclass=="C"], 
     col=1, pch=16,
     xlab="mpg", ylab="accelrate", 
     main="accelrate vs mpg,carclass",
     ylim=c(0,40), xlim=c(10,80))
points(mpg[carclass=="L"], accelrate[carclass=="L"], 
       col=2,pch=16)
points(mpg[carclass=="M"], accelrate[carclass=="M"], 
       col=3,pch=16)
points(mpg[carclass=="MV"], accelrate[carclass=="MV"], 
       col=4,pch=16)
points(mpg[carclass=="PT"], accelrate[carclass=="PT"], 
       col=5,pch=16)
points(mpg[carclass=="SUV"], accelrate[carclass=="SUV"], 
       col=6,pch=16)
points(mpg[carclass=="TS"], accelrate[carclass=="TS"], 
       col=8,pch=16)
abline(a=12.02325, b=-0.05019, col=1, lwd=3) #C
abline(a=(12.023258 + 8.36455), b=(-0.05019 -0.11931), 
       col=2, lwd=3) #L
abline(a=(12.02325+5.58129), b=(-0.05019 -0.08874), 
       col=3, lwd=3) #M
abline(a=(12.02325-0.55122), b=(-0.05019 -0.02332), 
       col=4, lwd=3) #MV
abline(a=(12.02325-4.80128), b=(-0.05019 + 0.24556), 
       col=5, lwd=3) #PT
abline(a=(12.02325+4.53937), b=(-0.05019 -0.08984), 
       col=6, lwd=3) #SUV
abline(a=(12.02325+1.42846), b=(-0.05019 -0.02377), 
       col=8, lwd=3) #TS
legend(15,35, legend=
         c("C", "L", "M", "MV", "PT", "SUV", "TS"), 
       col=c(1,2,3,4,5,6,8), pch=c(16,16,16,16,16,16,16), 
       bty = "n",horiz=TRUE)

## accelrate with msrp and carclass - no interaction
mod.msrp.class.nointer = lm(accelrate ~ msrp + carclass, 
                            data=hybrid)
par(mfrow=c(2,2))
plot(mod.msrp.class.nointer)
summary(mod.msrp.class.nointer)
par(mfrow=c(1,1))
plot(msrp[carclass=="C"], accelrate[carclass=="C"], 
     col=1, pch=16,
     xlab="msrp", ylab="accelrate", 
     main="accelrate vs msrp,carclass",
     ylim=c(0,40), xlim=c(10000,150000))
points(msrp[carclass=="L"], accelrate[carclass=="L"], 
       col=2,pch=16)
points(msrp[carclass=="M"], accelrate[carclass=="M"], 
       col=3,pch=16)
points(msrp[carclass=="MV"], accelrate[carclass=="MV"], 
       col=4,pch=16)
points(msrp[carclass=="PT"], accelrate[carclass=="PT"], 
       col=5,pch=16)
points(msrp[carclass=="SUV"], accelrate[carclass=="SUV"], 
       col=6,pch=16)
points(msrp[carclass=="TS"], accelrate[carclass=="TS"], 
       col=8,pch=16)
abline(a=7.418, b=8.672e-05, col=1, lwd=3) #C
abline(a=(7.418+1.160), b=8.672e-05, col=2, lwd=3) #L
abline(a=(7.418+1.973), b=8.672e-05, col=3, lwd=3) #M
abline(a=(7.418-2.136), b=8.672e-05, col=4, lwd=3) #MV
abline(a=(7.418+3.901e-01), b=8.672e-05, col=5, lwd=3) #PT
abline(a=(7.418+1.373), b=8.672e-05, col=6, lwd=3) #SUV
abline(a=(7.418+7.404e-01), b=8.672e-05, col=8, lwd=3) #TS
legend(20000,35, legend=
         c("C", "L", "M", "MV", "PT", "SUV", "TS"),
       col=c(1,2,3,4,5,6,8), pch=c(16,16,16,16,16,16,16),
       bty = "n", horiz=TRUE)

## accelrate with msrp and carclass - interaction
mod.msrp.class.inter = lm(accelrate ~ msrp + carclass + 
                            msrp:carclass,
                          data=hybrid)
par(mfrow=c(2,2))
plot(mod.msrp.class.inter)
summary(mod.msrp.class.inter)
par(mfrow=c(1,1))
plot(msrp[carclass=="C"], accelrate[carclass=="C"], 
     col=1, pch=16,
     xlab="msrp", ylab="accelrate", 
     main="accelrate vs msrp,carclass",
     ylim=c(0,40), xlim=c(10000,150000))
points(msrp[carclass=="L"], accelrate[carclass=="L"], 
       col=2,pch=16)
points(msrp[carclass=="M"], accelrate[carclass=="M"], 
       col=3,pch=16)
points(msrp[carclass=="MV"], accelrate[carclass=="MV"], 
       col=4,pch=16)
points(msrp[carclass=="PT"], accelrate[carclass=="PT"], 
       col=5,pch=16)
points(msrp[carclass=="SUV"], accelrate[carclass=="SUV"], 
       col=6,pch=16)
points(msrp[carclass=="TS"], accelrate[carclass=="TS"], 
       col=8,pch=16)
abline(a=6.951, b=1.034e-04, col=1, lwd=3) #C
abline(a=(6.951+3.224), b=(1.034e-04 -3.546e-05), 
       col=2, lwd=3) #L
abline(a=(6.951+1.818), b=(1.034e-04 -9.513e-08), 
       col=3, lwd=3) #M
abline(a=(6.951-1.144), b=(1.034e-04 -3.448e-05), 
       col=4, lwd=3) #MV
abline(a=(6.951-4.765e-01), b=(1.034e-04 + 1.988e-05), 
       col=5, lwd=3) #PT
abline(a=(6.951+3.082), b=(1.034e-04 -4.281e-05), 
       col=6, lwd=3) #SUV
abline(a=(6.951+6.068), b=(1.034e-04 -2.665e-04), 
       col=8, lwd=3) #TS
legend(20000,35, legend=
         c("C", "L", "M", "MV", "PT", "SUV", "TS"),
       col=c(1,2,3,4,5,6,8), pch=c(16,16,16,16,16,16,16), 
       bty = "n", horiz = TRUE)

## Final Model msrp + mpg + year:msrp
mod.msrpmpg.class.inter = lm(accelrate ~ msrp + mpg + 
                               year:msrp, data=hybrid)
par(mfrow=c(2,2))
plot(mod.msrpmpg.class.inter)
summary(mod.msrpmpg.class.inter)
par(mfrow=c(1,1))

## Perform a partial F-Test to determine if the interaction term
## makes sense
## Full Final Model
Full.model <- lm(accelrate ~ msrp + mpg + 
                   year:msrp, data=hybrid)
summary(Full.model)
## Remove the interaction term and make a reduced (nested) model
Reduced.model <- lm(accelrate ~ msrp + mpg, data=hybrid)
summary(Reduced.model)

## Partial F-Test
anova(Reduced.model, Full.model)

## Two Way Anova
library(car)
# Make msrp a categorical variable low < 40k high > 50k
hybrid$price <- as.factor(ifelse(hybrid$msrp < 40000, yes="low", no="high"))
                          
                          
summary(hybrid)
plot <- ggline(hybrid, x="price", y="accelrate", color="carclass",
               add=c("mean_se", "dotplot"))
plot
model1 <-lm(accelrate~price*carclass, data=hybrid)
Anova(model1,type=2)

## Check model assumptions
## normal distribution of model residuals
plot(model1, 2)
aov_residuals <- residuals(object=model1)
shapiro.test(aov_residuals)

## homogeneity of variance
plot(model1, 1)
leveneTest(accelrate~price*carclass, data=hybrid)

## ook at all the separate group combinations
posthoc <- lsmeans(model1,
                   pairwise~price*carclass,
                   adjust="tukey")
posthoc
