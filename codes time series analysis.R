#get SPXL stock data from yahoo#########################################################
install.packages("quantmod")
library(quantmod)
getSymbols("SPXL" , from = "2008-11-01" , to = "2018-11-01" , auto.assign = TRUE)
##close<-SPXL$SPXL.Close
plot(SPXL[, "SPXL.Close"], main = "SPXL", col="orange")
SPXL$logreturns1=diff(log(SPXL$SPXL.Close))
SPXL$logreturns1=round(SPXL$logreturns1,4)
dim(SPXL$logreturns1)
x=SPXL$logreturns1[-1]
x
plot(x, col="orange")
d <- density(x)
plot(d, main="Kernel Density of SPXL-Log returns")
polygon(d, col="orange", border="black")
k<-density(close)
plot(k)
polygon(k, col="orange", border="black")

##GET GOOGLE STOCK DATA FROM YAHOO###########################################################
install.packages("quantmod")
library(quantmod)
getSymbols("GOOG" , from = "2008-11-01" , to = "2018-10-30" , auto.assign = TRUE)
close1<-GOOG$GOOG.Close
plot(GOOG[, "GOOG.Close"], main = "GOOG", col="orange")
lrgoog<-diff(log(GOOG$GOOG.Close))
dim(lrgoog)
y=lrgoog[-1]
plot(y, col="blue")

l<-density(close1)
plot(l)
polygon(l, col="blue", border="green")

d <- density(y)
plot(d, main="Kernel Density of GOOG-Log returns")
polygon(d, col="blue", border="green")
length(y)
length(x)
#######################################ACF- LOG RETURNS#####################################################
acf(x, main="ACF of SPXL log returns")
acf(x^2 , main="ACF of SPXL Squared-log returns")
pacf(x, main="PACF of SPXL Log returns")
qqnorm(x, main="qq-plot of SPXL log returns")
qqline(x, col="red")
hist(x, freq=FALSE, main="Histogram of SPXL log returns" , col="orange")
lines(density(x), lwd=5, col= "black")
title(xlab="logreturns")

acf(y, main="ACF of GOOG log returns")
acf(y^2, main="ACF of GOOG Squared- log returns")
pacf(y, main="PACF of GOOG log returns")
qqnorm(y, main="qq-plot of GOOG log returns")
qqline(y, col="red")
hist(y, freq=FALSE, main="Histogram of GOOG log returns" , col="blue")
lines(density(y), lwd=5, col= "green")
title(xlab="logreturns")
#############descriptive statistics#############################################################
summary(x)
sd(x)
install.packages("psych")
library("psych")
describe(x)
describe(y)
#####################JB-TEST#################################################################
install.packages("tseries")
library("tseries")
jarque.bera.test(x)
jarque.bera.test(y)
##########shapiro test and ks test##
shapiro.test(x)

#########################BOX-Ljung#######################################################

Box.test(x,lag=12,type="Ljung")
Box.test(y,lag=12,type="Ljung")

Box.test(x^2,lag=12,type="Ljung")
Box.test(y^2,lag=12,type="Ljung")

###############################STATIONARITY TESTING######################################
adf.test(x)
pp.test(x)
adf.test(y)
pp.test(y)
###########################FITTING AR(1) + GARCH(1,1)###################################

###################NORMAL FIT###########################################################
library(fGarch)
fit=garchFit(formula=~arma(1,0)+garch(1,1),x,cond.dist="norm" , trace = F)
res=residuals(fit)
res_sd=residuals(fit,standardize=TRUE)
acf(res)
acf(res^2)
acf(res_sd)
acf(res_sd^2)
qqnorm(res_sd)
n=length(res_sd)
m=qt((1:n)/(n+1),df=4)
qqplot(m,sort(res_sd),xlab="t(4)",ylab="residuals")
qqline(m, col="red")
coef(fit)
summary(fit)
predict(fit,n.ahead=10)

##
fit=garchFit(formula=~arma(1,0)+garch(1,1),y,cond.dist="norm" , trace = F)
res=residuals(fit)
res_sd=residuals(fit,standardize=TRUE)
acf(res)
acf(res^2)
acf(res_sd)
acf(res_sd^2)
qqnorm(res_sd)
n=length(res_sd)
m=qt((1:n)/(n+1),df=4)
qqplot(m,sort(res_sd),xlab="t(4)",ylab="residuals")
qqline(m, col="red")
coef(fit)
summary(fit)
predict(fit,n.ahead=10)
#########################STUDENT-T############
library(fGarch)
fit1<-garchFit(~arma(1,0)+garch(1,1), x, cond.dist="std")
res=residuals(fit1)
res_sd=residuals(fit1,standardize=TRUE)
acf(res)
acf(res^2)
acf(res_sd)
acf(res_sd^2)
qqnorm(res_sd)
n=length(res_sd)
m=qt((1:n)/(n+1),df=4)
qqplot(m,sort(res_sd),xlab="t(4)",ylab="residuals")
qqline(m, col="red")
summary(fit1)
coef(fit1)
predict(fit1,n.ahead=10)

fit1=garchFit(formula=~arma(1,0)+garch(1,1),y,cond.dist="std")
res=residuals(fit1)
res_sd=residuals(fit1,standardize=TRUE)
acf(res)
acf(res^2)
acf(res_sd)
acf(res_sd^2)
qqnorm(res_sd)
n=length(res_sd)
m=qt((1:n)/(n+1),df=4)
qqplot(m,sort(res_sd),xlab="t(4)",ylab="residuals")
qqline(m, col="red")
coef(fit1)
summary(fit1)
predict(fit1,n.ahead=10)

##################SKEWED T##########################
library(fGarch)
fit2<-garchFit(~arma(1,0)+garch(1,1), x, cond.dist="sstd")
res=residuals(fit2)
res_sd=residuals(fit2,standardize=TRUE)
acf(res)
acf(res^2)
acf(res_sd)
acf(res_sd^2)

qqnorm(res_sd)
n=length(res_sd)
m=qt((1:n)/(n+1),df=4)
qqplot(m,sort(res_sd),xlab="t(4)",ylab="residuals")
qqline(m, col="red")
coef(fit2)
predict(fit2,n.ahead=10)
summary(fit2)
show(fit2)

## jb
plot(sqrt(252) * fit2@sigma.t, type="l" , col="orange")
##


library(fGarch)
fit3<-garchFit(~arma(1,0)+garch(1,1), y, cond.dist="sstd")
qqplot
res=residuals(fit2)
res_sd=residuals(fit2,standardize=TRUE)
acf(res)
acf(res^2)
acf(res_sd)
acf(res_sd^2)
qqnorm(res_sd)
n=length(res_sd)
m=qt((1:n)/(n+1),df=4)
qqplot(m,sort(res_sd),xlab="t(4)",ylab="residuals")
qqline(m, col="red")
coef(fit3)
predict(fit3,n.ahead=10)
summary(fit3)
plot(sqrt(252) * fit3@sigma.t, type="l" , col="blue")


##############FITTING PARAMETRIC DISTRIBUTION FAMILIES##############################

library(Ecdat)
library(fGarch)
library(sn)
library(MASS)
install.packages("LambertW")
library(LambertW)
library(stats)
loglik_sstd = function(beta) sum(- dsstd(x, mean = beta[1],
                                         sd = beta[2], nu = beta[3], xi = beta[4], log = TRUE))
start = c(mean(x), sd(x), 5, 1)
fit_sstd = optim(start, loglik_sstd, hessian = T,
                 method = "BFGS")
AIC_sstd = 2*fit_sstd$value + 2 * 4
BIC_sstd = 2*fit_sstd$value + log(n) * 4
sd_sstd = sqrt(diag(solve(fit_sstd$hessian)))
fit_sstd$par
sd_sstd
AIC_sstd
BIC_sstd
## goog############
loglik_sstd = function(beta) sum(- dsstd(y, mean = beta[1],
                                         sd = beta[2], nu = beta[3], xi = beta[4], log = TRUE))
start = c(mean(y), sd(y), 5, 1)
fit_sstd = optim(start, loglik_sstd, hessian = T,
                 method = "BFGS")
AIC_sstd = 2*fit_sstd$value + 2 * 4
BIC_sstd = 2*fit_sstd$value + log(n) * 4
sd_sstd = sqrt(diag(solve(fit_sstd$hessian)))
fit_sstd$par
sd_sstd
AIC_sstd
BIC_sstd

############COPULA################################
library(copula)
install.packages("sn")
library(sn)
library(ks)
n=nrow(x);
x
fit1=st.mple(matrix(1,n,1),y=x,dp=c(mean(x),sd(x),0,10));#fit skewed t to first variable
est1=fit1$dp;
u1=pst(x,dp=est1); #transform to a variable with uniform distribution
x2<-y;
fit2=st.mple(matrix(1,n,1),y=x2,dp=c(mean(x2),sd(x2),0,10)); #fit skewed t to the second variable
est2=fit2$dp;
u2=pst(x2,dp=est2);#transform to a variable with uniform distribution
U.hat=cbind(u1,u2)
install.packages("ks")
library(ks)
fhatU=kde(x=U.hat,H=Hscv(x=U.hat));#nonparametric density estimation 
plot(fhatU,cont=seq(10,80,10) ,col="red"); #contour plots
fhatU

tau=as.numeric(cor.test(u1,u2,method="kendall")$estimate);
tau
tau(tCopula(param = 0.712))
omega=sin(tau*pi/2);#estimator for rho
omega
library(copula)
Ct=fitCopula(copula=tCopula(dim=2),data=U.hat,method="ml");#fit t copula
Ct@estimate;
loglikCopula(param=Ct@estimate,u=U.hat,copula=tCopula(dim=2));#compute loglikelihood function
-2*.Last.value+2*length(Ct@estimate);#compute AIC
Cgauss=fitCopula(copula=normalCopula(dim=2),data=U.hat,method="ml",start=c(omega));#fit Gaussian copula
Cgauss@estimate;
loglikCopula(param=Cgauss@estimate,u=U.hat,copula=normalCopula(dim=2));
-2*.Last.value+2*length(Cgauss@estimate);#compute AIC
Cfr=fitCopula(copula=frankCopula(1,dim=2),data=U.hat,method="ml");#fit frank copula
Cfr@estimate;
loglikCopula(param=Cfr@estimate,u=U.hat,copula=frankCopula(dim=2));
-2*.Last.value+2*length(Cfr@estimate);#compute AIC

Cjoe<-fitCopula(copula = joeCopula(2,dim = 2),data = U.hat)
Cjoe@estimate
loglikCopula(param=Cjoe@estimate,u=U.hat,copula=joeCopula(dim = 2))


-2*.Last.value+2*length(Cjoe@estimate)

Cgumb<-fitCopula(copula = gumbelCopula(2,dim = 2),data = U.hat)
Cgumb@estimate
loglikCopula(param=Cgumb@estimate,u=U.hat,copula = gumbelCopula(dim = 2))
-2*.Last.value+2*length(Cfr@estimate)
################################################################################################
hist(u1, main="(a)", xlab=expression(hat(U)[1]), freq = FALSE)
hist(u2, main="(b)", xlab=expression(hat(U)[2]), freq = FALSE)
plot(u1, u2, main="(c)", xlab = expression(hat(U)[1]),ylab = expression(hat(U)[2]), mgp = c(2.5, 1, 0), col="green")
plot(fhatU, drawpoints=FALSE, drawlabels=FALSE,cont=seq(10, 80, 10), main="(d)", xlab=expression(hat(U)[1]),ylab=expression(hat(U)[2]), mgp = c(2.5, 1, 0))

install.packages("Hmisc")
library(Hmisc)
cor.test(u1, u2, method="spearman")
cor.test(u1, u2, method="kendall")
sin(-0.242*pi/2)
cor.test(u1, u2, method="pearson")
u1
###########################################RISK CALCULATIONS##########################
install.packages("vars")
library(vars)
library(copula)
sample(U.hat, size = 1001, replace = FALSE, prob = NULL)
install.packages("rgl")
library(rgl)
plot3d(u1, u2 ,pch=20,col='navyblue')

pairs(U.hat, col="red")
cor(U.hat,meth='spearman')
rho <- coef(Ct)[1]
df <- coef(Ct)[2]
persp(tCopula(dim=2,rho,df=df),dCopula ,col = "purple")##density

# Sample random observation from the copula
library(copula)
library(mvtnorm)
k <- rCopula(1000,tCopula(dim=2,rho,df=df))
U <- cCopula(U.hat,tCopula,inverse = TRUE)
k
plot(k[,1],k[,2],pch='.',col='blue')
cor(k,method='spearman')

plot(U[,2], ylab = quote(U[2]))






#####
x_mu <- mean(x)
x_sd <- sd(x)
y_mu <- mean(y)
y_sd <- sd(y)

copula_dist <- mvdc(copula=tCopula(rho,dim=2,df=df), margins=c("norm","norm"),
                    paramMargins=list(list(mean=x_mu, sd=x_sd),
                                      list(mean=y_mu, sd=y_sd)))
sim <- rMvdc(1000, copula_dist)
sim

############
library(rugarch)

k <- rCopula(1000,tCopula(dim=2,rho,df=df))
alpha <-c(0.99)
w <- cbind (1,1) #Weights of each risk
w
sim.val.ln1 <- qnorm (k [, 1], mean = x_mu, sd = x_sd)
sim.val.ln1
sim.val.ln2 <- qnorm (k [, 2], mean = y_mu, sd = y_sd)
MC.data <-cbind (sim.val.ln1, sim.val.ln2)
MC.data
MC.Lsim <-  -(MC.data%*%t(w))
Q<-quantile (MC.Lsim, alpha)
Q
mean (MC.Lsim [MC.Lsim> quantile (MC.Lsim, alpha [1])])
sd(MC.Lsim [MC.Lsim> quantile (MC.Lsim, alpha [1])])
install.packages("QRM")
library(QRM)
qnorm(.99, mean = 0.08147084, sd = 0.01180397)
#ESnorm(.99, mean = 0.081, sd = 0.0118)
install.packages("performanceAnalytics")
library(PerformanceAnalytics)
library(cvar)
install.packages("cvar")
#cvar::ES(qnorm, x = 0.01, dist.type = "qf")

res1 <- cvar::ES(qnorm, x = 0.01, mean = x_mu, sd = x_sd)
res2 <- cvar::ES(qnorm, x = 0.01, intercept = y_mu, slope = y_sd)
abs((res2 - res1))
###############BOOTSTRAPPING#########
library(MASS); 
summary(MC.Lsim)
(q99.tcopula <- qnorm(0.99,mean=mean(x),sd=sd(x)))

#Simulate from fitted Gaussian; bundle up estimating 99th percentile into a function
rx <- function() {  rnorm(n=nrow(MC.Lsim),mean=mean(x),sd=sd(x))}
est.q99.tcopula <- function(x) {return(qnorm(0.99,mean=x_mu,sd=x_sd))}

#Simulate, plot the sampling distribution from the simulations
sampling.dist.tcopula <- replicate(1000, est.q99.tcopula(rx()))
plot(hist(sampling.dist.tcopula,breaks=10),freq=FALSE)
plot(density(sampling.dist.tcopula))

#Find standard error and a crude confidence interval
sd(sampling.dist.tcopula)
quantile(sampling.dist.tcopula,c(0.99))

#Find the basic CI
2*q99.tcopula - quantile(sampling.dist.tcopula,c(0.99))

#Compare histogram to fitted density and to a smooth density estimate
plot(hist(MC.Lsim),freq=FALSE)
curve(dnorm(x,mean=mean(x),sd=sd(y)),add=TRUE,col="purple")
lines(density(x),lty=2)

#non-parametric B##

#Resampling, re-estimating, and finding sampling distribution,standard error, bias, CIs
(q99.np <- quantile(MC.Lsim,0.99))
resample <- function(x) {
  sample(x,size=length(x),replace=TRUE)
}
est.q99.np <- function(x) {
  quantile(x,0.99)
}
sampling.dist.np <- replicate(1000, est.q99.np(resample(MC.Lsim)))
plot(density(sampling.dist.np))
abline(v=q99.np,lty=2)
sd(sampling.dist.np)
mean(sampling.dist.np - q99.np)
quantile(sampling.dist.np,c(0.99))
2*q99.np - quantile(sampling.dist.np,c(0.99))

