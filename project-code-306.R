## Stat 306 Final Project

#####################################
# Requirements/Information:

# using data from http://www.gapminder.org/data/ to build regression model 
# Response variable: Human Development Index (http://hdr.undp.org/en/content/human-development-index-hdi)
# Nine explanatory variables are used in full model:

  # -Body Mass Index (BMI), men, Kg/m2
  # -Cholesterol (fat) in blood, men, mmol/L
  # -GDP/capita (US$, inflation-adjusted)
  # -Sugar per person (g per day)
  # -Infant mortality (rate per 1,000 births)
  # -Females aged 15-64 labour force participation rate (%)
  # -Internet users (per 100 people)
  # -Personal computers (per 100 people)
  # -Cell phones (per 100 people)
  # -Military expenditure (% of GDP)

## Models tested: 
## full model and 5-variable model 
## found best model based on cp and adjR^2 -- which same are equal and same as 5-var model
## Do LOOVC on both full and 5-variable model
## Do 5-fold cross validation on both as well

# Graphs we want:
# residuals (model fit)
# qq plot
# some residual plots to show why we transformed the variables
# histogram of HDI
# output for models full and 5
# values for the LOOVC and 5-fold (so will have 4 values) (2x2 table)

########################################################
#Helper Functions
setwd("/")
library(MASS)

#Function to calculate the leave-one-out cross validation error.
ls.cvrmse <- function(ls.out)
  # Compute the leave-one-out cross-validated root mean squared error of prediction.
  # Handles missing values.
  # ls.out is a fitted regression model from lsreg or lm.
  # (c) Copyright William J. Welch 1997
{
  res.cv <- ls.out$residuals / (1.0 - ls.diag(ls.out)$hat)
  # Identify NA's and remove them.
  is.na.res <- is.na(res.cv)
  res.cv <- res.cv[!is.na.res]
  cvrmse <- sqrt(sum(res.cv^2) / length(res.cv))
  return(cvrmse)
}

# Prepare our data
data = read.csv("project_data.csv", header = TRUE, sep = ",", fill = TRUE)

# remove guinea, an outlier !!!(must remove here otherwise won't run properly)!!!
data = data[-50,]

# keep only columns with data (not country names)
data = data[,2:11]

# remove NAs
data = data[complete.cases(data),]

###########################################################
# before transforms, run preliminary regression, look at certain plots.
reg.prelim = lm(HDI~ BMI + Sugar + Chol + Infant.Mortality + Female.LF.Particip + InternetUsage + X..Comps + X..Cphones + Army, data = data)
allSumm.prelim = summary(reg.prelim)
print(allSumm.prelim)
ls.diag(reg.prelim)

pred.prelim = predict(reg.prelim)
res.prelim = resid(reg.prelim)
sigma.prelim = allSumm.prelim$sigma
jpeg('resplot1-prelim.jpg')
par(mfrow=c(2,2))
qqnorm(res.prelim,main="normal Q-Q plot of residuals")
plot(pred.prelim,res.prelim,xlab="predicted value",ylab="residual")
abline(h=2*sigma.prelim); abline(h=-2*sigma.prelim)

plot(data$BMI,res.prelim,xlab="BMI",ylab="residual")
abline(h=2*sigma.prelim); abline(h=-2*sigma.prelim)

plot(data$Chol,res.prelim,xlab="Cholestrol",ylab="residual")
abline(h=2*sigma.prelim); abline(h=-2*sigma.prelim)
dev.off()

jpeg('resplot2-prelim.jpg')
par(mfrow=c(2,2))
plot(data$Sugar,res.prelim,xlab="Sugar",ylab="residual")
abline(h=2*sigma.prelim); abline(h=-2*sigma.prelim)

plot(data$Infant.Mortality,res.prelim,xlab="Infant Mortality",ylab="residual")
abline(h=2*sigma.prelim); abline(h=-2*sigma.prelim)

plot(data$Female.LF.Particip,res.prelim,xlab="Female Labor Participation",ylab="residual")
abline(h=2*sigma.prelim); abline(h=-2*sigma.prelim)

plot(data$X..Comps,res.prelim,xlab="Num Computers",ylab="residual")
abline(h=2*sigma).prelim; abline(h=-2*sigma.prelim)
dev.off()

jpeg('resplot3-prelim.jpg')
par(mfrow=c(2,2))
plot(data$X..Cphones,res.prelim,xlab="Num Celphones",ylab="residual")
abline(h=2*sigma.prelim); abline(h=-2*sigma.prelim)

plot(data$Army,res.prelim,xlab="Army",ylab="residual")
abline(h=2*sigma.prelim); abline(h=-2*sigma.prelim)
dev.off()

#################################################################

# Data transformations
# HDI:
data[,1] = (data[,1]^2)
# BMI
data[,2] = (data[,2])/10
# Sugar
data[,4] = (data[,4])^(1/3)
# Infant.Mortality
data[,5] = log(data[,5])
# Female.LF.Particip 
data[,6] = (data[,6])/10
# InternetUsage
data[,7] = log(data[,7])
# Comps
data[,8] = log(data[,8])
# Cphones
data[,9] = log(data[,9])
# Army
data[,10] = log(data[,10])

# check that response variable is approximately normal
hist(data$HDI)

######################
# Models: 
# Full model summary
reg = lm(HDI~ BMI + Sugar + Chol + Infant.Mortality + Female.LF.Particip + InternetUsage + X..Comps + X..Cphones + Army, data = data)
allSumm = summary(reg)
print(allSumm)
ls.diag(reg)

# Select 5 significant explanatory variables from the full model, re-run regression
# FIVE variable model summary
reg.sig = lm(HDI~ BMI + Chol + Infant.Mortality + Female.LF.Particip + InternetUsage, data = data)
allSumm.sig = summary(reg.sig)
print(allSumm.sig)
ls.diag(reg.sig)

####################
# Look at residuals and plots for FULL model:
pred = predict(reg)
res = resid(reg)
sigma = allSumm$sigma
jpeg('resplot1-full.jpg')
par(mfrow=c(2,2))
qqnorm(res,main="normal Q-Q plot of residuals")
plot(pred,res,xlab="predicted value",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma)

plot(data$BMI,res,xlab="BMI",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma)

plot(data$Chol,res,xlab="Cholestrol",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma)
dev.off()

jpeg('resplot2-full.jpg')
par(mfrow=c(2,2))
plot(data$Sugar,res,xlab="Sugar",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma)

plot(data$Infant.Mortality,res,xlab="Infant Mortality",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma)

plot(data$Female.LF.Particip,res,xlab="Female Labor Participation",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma)

plot(data$X..Comps,res,xlab="Num Computers",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma)
dev.off()

jpeg('resplot3-full.jpg')
par(mfrow=c(2,2))
plot(data$X..Cphones,res,xlab="Num Celphones",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma)

plot(data$Army,res,xlab="Army",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma)
dev.off()

###################
# Look at residuals and plots for 5-variable model:
pred.sig = predict(reg.sig)
res.sig = resid(reg.sig)
sigma.sig = allSumm.sig$sigma
jpeg('resplot1-5var.jpg')
par(mfrow=c(2,2))
qqnorm(res.sig,main="normal Q-Q plot of residuals")
plot(pred.sig,res.sig,xlab="predicted value",ylab="residual")
abline(h=2*sigma.sig); abline(h=-2*sigma.sig)

plot(data$BMI,res.sig,xlab="BMI",ylab="residual")
abline(h=2*sigma.sig); abline(h=-2*sigma.sig)

plot(data$Chol,res.sig,xlab="Cholestrol",ylab="residual")
abline(h=2*sigma.sig); abline(h=-2*sigma.sig)
dev.off()

jpeg('resplot2-5var.jpg')
par(mfrow=c(2,2))
plot(data$Sugar,res.sig,xlab="Sugar",ylab="residual")
abline(h=2*sigma.sig); abline(h=-2*sigma.sig)

plot(data$Infant.Mortality,res.sig,xlab="Infant Mortality",ylab="residual")
abline(h=2*sigma.sig); abline(h=-2*sigma.sig)

plot(data$Female.LF.Particip,res.sig,xlab="Female Labor Participation",ylab="residual")
abline(h=2*sigma.sig); abline(h=-2*sigma.sig)

plot(data$X..Comps,res.sig, xlab="Num Computers",ylab="residual")
abline(h=2*sigma.sig); abline(h=-2*sigma.sig)
dev.off()

jpeg('resplot3-5var.jpg')
par(mfrow=c(2,2))
plot(data$X..Cphones,res.sig,xlab="Num Celphones",ylab="residual")
abline(h=2*sigma.sig); abline(h=-2*sigma.sig)

plot(data$Army,res.sig,xlab="Army",ylab="residual")
abline(h=2*sigma.sig); abline(h=-2*sigma.sig)
dev.off()

# no categorical variables 

# Best subset model
library("leaps")
s1<- regsubsets(HDI~., data=data, method="exhaustive")
ss1 <- summary(s1)
print(ss1)
print(ss1$adjr2)
print(ss1$cp)

# both the max adjr2 and cp indicate an identical 5-variable model is optimal

# Now compare the 5-variable model to the full model

# Full model 
full.model = lm(HDI~ BMI + Sugar + Chol + Infant.Mortality + Female.LF.Particip + InternetUsage + X..Comps + X..Cphones + Army, data = data)
full.model.cvrmse <- ls.cvrmse(full.model)
# Best model by cp/adjr2/significant from full model
best.model = lm(HDI~ BMI + Chol + Infant.Mortality + Female.LF.Particip + InternetUsage, data = data)
# Leave one out cvrmse(best.model)
best.model.cvrmse <- ls.cvrmse(best.model)
# Results
print(c(full.model.cvrmse, best.model.cvrmse))
# output: 0.05052809 0.04890043

n <- nrow(data)
sn <- floor(n/5)
# 17, doesn't matter if you use round, floor, ceiling. 

# 5 fold CVRMSE
set.seed(306)
B <- 500 #Do 500 random splits
errMx <- matrix(NA, B, 2) #matrix to store the results
colnames(errMx) <- c("FullModel", "BestModel")
for (i in 1:B)
{
  testInd <- sample(1:n, sn, replace=FALSE)
  
  tTestDat <- data[testInd, ] #Treat the sampled index as testing set
  tTrainDat <- data[-testInd, ] #The rest is training set.
  
  tFullModel <- lm(HDI~ BMI + Sugar + Chol + Infant.Mortality + Female.LF.Particip + InternetUsage + X..Comps + X..Cphones + Army, data = tTrainDat)
  tFullModel.pred <- predict(tFullModel, tTestDat)
  errMx[i, 1] <- sqrt(sum((tTestDat$HDI - tFullModel.pred)^2)/sn)
  
  tBestModel <- lm(HDI~ BMI + Chol + Infant.Mortality + Female.LF.Particip + InternetUsage, data = tTrainDat)
  tBestModel.pred <- predict(tBestModel, tTestDat)
  errMx[i, 2] <- sqrt(sum((tTestDat$HDI - tBestModel.pred)^2)/sn)
}
apply(errMx, 2, mean)

#output:
# FullModel  BestModel 
# 0.05037194 0.04830219 

####################################
# output for LOOVC and 5-fold:
rbind(apply(errMx, 2, mean), c(full.model.cvrmse, best.model.cvrmse))

#        FullModel  BestModel
#LOOVC  0.05037194 0.04830219 
#5-fold 0.05052809 0.04890043

# --> bestModel (i.e. 5-var) is always better! 

##################################
# other stuff
y_hat = predict(object = reg)
# not available (country), was removed:
# df= cbind(data$Country, data$HDI)
residuals = data$HDI - y_hat

cov_matrix = cov(data)
cor_matrix = cor(data)

# Check partial Correlations
pcor=function(s)
{ i=1; j=2
i1=c(i, j)
i2=1:nrow(s); i2=i2[c(-i, -j)]
s11=s[i1,i1]; s12=s[i1,i2]; s21=s[i2,i1]; s22=s[i2,i2];
condcov = s11 - s12 %*% solve(s22) %*% s21
condcov[1,2]/sqrt(condcov[1,1] * condcov[2,2])
}
# or:
# Generate model for each pairing we want to look at such as
fit.HDI.BMI = lm(HDI~ BMI, data = data)
fit.Chol.BMI = lm(Chol~ BMI, data = data)
cor.res=cor(fit.HDI.BMI$residuals,fit.Chol.BMI$residuals)
print(cor.res)

# OR use pcor Function
str1.order=c('HDI','Chol','BMI')
r.HDIchol.BMI=pcor(corMatrix[str1.order,str1.order])
print(r.HDIchol.BMI)

