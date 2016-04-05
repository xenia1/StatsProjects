## Stat 306 Final Project

#####################################
# Requirements/Information:
## full model and 5-variable model 
## best model based on cp and adjR^2 -- which same are equal and same as 5-var model
## LOOVC on both full and 5-variable model
## 5-fold cross validation on both 

# Graphs we want:
# residuals (model fit)
# qq plot(maybe?)
# some residual plots to show why we transformed the variables
# histogram of HDI
# output for models full and 5
# values for the LOOVC and 5-fold (so will have 4 values) (2x2 table)

########################################################
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

pcor=function(s)
{ i=1; j=2
i1=c(i, j)
i2=1:nrow(s); i2=i2[c(-i, -j)]
s11=s[i1,i1]; s12=s[i1,i2]; s21=s[i2,i1]; s22=s[i2,i2];
condcov = s11 - s12 %*% solve(s22) %*% s21
condcov[1,2]/sqrt(condcov[1,1] * condcov[2,2])
}

# Prepare our data
data = read.csv("project_data.csv", header = TRUE, sep = ",", fill = TRUE)

# remove guinea, an outlier
data = data[-50,]
data = data[,2:11]
# remove NAs
data = data[complete.cases(data),]

# take log of infant mort, cellphone, comp, army, internet
# HDI     # BMI   # Chol    # Sugar # Infant.Mortality 
# Female.LF.Particip # InternetUsage # Comps # Cphones # Army

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


# not that normal
hist(data$HDI)
hist((data$HDI^(1.9)))
hist((data$HDI^(7/3)))
hist((data$HDI^(3/2)))

# Full model summary
reg = lm(HDI~ BMI + Sugar + Chol + Infant.Mortality + Female.LF.Particip + InternetUsage + X..Comps + X..Cphones + Army, data = data)
allSumm = summary(reg)
print(allSumm)
ls.diag(reg)

# FIVE variable model summary
reg.sig = lm(HDI~ BMI + Chol + Infant.Mortality + Female.LF.Particip + InternetUsage, data = data)
allSumm.sig = summary(reg.sig)
print(allSumm.sig)
ls.diag(reg.sig)

# Look for patterns in residuals, Do we need to transform any of our data?
pred = predict(reg)
res = resid(reg)
sigma = allSumm$sigma
jpeg('resplot1.jpg')
par(mfrow=c(2,2))
qqnorm(res,main="normal Q-Q plot of residuals")
plot(pred,res,xlab="predicted value",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma)

plot(data$BMI,res,xlab="BMI",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma)

plot(data$Chol,res,xlab="Cholestrol",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma)
dev.off()

jpeg('resplot2.jpg')
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

jpeg('resplot3.jpg')
par(mfrow=c(2,2))
plot(data$X..Cphones,res,xlab="Num Celphones",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma)

plot(data$Army,res,xlab="Army",ylab="residual")
abline(h=2*sigma); abline(h=-2*sigma)
dev.off()

# no categorical variables 

# Correlation
corMatrix = cor(data)
print(corMatrix)

# Check partial Correlations
# Generate model for each pairing we want to look at such as
fit.HDI.BMI = lm(HDI~ BMI, data = data)
fit.Chol.BMI = lm(Chol~ BMI, data = data)
cor.res=cor(fit.HDI.BMI$residuals,fit.Chol.BMI$residuals)
print(cor.res)

# OR use pcor Function
str1.order=c('HDI','Chol','BMI')
r.HDIchol.BMI=pcor(corMatrix[str1.order,str1.order])
print(r.HDIchol.BMI)  # -0.1503615 partial cor of y and mfee given ffarea

# Multicollinearity

# Best subset model
library("leaps")
s1<- regsubsets(HDI~., data=data, method="exhaustive")
ss1 <- summary(s1)
print(ss1)
print(ss1$adjr2)
print(ss1$cp)

# Best model by adjr2 --> need to change this one!! 
best.adjr2 = lm(HDI~ BMI + Chol + Sugar + Infant.Mortality + Female.LF.Particip + InternetUsage + X..Cphones, data = data)
# Leave one out cvrmse adjr2
best.adjr2.cvrmse <- ls.cvrmse(best.adjr2)
# Best model by cp
best.cp = lm(HDI~ BMI + Chol + Infant.Mortality + Female.LF.Particip + InternetUsage, data = data)
# Leave one out cvrmse(best.cp)
best.cp.cvrmse <- ls.cvrmse(best.cp)
# Results
print(c(best.adjr2.cvrmse, best.cp.cvrmse))

n <- nrow(data)
sn <- floor(n/5)
# 17, doesn't matter if you use round, floor, ceiling. 

# 5 fold CVRMSE
set.seed(306)
B <- 500 #Do 500 random splits
errMx <- matrix(NA, B, 2) #matrix to store the results
colnames(errMx) <- c("ADJR2Model", "CPModel")
for (i in 1:B)
{
  testInd <- sample(1:n, sn, replace=FALSE)
  
  tTestDat <- data[testInd, ] #Treat the sampled index as testing set
  tTrainDat <- data[-testInd, ] #The rest is training set.
  
  tAdjR2Model <- lm(HDI~ BMI + Chol + Sugar + Infant.Mortality + Female.LF.Particip + InternetUsage + X..Cphones, data = tTrainDat)
  tAdjR2Model.pred <- predict(tAdjR2Model, tTestDat)
  errMx[i, 1] <- sqrt(sum((tTestDat$HDI - tAdjR2Model.pred)^2)/sn)
  
  
  tCPModel <- lm(HDI~ BMI + Chol + Infant.Mortality + Female.LF.Particip + InternetUsage, data = tTrainDat)
  tCPModel.pred <- predict(tCPModel, tTestDat)
  errMx[i, 2] <- sqrt(sum((tTestDat$HDI - tCPModel.pred)^2)/sn)
}
apply(errMx, 2, mean)

y_hat = predict(object = reg)
df= cbind(data$Country, data$HDI)
residuals = data$HDI - y_hat

cov_matrix = cov(data)
cor_matrix = cor(data)
