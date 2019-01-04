################################################################################
# LASSO Example
################################################################################
# 97 men who were about to undergo radical prostatectomy.
# goal: measure association between cancer volume and 8 other clinical measures.
#
# Dataset
# lcavol: log(cancer volumne)
# lweight: log(prostate weight volumne)
# age: age of person
# lbph: log(benign prostatic hyperplasia)
# svi: seminal vesicle invasion
# lcp: log(capsular penetration)
# gleason: Gleason score
# pgg45: $ Gleason score 4 or 5
# lpsa: target variable y, log(prostate specific antigen)
################################################################################
#Read the data
dat <- read.csv('C:\\Users\\Matthew A. Lanham\\Dropbox\\_Purdue\\_Teaching\\PA474\\4_OLS_Regularization_Lasso_Ridge\\Prostate.csv')

#Split data into a training and validation set
train <- dat[dat$train==TRUE,1:9]
test <- dat[dat$train==FALSE,1:9]

################################################################################
# Standardize your input features (here we performed a z-score standardization
################################################################################
## Option 1 (long way)
#center and scale all predictors - necessary for shrinkage methods
trainS <- data.frame(
    lcavol  =(train$lcavol  -mean(train$lcavol ))/sd(train$lcavol ),
    lweight =(train$lweight -mean(train$lweight))/sd(train$lweight),
    age     =(train$age     -mean(train$age    ))/sd(train$age    ),
    lbph    =(train$lbph    -mean(train$lbph   ))/sd(train$lbph   ),
    svi     =(train$svi     -mean(train$svi    ))/sd(train$svi    ),
    lcp     =(train$lcp     -mean(train$lcp    ))/sd(train$lcp    ),
    gleason =(train$gleason -mean(train$gleason))/sd(train$gleason),
    pgg45   =(train$pgg45   -mean(train$pgg45  ))/sd(train$pgg45  )
)
lpsa <- train$lpsa

## Option 2 (more easily using caret)
# You can try this and you'll notice you get the same values
library(caret)
preProcValues <- preProcess(train[,1:8], method = c("center","scale"))
d <- predict(preProcValues, train)

################################################################################
# Doing some explortatory data analysis (EDA)
################################################################################
#By default, refer to objects in trainS
attach(trainS)

#examine the correlation among the values in the data 
pairs(trainS)
cor(trainS)

################################################################################
# Show OLS "best fit" line and other poor lines
################################################################################
#Some basic regressions
#optimal fit using OLS
plot(lcavol,lpsa,pch=16,main='Describe the association')
lcavolfit<-summary(lm(lpsa~lcavol))

#other lines that are not optimal
plot(lcavol,lpsa,pch=16,main='Data plus estimated regression line')
abline(lcavolfit$coeff[1,1], lcavolfit$coeff[2,1], col='red',lwd=3)

plot(lcavol,lpsa,pch=16,main='Data plus estimated regression line?')
abline(lcavolfit$coeff[1,1],lcavolfit$coeff[2,1],col='red',lwd=3)
abline(a=-2,b=3,col='darkgreen',lwd=3)

plot(lcavol,lpsa,pch=16,main='Data plus estimated regression line?')
abline(lcavolfit$coeff[1,1],lcavolfit$coeff[2,1],col='red',lwd=3)
abline(a=1.2,b=1,col='darkblue',lwd=3)

################################################################################
# Back to doing some explortatory data analysis (EDA)
################################################################################
#Bivariate relationship between lpsa (the outcome) and each predictor
par(mfrow=c(2,4))
plot(age,lpsa,pch=16,cex.lab=1.5)
agefit<-summary(lm(lpsa~age))
abline(agefit$coeff[1,1],agefit$coeff[2,1],col='red',lwd=3)

plot(gleason,lpsa,pch=16,cex.lab=1.5)
gleasonfit<-summary(lm(lpsa~gleason))
abline(gleasonfit$coeff[1,1],gleasonfit$coeff[2,1],col='red',lwd=3)

plot(lbph,lpsa,pch=16,cex.lab=1.5)
lbphfit<-summary(lm(lpsa~lbph))
abline(lbphfit$coeff[1,1],lbphfit$coeff[2,1],col='red',lwd=3)

plot(lcavol,lpsa,pch=16,cex.lab=1.5)
lcavolfit<-summary(lm(lpsa~lcavol))
abline(lcavolfit$coeff[1,1],lcavolfit$coeff[2,1],col='red',lwd=3)

plot(lweight,lpsa,pch=16,cex.lab=1.5)
lweightfit<-summary(lm(lpsa~lweight))
abline(lweightfit$coeff[1,1],lweightfit$coeff[2,1],col='red',lwd=3)

plot(svi,lpsa,pch=16,cex.lab=1.5)
svifit<-summary(lm(lpsa~svi))
abline(svifit$coeff[1,1],svifit$coeff[2,1],col='red',lwd=3)

plot(lcp,lpsa,pch=16,cex.lab=1.5)
lcpfit<-summary(lm(lpsa~lcp))
abline(lcpfit$coeff[1,1],lcpfit$coeff[2,1],col='red',lwd=3)

plot(pgg45,lpsa,pch=16,cex.lab=1.5)
pgg45fit<-summary(lm(lpsa~pgg45))
abline(pgg45fit$coeff[1,1],pgg45fit$coeff[2,1],col='red',lwd=3)

#Examine the ordinary least squares fit of the full model using multiple 
#regression
OLS <- summary(lm(lpsa~.,data=trainS))
OLS

################################################################################
# implement lasso using lars
################################################################################
#Load the lars package, which can fit the lasso
library(lars)
?lars  #find out more about what lars can do?
#define a lasso-object
lasso <-lars(x=as.matrix(trainS)
            ,y=lpsa
            ,type='lasso'
            ,trace=FALSE
            ,normalize=TRUE
            ,intercept=TRUE)
#plot the lasso path
par(mfrow=c(1,1));plot(lasso)
#see steps
lasso

#look at lasso coefficients at each step
#the last step are the coefficients you got from OLS
coef(lasso) 

#obtain estimated parameter coefficients
predict.lars(object=lasso
             ,s=.375
             ,mode='fraction'
             ,type='coefficients')$coefficients
#obtain predictions using the lasso model with given s
predict.lars(object=lasso
             ,newx=trainS
             ,s=.375
             ,mode='fraction'
             ,type='fit')$fit

################################################################################
# Building the lasso from the gound up - just for fun (not required)
################################################################################
# take the sum of the absolute value of the OLS parameter estimates
absum <- sum(abs(OLS$coeff[-1,1]))
absum
#Build the lasso plot from the ground up 
t <- apply(abs(coef(lasso)),1,sum) #Sum of absolute value of OLS coefficients
s <- t/absum
s
# plot the coefficient by shrinkage factor
plot(s
     , coef(lasso)[,1], ylim=c(-.3,0.7), type='l'
     , lwd=2, xlab='Shrinkage factor s'
     , main='Lasso path - coefficients  as a function of shrinkage factor s'
     , xlim=c(0,1.2)
     , axes=FALSE, ylab='Coefficient', cex.lab=1.5, cex.axis=1.4)
axis(1,at=seq(0,1,.2),cex.axis=1.4)
axis(2,at=seq(-.3,.7,.2),cex.axis=1.4)
lines(s,coef(lasso)[,2],lwd=2)
lines(s,coef(lasso)[,3],lwd=2)
lines(s,coef(lasso)[,4],lwd=2)
lines(s,coef(lasso)[,5],lwd=2)
lines(s,coef(lasso)[,6],lwd=2)
lines(s,coef(lasso)[,7],lwd=2)
lines(s,coef(lasso)[,8],lwd=2)
text(1.05,0.72,'lcavol')
text(1.03,0.34,'svi')
text(1.05,0.30,'lweight')
text(1.05,0.26,'ppg45')
text(1.04,0.20,'lbph')
text(1.06,-.02,'gleason')
text(1.03,-.15,'age')
text(1.03,-.29,'lcp')
# add vertical lines for shrinkage factors tried
abline(v=s, col='lightgray', lty=3)

################################################################################
# Cross-validation and choosing the "best" s
################################################################################
# 10-fold Cross-validation to choose a value of s
set.seed(389173367) #sets 'starting point' for list of random numbers

#Genreate a vector of holdout labels
(cvlab <- sample(1:10, 67, replace=TRUE))

#How many of each label are there?
table(cvlab)

#Create a vector of candidate s values
#Try each s value on all cross validated sets
svec <- seq(0, 1, .05)
(J <- length(svec))

#Initialize a list to store lasso objects from k fold cross validation
lassolist <- list()

#Initialize a list to store predictions from each lasso set
predtrain <- list()

#Initialize a matrix to store MSE
#Rows correspond to the J values of s, columns correspond to the ten holdout sets
(MSEstore <- matrix(NA, J, 10))

#Use a for loop to get each lasso fit holding out the ith set in 10-fold cv
#Then predict the ith set using the holdout test set
for(i in 1:10){
    #i=1
    lassolist[[i]] <- lars(x = as.matrix(trainS)[cvlab!=i,]
                           , y = lpsa[cvlab!=i]
                           , type = 'lasso'
                           , trace = FALSE
                           , normalize = TRUE
                           , intercept = TRUE)
    predtrain[[i]] <- fit <- predict.lars(object = lassolist[[i]]
                                          , newx = trainS[cvlab==i,]
                                          , s = svec
                                          , mode = 'fraction'
                                          , type = 'fit')$fit
    #Start a new loop to get MSE for each combination of ith holdout set 
    #and jth value of s
    for(j in 1:J){
        #j=1
        #This computes MSE 
        MSEstore[j,i] <- mean((predtrain[[i]][,j] - lpsa[cvlab==i])^2)
    }
}

# These apply statements compute mean and standard error of the observed MSEs 
# at J values of s across the 10 holdout sets
meanMSE <- apply(MSEstore,1,mean)
stdMSE <- apply(MSEstore,1,sd)/sqrt(10)

plot(svec, meanMSE
     ,ylim=c(0.5,1.75), pch=16, axes=FALSE, cex=1.2, cex.lab=1.7, col=colors()[258]
     ,xlab='Shrinkage factor s', ylab='Mean square error'
     ,main='Average CV prediction error as a function of s')
axis(1,cex.axis=1.4,cex.axis=1.2)
axis(2,las=1,at=seq(0.5,1.75,.25),cex.axis=1.2)
lines(svec,meanMSE,lty=1,col=colors()[258])

# this will add our standard errors to the plot
for(i in 1:J)segments(svec[i],(meanMSE[i]-stdMSE[i]),svec[i],(meanMSE[i]+stdMSE[i]))
abline(h=(meanMSE+stdMSE)[18],lty=2)
points(svec[9],meanMSE[9],col='red',pch=15,cex=1.3)
legend(.35,1.5,legend=c('mean MSE','standard error (SE)','1 SE above lowest mean','chosen value of s'),
       pch=c(16,NA,NA,15),col=c(colors()[258],1,1,'red'),cex=1.1,lty=c(1,1,2,NA))

#These are the coefficients for the chosen model
predict.lars(lasso
             , s = 0.4
             , mode = 'fraction'
             , type = 'coefficients')$coefficients

################################################################################
#How well does the training model work on training and validation sets?
################################################################################

#center and scale all predictors - necessary for shrinkage methods
testS <- data.frame(
    lcavol  =(test$lcavol  -mean(test$lcavol ))/sd(test$lcavol ),
    lweight =(test$lweight -mean(test$lweight))/sd(test$lweight),
    age     =(test$age     -mean(test$age    ))/sd(test$age    ),
    lbph    =(test$lbph    -mean(test$lbph   ))/sd(test$lbph   ),
    svi     =(test$svi     -mean(test$svi    ))/sd(test$svi    ),
    lcp     =(test$lcp     -mean(test$lcp    ))/sd(test$lcp    ),
    gleason =(test$gleason -mean(test$gleason))/sd(test$gleason),
    pgg45   =(test$pgg45   -mean(test$pgg45  ))/sd(test$pgg45  )
)

# compute MSE on test set
yhat_v <- predict.lars(lasso
                       , newx = testS
                       , s = 0.4
                       , mode = 'fraction'
                       , type = 'fit')$fit
lpsa_v <- test$lpsa
mean((yhat_v - lpsa_v)^2)
#[1] 0.4471618

# the average MSE when train set at s=0.40. Specified 9 because .4 corresponds
# to the 9th element
meanMSE[[9]]
#[1] 0.6442007

################################################################################
# trying to use caret
################################################################################
library(caret)
ctrl <- trainControl(method="cv", number=10,
                     classProbs = FALSE,               # b/c a regression problem
                     summaryFunction = defaultSummary  # b/c a regression problem
                     #, search="grid"
)

# train a LASSO
lassofit <- train(lpsa ~ .,
                  data = train,
                  method = "lars",
                  trControl = ctrl,
                  preProcess=c("center","scale"), # this ensures the inputs are standardized
                  tuneLength = 15,                # this specifies various s values
                  metric = "RMSE")
# the last line of the model shows you the s that is 'optimal' to use and the
# corresponding performance for that s tuning parameter based on the metric
# you specified that you wanted to maximize/minimize (in this case RMSE)
lassofit
# fraction = 0.8733333
plot(lassofit$finalModel)

# optimal s
lassofit$bestTune[[1]]

# various coefficients for different s
# last row are the coefficients from OLS
coef(lassofit$finalModel, lassofit$bestTune$.lambda)

# best lasso coefficients using s found from caret
lassoCoefs  <- predict.lars(lasso
                       , newdata = trainS
                       , s = lassofit$bestTune[[1]]   # best s from caret
                       , mode = 'fraction'
                       , type="coefficients")$coefficients
lassoCoefs 

# predictions on scaled training set - caret knows to transform based on how it 
# was done on the training set
testYhatLasso  <- predict(lassofit, newdata = test)

# forecasts on testset
TestErrorLasso <- postResample(pred=testYhatLasso, obs=test$lpsa)
TestErrorLasso[[1]]^2
#[1] 0.4936298

# the fraction vs the RMSE
plot(x=lassofit$results$fraction, y=lassofit$results$RMSE
     , col="blue", pch=19
     , main="RMSE vs s from caret runs", xlab="S", ylab="RMSE")

################################################################################
# Remember those indirect test error estimation approaches I discussed in class
# such as AIC, BIC, Cp, etc. We can easily identify which model is "best" based
# on those performance measures if we like
################################################################################
#What model is chosen by AIC BIC?
library(bestglm)
?bestglm #info about this package
Xy <- data.frame(
    lcavol  =trainS$lcavol  ,
    lweight =trainS$lweight ,
    age     =trainS$age     ,
    lbph    =trainS$lbph    ,
    svi     =trainS$svi     ,
    lcp     =trainS$lcp     ,
    gleason =trainS$gleason ,
    pgg45   =trainS$pgg45 ,
    y       =lpsa)

# Best model via BIC suggests just using lcavol and lweight
bestglm(Xy, family=gaussian, IC='BIC', method='exhaustive')
#            Estimate Std. Error   t value     Pr(>|t|)
#(Intercept) 2.4523451 0.09300619 26.367548 4.376848e-36
#lcavol      0.7798589 0.09824033  7.938277 4.141615e-11
#lweight     0.3519101 0.09824033  3.582135 6.576173e-04

# Best model via AIC suggests having lcavol, lweight, age, lbph, svi, lcp, pgg45
bestglm(Xy, family=gaussian, IC='AIC', method='exhaustive')
#               Estimate Std. Error   t value     Pr(>|t|)
#(Intercept)  2.4523451 0.08629499 28.418163 4.018539e-36
#lcavol       0.7131604 0.13055744  5.462426 9.883880e-07
#lweight      0.2951154 0.10416578  2.833132 6.298761e-03
#age         -0.1461421 0.09831356 -1.486490 1.424742e-01
#lbph         0.2113905 0.10218044  2.068796 4.295574e-02
#svi          0.3115400 0.12366603  2.519204 1.449125e-02
#lcp         -0.2877348 0.15327439 -1.877253 6.543004e-02
#pgg45        0.2621042 0.12012036  2.182013 3.310324e-02