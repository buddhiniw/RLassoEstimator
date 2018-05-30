#############################
#  This code uses Gaussian linear regression with Lasso regularization to predict
#  real estate selling prices utilizing the glment package in R. 
# 
#  Reference analysis - "Fitting the Lasso Estimator using R" by Finch, W. Holmes; Finch, Maria E. Hernandez
#  Practical Assessment, Research & Evaluation, 
#  v21 n7 May 2016
#  
#  R script author - B. Waidyawansa (04/30/2018)
#############################


# Load required packages
library(caret)
library(glmnet) # Package to fit ridge/lasso/elastic net models
library(selectiveInference)
library(DMwR)

# Clean up the directory
rm(list=ls())


# Set the working directory
home =Sys.getenv("HOME")
wd = paste0(home,"/MyWork/Upwork/R_bedrock_gui/RLassoEstimator")
setwd(wd)

# Get file name from command line arguments if run as script from command line or PHP
args <- commandArgs(TRUE)
print(args)

# Remove comment from the start of next line to run in RStudio
# Change file name as neccessary
args[1]="ason1.csv"
#args[1]="Dataset7Obs.csv"

# Get the input data file name
inFileName=args[1]


# Read the data from a .dat file, print the data to be sure that itwas read in correctly
dataIn <- read.csv(inFileName, header=T)

# Standardize the predictor(x) variables and response(y) variable
#
# Standarizing/scaling the predictor variables prior to fitting the model will ensure
# the lasso penalization will treat different scale explanatory variables on a more equal footing
# But only continuos predictors needs to be standardized.
# DONOT scale factor variables (using 1/0) used for yes/no situations.
#
# We cannot use the preProcess option in the caret package to do the scaling because the dataset
# contains factor variables.
#
#
# Get x variables (including the data-to-be-predicted row)
dataIn.x <- dataIn[,-1]


# Identify columns with categorical data (0/1)
cat.vars <- apply(dataIn.x,2,function(x) { all(x %in% 0:1) })
dataIn.continuous <- dataIn.x[!cat.vars]
dataIn.catrgorical <- dataIn.x[cat.vars]

# Now scale the continuous variables 
dataIn.continuous.scaled <-scale(dataIn.continuous,center=TRUE, scale=TRUE)

# Create the scaled x matrix
x.scaled <- cbind(as.data.frame(dataIn.continuous.scaled),dataIn.catrgorical)

# Scale the responce variable
dataIn.y <- dataIn[1:(nrow(dataIn)-1),1,drop=FALSE]
dataIn.y.scaled <- scale(dataIn.y,center=TRUE, scale=TRUE)
y.scaled <- as.data.frame(dataIn.y.scaled)

# Create the X and Y regression matrices
x <- as.matrix(x.scaled[1:(nrow(x.scaled)-1),])
y <- as.matrix(y.scaled)


# Get the to-be-predicted data set assuming they are the last row of the original data set
dataIn.scaled.predict <- x.scaled[nrow(x.scaled),]
x.predict <-as.matrix(dataIn.scaled.predict)


# cross-validate using n folds where n = number of entries

# create the original dataset with scaled data
dataIn.scaled <- cbind(y,x)
# remove NA entries
dataIn.scaled <- na.omit(dataIn.scaled)

# Use caret package to train the glmnet model to get the best parameter values for alpha and lambda
set.seed(42)
cv_n = trainControl(method = "LOOCV", number = 5)

fit_elnet = train(
    SellingPrice ~ ., data = dataIn.scaled,
    method = "glmnet",
    trControl = cv_n,
    tuneLength = 10
)

fit_elnet

#Caret.Fit.BestResult(fit_elnet)

# Fit elastic net with 0< alpha <1
# First do a grid search to find the best alpha
a <- seq(0.1, 0.9, 0.05)
search <- foreach(i = a, .combine = rbind) %dopar% {
    cv <- cv.glmnet(x, y, family = "gaussian", nfold = length(y), type.measure = "mse", paralle = TRUE, alpha = i)
    data.frame(cvm = cv$cvm[cv$lambda == cv$lambda.min], lambda.min = cv$lambda.min, alpha = i)
}
cv3 <- search[search$cvm == min(search$cvm), ]
# Fit LASSO using glmnet(y=, x=). Use alpha=1 for lasso only.
# Set standardize = FALSE because the predictor variables were standardized in the begining of the 
# analysis using "scale" function. Intercept =FALSE because responce variables(y) is also scaled


fit.glmnet.lasso <- glmnet(x,y,family="gaussian",lambda=cv3$lambda.min, alpha = cv3$alpha,standardize=FALSE,intercept = FALSE)
plot(fit.glmnet.lasso, xvar="lambda")
#w<-(abs(coef(fit.glmnet.lasso.t)[-1])+1/nrow(x.scaled))^(-1)
#fit.glmnet.lasso=glmnet(x,y,penalty.factor = w)
#plot(fit.glmnet.lasso, xvar="lambda")


# Perform nfolds=sample size cross-validation  (leave-one-out-cross-validation)
# runs glmnet nfolds+1 times; the first to get the lambda sequence, and then the
# remainder to compute the fit with each of the folds omitted. 
# The error is accumulated, and the average error and standard deviation over the folds is computed
fit.glmnet.lasso.cv <- cv.glmnet(x,y,alpha=1,nfolds = length(y),type.measure="mse") 
#grid=10^seq(10,-2,length=100) ##get lambda sequence
#fit.glmnet.lasso.cv <- cv.glmnet(x,y,alpha=1,nfolds = 10,type.measure="mse",lambda=grid) 

plot(fit.glmnet.lasso.cv)
lasso.glmnet <- fit.glmnet.lasso.cv$glmnet.fit

# the λ at which the minimum MSE is achieved
lambda.min <- fit.glmnet.lasso.cv$lambda.min
#print(lambda.min)

# make predictions from the cross-validated glmnet model using lambda min(optimal lambda)
y.hat.glmnet.scaled <- predict(fit.glmnet.lasso.cv,x.predict,s="lambda.min")
# unscale to get the actual magnitude
y.hat.glmnet.unscaled <- unscale(y.hat.glmnet.scaled,dataIn.y.scaled)
# get the model error
#sigma.glmnet.scaled <- estimateSigma(x,y,intercept = FALSE, standardize = FALSE)$sigmahat
#sigma.glmnet.unscaled <- unscale(sigma.glmnet.scaled,dataIn.y.scaled)
sigma.glmnet.scaled <- 9;

# get the model coefficients at optimal lambda (lambda min)
beta.hat.glmnet.scaled <- predict(fit.glmnet.lasso.cv,s="lambda.min",exact=TRUE,type="coefficients")

# get the regression formula for optimal lambda
y.name=names(y.scaled)[1]
x.names=names(x.scaled)
formula.lasso = paste(y.name,paste0(x.names[which(!beta.hat.glmnet.scaled==0)-1],collapse=' + '),sep=' ~ ')


# compute p-values and confidence intervals
#lasso.inference <- fixedLassoInf(x,y,beta.hat.glmnet.scaled[-1],lambda.min,sigma=sigma.glmnet.scaled)


#########################################
# In the following section, we are going to perform ordinary least squares (OLS) regression
# on the data set to cross-compare the model coefficients with the lasso results.
# If n<p then skip this comparision and set N/A for the OLS coefficient column in the
# section 

no_ols = TRUE
if (!no_ols){
  # Get the formula used for simple linear regression model 
  formula.ols = paste(y.name,paste0(x.names,collapse=' + '),sep=' ~ ')

  # Since the Lasso analysis was done with standardized data, we are going to use
  # the standardized data as a training set for the OLS too. 
  dataIn.train <- cbind(y.scaled,x.scaled[1:(nrow(x.scaled)-1),])
  dataIn.test <-x.scaled[(nrow(x.scaled)),]

  # Make predictions from the simple linear model for comparision
  mod.ols <- lm(formula.ols, dataIn.train)
  y.hat.ols <- predict(mod.ols,newdata=dataIn.test,interval="prediction")
  beta.hat.ols.scaled <- summary(mod.ols)$coefficients

  # Following section performs an anova test to compare the models
  # For this the models needs to be nested. i.e. same outcome variable and 
  # model 2 contains all the variables of model 1 plus 2 additional variables

  mod.lasso <- lm(formula.lasso,dataIn.train) 
  #mod.lm <- lm(formula.ols,dataIn.train) 
  #anova.compare <- anova(mod.lasso,mod.lm)
}

source("BEDROCK-HELP-FUNC.R")
source("BEDROCK-DOC.R")
