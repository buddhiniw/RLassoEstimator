#  This is a code to do Lasso on real-estate data using glmnet package
# 
#  Reference - "Fitting the Lasso Estimator using R" by Finch, W. Holmes; Finch, Maria E. Hernandez
#  Practical Assessment, Research & Evaluation, 
#  v21 n7 May 2016
#  Author by B. Waidyawansa (04/30/2018)
# 
#  Gaussian Assumption in Lasso Method
#  Command line run example
#  Rscript --default-packages=methods,datasets,utils,grDevices,graphics,stats,glmnet,ReporteRs,selectiveInference BEDROCK.R ason1.csv
#  TO RUN in RStudio see section at line 20
#  TO Change default Working Directory see section at line 20


# Remove comment from this line to suppress warnings
# options(warn=-1)

#############################
# Load required packages
#############################
library(glmnet) # Package to fit ridge/lasso/elastic net models
library(selectiveInference)
library(DMwR)

rm(list=ls())


###################################################################################
################ BEGIN: THIS SECTION MUST BE CUSTOMIZED FOR SYSTEM ################
##  Set the working directory
home =Sys.getenv("HOME")
wd = paste0(home,"/MyWork/Upwork/R_bedrock_gui/RLassoEstimator")
setwd(wd)

# get file name from command line arguments if run as script from command line or PHP
args <- commandArgs(TRUE)
print(args)

# Remove commnet from the start of next line to run in RStudio
# Change file name as neccessary
args[1]="ason1.csv"
#args[1]="Dataset7Obs.csv"

# Get the input data file name
inFileName=args[1]


################ ENDL:  THIS SECTION MUST BE CUSTOMIZED FOR SYSTEM ################
###################################################################################


#Read the data from a .dat file, print the data to be sure that it# 
#was read in correctly, and create matrices of the independent and#
#dependent variables.#
dataIn <- read.csv(inFileName, header=T)


# First standardize the predictor variables
#
# Standarizing/scaling the predictor variables prior to fitting the model will ensure
# the lasso penalization will treat different scale explanatory variables on a more equal footing
# But only continuos predictors needs to be standardized.
# DONOT scale factor variables (using 1/0) used for yes/no situations.
# Therefore seperate the dataframe with continuos variables and factors

# Get x variables (including the test data row)
dataIn.x <- dataIn[,-1]

# Identify columns with categorical data (0/1)
cat.vars <- apply(dataIn.x,2,function(x) { all(x %in% 0:1) })
dataIn.continuous <- dataIn.x[!cat.vars]
dataIn.catrgorical <- dataIn.x[cat.vars]

# Now scale the continuous varibales ONLY
dataIn.continuous.scaled <-scale(dataIn.continuous,center=TRUE, scale=TRUE)

# Create the scaled x matrix
x.scaled <- cbind(as.data.frame(dataIn.continuous.scaled),dataIn.catrgorical)


dataIn.y <- dataIn[1:(nrow(dataIn)-1),1,drop=FALSE]
dataIn.y.scaled <- scale(dataIn.y,center=TRUE, scale=TRUE)
y.scaled <- as.data.frame(dataIn.y.scaled)

# Create the X and Y regression matrices
x <- as.matrix(x.scaled[1:(nrow(x.scaled)-1),])
y <- as.matrix(y.scaled)


# Get the test data set assuming the last row  in the original data set is the test set
dataIn.scaled.test <- x.scaled[nrow(x.scaled),]
x.test <-as.matrix(dataIn.scaled.test)


# Fit LASSO using glmnet(y=, x=). Gaussian model is default. alpha=1 for lasso only.
# Set standardize = FALSE because the variables were standardized in the begining of the 
# analysis using "scale" function. intercept =FALSE because responce variables(y) is also scaled
# Function produces series of fits for many values of lambda. 
fit.glmnet.lasso <- glmnet(x,y,family="gaussian",alpha=1,standardize=FALSE,intercept = FALSE)
plot(fit.glmnet.lasso, xvar="lambda")

# Perform nfolds=sample size cross-validation  (leave-one-out-cross-validation)
# runs glmnet nfolds+1 times; the first to get the lambda sequence, and then the
# remainder to compute the fit with each of the folds omitted. 
# The error is accumulated, and the average error and standard deviation over the folds is computed
fit.glmnet.lasso.cv <- cv.glmnet(x,y,alpha=1,nfolds = length(y),type.measure="mse") 
plot(fit.glmnet.lasso.cv)
lasso.glmnet <- fit.glmnet.lasso.cv$glmnet.fit

# the λ at which the minimal MSE is achieved
lambda.min <- fit.glmnet.lasso.cv$lambda.min
#print(lambda.min)

# make predictions from the cross-validated glmnet model using lambda min(optimal lambda)
y.hat.glmnet.scaled <- predict(fit.glmnet.lasso.cv,x.test,s="lambda.min")
# unscale to get the actual magnitude
y.hat.glmnet.unscaled <- unscale(y.hat.glmnet.scaled,dataIn.y.scaled)
print(y.hat.glmnet.unscaled)

# get the model coefficients at optimal lambda (lambda min)
beta.hat.glmnet.scaled <- coef(fit.glmnet.lasso.cv,s="lambda.min")


# In the following section, we are going to perform ordinary least squares (OLS) regression
# on the data set to cross-compare the model coefficients with the lasso results.

# Get the formula used for simple linear regression model 
y.name=names(dataIn)[1]
x.names=names(dataIn)[-1]
formula.lm = paste(y.name,paste0(x.names,collapse=' + '),sep=' ~ ')
#koala.min.formula=paste(wombat.y.name,paste0(wombat.x.names[koala.min.Xs],collapse=' + '),sep=' ~ ')

# Since the Lasso analysis was done with standardized data, we are going to use
# the standardized data as a training set for the OLS too. 
dataIn.train <- cbind(y.scaled,x.scaled[1:(nrow(x.scaled)-1),])
dataIn.test <-dataIn[nrow(dataIn),-1]

# Make predictions from the simple linear model for comparision
fit.lm <- lm(formula.lm, data=dataIn.train)
y.hat.lm <- predict(fit.lm,newdata=dataIn.test,interval="prediction")

source("BEDROCK-HELP-FUNC.R")
source("BEDROCK-DOC.R")
