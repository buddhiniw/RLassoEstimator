#############################
#  This code uses Gaussian linear regression with Elastic Net regularization to predict
#  real estate selling prices utilizing the glment package in R. 
# 
#  Reference analysis - "Fitting the Lasso Estimator using R" by Finch, W. Holmes; Finch, Maria E. Hernandez
#  Practical Assessment, Research & Evaluation, 
#  v21 n7 May 2016
#  Reference methodolo
#  
#  R script author - B. Waidyawansa (04/30/2018)
#############################


# Load required packages
library(caret) # Package to traint the model
library(glmnet) # Package to fit ridge/lasso/elastic net models
#library(selectiveInference)
#library(DMwR)
library(boot) # Package to do bootstrap error estimates

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

###################################################################
# Standardize the predictor(x) variables and response(y) variable
###################################################################
# Standarizing/scaling the predictor variables prior to fitting the model will ensure
# the lasso penalization will treat different scale explanatory variables on a more 
# equal footing. 
# BUT only continuos predictors needs to be standardized.
# DONOT scale factor variables (using 1/0) used for yes/no situations.
#
# NOTE - We cannot use the preProcess option in the caret package to do the scaling 
# because the real estate datasets contain factor variables.
#
###############################
# Scale x (predictor) variables
###############################
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

###############################
# Scale y (responce) variable
###############################
dataIn.y <- dataIn[1:(nrow(dataIn)-1),1,drop=FALSE]
dataIn.y.scaled <- scale(dataIn.y,center=TRUE, scale=TRUE)
y.scaled <- as.data.frame(dataIn.y.scaled)

# Create the X and Y regression matrices
x <- as.matrix(x.scaled[1:(nrow(x.scaled)-1),])
y <- as.matrix(y.scaled)

###################################################################
# Get the scaled test data set.
# This program assumes the test data are in the last row of the input
# dataset
###################################################################
dataIn.scaled.test <- x.scaled[nrow(x.scaled),]
x.test <-as.matrix(dataIn.scaled.test)


###################################################################
# Use caret package to train the glmnet model to get the 
# best parameter values for alpha and lambda
###################################################################

# Set alpha and lambda grid
lambda.grid <- 10^seq(2,-2,length=100)
alpha.grid <- seq(0,1,length=10)

# Setup leave-one-out-cross-validation method for train function. 
train.control = trainControl(method = "LOOCV", number = 10, repeats=1)

# Setup serach grid for alpha and lambda
search.grid <- expand.grid(.alpha = alpha.grid, .lambda = lambda.grid)

# Full dataset after scaling
dataIn.scaled <- cbind(y,x)
# Remove NA entries
dataIn.scaled <- na.omit(dataIn.scaled)

#The formula used for simple linear regression model 
y.name=names(dataIn)[1]
x.names=names(dataIn)[-1]
formula.lm = paste(y.name,paste0(x.names,collapse=' + '),sep=' ~ ')

# perfrom cross-validated forecasting of SellingPrice using all features
set.seed(42)
train.elnet = train(
    x.scaled[1:(nrow(x.scaled)-1),], y.scaled[[1]],
    method = "glmnet",
    tuneGrid = search.grid,
    trControl = train.control
)


# Plot CV performance
plot(train.elnet)

# Retrun best tuning parameters lambda and alpha
best.alpha <- train.elnet$bestTune$alpha
best.lambda <- train.elnet$bestTune$lambda

# Get the best model (model with best alpha)
final.glmnet.model <- train.elnet$finalModel

# Get the model coefficients at optimal lambda (lambda min)
beta.hat.glmnet.scaled <- coef(final.glmnet.model, s=best.lambda)

###################################################################
# Make predictions from final model
###################################################################
# Make price prediction for test data
y.hat.glmnet.scaled <- predict(final.glmnet.model,x.test,s=best.lambda)
# Unscale to get the actual magnitude
y.hat.glmnet.unscaled <- unscale(y.hat.glmnet.scaled,dataIn.y.scaled)


###################################################################
# Estimate starndard errors using bootstrap method
###################################################################
# NOTE bootstrapping is only real way to estimate the std. errors 
# for a penalized regression. BUT it is unclear how meaningful the std. errors are in this case. 


test <- function(data, idx){
    bootstrap.data <- data[idx, ]
    bootstrap.mod <- train(SellingPrice ~.,
                      data = bootstrap.data, 
                      method = "glmnet", 
                      trControl = trainControl(method = "none"),
                      tuneGrid = train.elnet$bestTune)
  
    as.vector(coef(bootstrap.mod$finalModel, train.elnet$bestTune$lambda))
}

boot.samples <- boot(dataIn.scaled, test, 100L)

#########################################

#source("BEDROCK-HELP-FUNC.R")
#source("BEDROCK-DOC.R")
