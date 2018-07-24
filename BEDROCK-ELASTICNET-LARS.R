#############################
#  This R code uses Gaussian linear regression with Elastic Net regularization to predict
#  real estate selling prices. 
# 
#  Reference - "Regularization and variable selection via the elastic net"
#  Hui Zou and Trevor Hastie
#  J. R. Statist. Soc. B (2005) 67 , Part 2 , pp. 301–320
# 
#  
#  R script author - B. Waidyawansa (04/30/2018)
#############################


# Load required packages
library(caret) # Package to traint the model
library(glmnet) # Package to fit ridge/lasso/elastic net models
library(DMwR)
library(boot) # Package to do bootstrap error estimates
library(elasticnet)

# Clean up the directory
rm(list=ls())

# Get file name from command line arguments if run as script from command line or PHP
args <- commandArgs(TRUE)

# Test if the input file name is given: if not, return an error
if (length(args)==0) {
  stop("Correct Usage: Rscript BEDROCK-ELASTICNET_LARS.R <location/data_file_name.csv>", call.=FALSE)
}


# Get the input data file name
inFileName = paste0("data/",args[1])
cat("Reading data from - ",inFileName)

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
###############################b
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
# Use caret package to train the elasticnet model to get the 
# best parameter values for lambda(weight decay) and s(fraction).
# From reference -
# Pick a (relatively small) grid of values for λ2 , say .0, 0:01, 0:1, 1, 10, 100/. 
# Then, for each λ2 , algorithm LARS-EN produces 
# the entire solution path of the elastic net. 
# The other tuning parameter (λ1 , s or k) is selected by tenfold CV. 
# The chosen λ2 is the one giving the smallest CV error.
###################################################################

# Set s and lambda grid 
#lambda.grid <- 10^seq(5,-5,length=100)
lambda.grid <- c(0,0.01,0.1,1,10,100)

#lambda.grid <- seq(0,10,by = 1)
s.grid <- seq(0,1,by=0.05)

# Use leave-one-out-cross-validation method for cross-validation.
# Using RMSE
train.control = trainControl(method = "LOOCV")

# Setup serach grid for s and lambda
#search.grid <- expand.grid(.alpha = alpha.grid, .lambda = lambda.grid)
search.grid <- expand.grid(.fraction = s.grid, .lambda = lambda.grid)

# Full dataset after scaling
dataIn.scaled <- cbind(y,x)
# Remove NA entries
dataIn.scaled <- na.omit(dataIn.scaled)

# perfrom cross-validated forecasting of SellingPrice using all features
set.seed(42)
train.enet = train(
    x.scaled[1:(nrow(x.scaled)-1),], y.scaled[[1]],
    method = "enet",
    metric = "RMSE",
    tuneGrid = search.grid,
    normalize = FALSE,
    intercept = FALSE,
    trControl = train.control
)


# Plot CV performance
plot(train.enet)

# Retrun best tuning parameters lambda and alpha
best.fraction <- train.enet$bestTune$fraction
best.lambda <- train.enet$bestTune$lambda

# Get model prediction error(RMSE) from the best tune
best = which(rownames(train.enet$results) == rownames(train.enet$bestTune))
best.result = train.enet$results[best, ]
rownames(best.result) = NULL
prediction.error <- best.result$RMSE
prediction.rsquared <-best.result$Rsquared

# Get the best model (model with best alpha that gives minimum RMSE)
# By default, the train function chooses the model with the largest performance value
# (or smallest, for mean squared error in regression models).
final.enet.model <- train.enet$finalModel

# Get the model coefficients at optimal lambda from the final model
beta.hat.enet.scaled <- predict.enet(final.enet.model,
                                     s=best.fraction,
                                     type="coefficient",
                                     mode="fraction")

###################################################################
# Make predictions using the final model selected by caret
###################################################################
y.hat.enet.scaled <- predict.enet(final.enet.model,
                                  as.data.frame(x.test),
                                  s=best.fraction,
                                  type="fit",
                                  mode="fraction")
# Unscale to get the actual magnitude
y.hat.enet.unscaled <- unscale(y.hat.enet.scaled$fit,dataIn.y.scaled)


#########################################
source("BEDROCK-HELP-FUNC.R")
source("BEDROCK-DOC.R")
