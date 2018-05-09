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

rm(list=ls())


###################################################################################
################ BEGIN: THIS SECTION MUST BE CUSTOMIZED FOR SYSTEM ################
##  Set the working directory
home =Sys.getenv("HOME")
wd = paste0(home,"/MyWork/Upwork/R_bedrock_gui")
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

# Standardize the data prior to fitting the model
# Then the penalization will treat different scale explanatory variables 
# on a more equal footing
dataIn.scaled <- as.data.frame(scale(dataIn, center=TRUE, scale=TRUE))
#dataIn.scaled <- dataIn

# The last line of the loaded data set should be removed from the model to get the training set. 
dataIn.scaled.train <- dataIn.scaled[1:(nrow(dataIn.scaled)-1),]
# The last line contains data to test the resulting model.
dataIn.scaled.test <-dataIn[nrow(dataIn),-1]
#dataIn.scaled.test <-dataIn.scaled[nrow(dataIn.scaled),-1]

x.test <-as.matrix(dataIn.scaled.test)

#x.test <-(dataIn.scaled.test)

# Create the X and Y regression matrices
x <- as.matrix(dataIn.scaled.train[,-1])
#y <- is.na(dataIn[[1]])
y <- as.matrix(dataIn.scaled.train[,1])



# Fit LASSO using glmnet(y=, x=). Gaussian model is default. alpha=1 for lasso only.
# Set standardize = FALSE because the variables were standardized in the begining of the 
# analysis using "scale" function.
# Function produces series of fits for many values of lambda. 
fit.glmnet.lasso <- glmnet(x,y,alpha=1,standardize=F)
plot(fit.glmnet.lasso, xvar="lambda")

# Perform nfolds=sample size cross-validation  (leave-one-out-cross-validation)
# runs glmnet nfolds+1 times; the first to get the lambda sequence, and then the
# remainder to compute the fit with each of the folds omitted. 
# The error is accumulated, and the average error and standard deviation over the folds is computed
fit.glmnet.lasso.cv=cv.glmnet(x,y,alpha=1,nfolds = length(y),type.measure="mse") 
plot(fit.glmnet.lasso.cv)
lasso.glmnet = fit.glmnet.lasso.cv$glmnet.fit

# the λ at which the minimal MSE is achieved
lambda.min = fit.glmnet.lasso.cv$lambda.min
print(lambda.min)
#print(min(fit.glmnet.lasso.cv$lambda))

# get the model coefficients at optimal lambda (lambda min)
coef(fit.glmnet.lasso.cv,s="lambda.min")

# make predictions from the cross-validated glmnet model
y.hat.glmnet = predict(fit.glmnet.lasso.cv,x.test,s=lambda.min)
beta.hat.glmnet = coef(fit.glmnet.lasso.cv,s=lambda.min)


# In the following section, we are going to perform simple linear regression
# on the data set to cross-compare the model coefficients with the lasso results.

# The formula used for simple linear regression model 
y.name=names(dataIn)[1]
x.names=names(dataIn)[-1]
formula.lm = paste(y.name,paste0(x.names,collapse=' + '),sep=' ~ ')
fit.lm <- lm(formula.lm, data=dataIn.scaled.train )
y.hat.lm <- predict(fit.lm,interval="predict")

#lm.formula <- y.name,paste0(x.names[koala.min.Xs],collapse=' + '),sep=' ~ '

# koala.min.formula=paste(wombat.y.name,paste0(wombat.x.names[koala.min.Xs],collapse=' + '),sep=' ~ ')
# koala.min.lm=lm(koala.min.formula,data=df)
# koala.min.lm.predict=predict(koala.min.lm,as.data.frame(wombat.x.predict),interval="predict") #produces predicted values, obtained by evaluating the regression function in the data frame
# #plot(koala.min.lm)




#dataIn.z.lasso.cv <- cv.glmnet(x,y, type.measure="mse", nfolds=10) 
#plot(dataIn.z.lasso.cv)

# ## Fit the model using glmnet
# koala=glmnet(wombat.x,wombat.y,standardize=TRUE) #contains all the relevant information of the fitted model
# # Do folds=sample size cross-validation



# the original data is called "wombat"
#wombat=as.matrix(dataIn)






# #wombat=scale(wombat)
# wombat.y.name=names(dataIn)[1]
# wombat.x.names=names(dataIn)[-1]
# # need this to undo scaling
# wombat.y.raw = wombat[!predictY,1]
# 
# 
# # what to predict
# wombat.x.predict=wombat[predictY,-1,drop=FALSE]
# 
# # regression x and y matrix
# wombat.x<-wombat[!predictY,-1]
# wombat.y<-wombat[!predictY,1]
# 
# # lamda values to use in lasso
# grid=10^seq(10,-2,length=100)
# 
# df=data.frame(cbind(wombat.y,wombat.x))
# colnames(df)[1]=wombat.y.name
# 
# ## Fit the model using glmnet
# koala=glmnet(wombat.x,wombat.y,standardize=TRUE) #contains all the relevant information of the fitted model
# # Do folds=sample size cross-validation
# #koala.cv=cv.glmnet(wombat.x,wombat.y,nfolds = 10) #contains a list with all the ingredients of the cross-validation fit
# koala.cv=cv.glmnet(wombat.x,wombat.y,nfolds = nrow(dataIn)) #contains a list with all the ingredients of the cross-validation fit
# 
# koala.min=koala.cv$lambda.min  #the value of λ that gives minimum mean cross-validated error
# koala.1se=koala.cv$lambda.1se  # the lambda which gives the most regularized model such that error is within one standard error of the minimum
# 
# #Conduct post‐selection inference for the lasso#
# # lasso.sigma<-estimateSigma(wombat.x,wombat.y)
# # lasso.signaltonoise = lasso.sigma$sigmahat / sd(wombat.y)
# # lasso.beta = coef(koala, s=koala.cv$lambda.min)[-1]
# # lasso.inference.min  = fixedLassoInf(wombat.x,wombat.y,lasso.beta,koala.cv$lambda.min, sigma=lasso.sigma$sigmahat)
# # lasso.inference.min.text = capture.output(print(lasso.inference.min))[6:9]
# # lasso.beta = coef(koala, s=koala.cv$lambda.1se)[-1]
# # lasso.inference.1se = fixedLassoInf(wombat.x,wombat.y,lasso.beta,koala.cv$lambda.1se, sigma=lasso.sigma$sigmahat)
# # lasso.inference.1se.text = capture.output(print(lasso.inference.1se))[6:9]
# #names(lasso.inference)
# 
# ############ Lambda Min Section
# koala.min.where=which(koala.min==koala.cv$lambda) # the λ at which the minimal MSE is achieved
# koala.min.coef=predict(koala,s=koala.min,type="coef") # extract coefficients at the value of λ that gives minimum mean cross-validated error
# # Do lasso regression
# koala.min.pred=predict(koala,newx=wombat.x.predict,s=koala.min) # make predictions
# koala.min.pred.exact=predict(koala,newx=wombat.x.predict,s=koala.min,exact=TRUE) # make predictions using the exact values of coefficients
# koala.min.R2 <- koala$dev.ratio[koala.min.where]
# koala.min.pred.text = as.character(round(unclass(koala.min.pred.exact),6))
# koala.min.R2.text = as.character(round(koala.min.R2,6))
# koala.min.Index <- which(as.vector(koala.min.coef) != 0)
# 
# # Get the formula used in lasso
# koala.min.Coefficients <- koala.min.coef[koala.min.Index]
# koala.min.Xs = koala.min.Index-1
# koala.min.Xs = koala.min.Xs[koala.min.Xs!=0]
# 
# # Perfrom linear regression
# koala.min.formula=paste(wombat.y.name,paste0(wombat.x.names[koala.min.Xs],collapse=' + '),sep=' ~ ')
# koala.min.lm=lm(koala.min.formula,data=df)
# koala.min.lm.predict=predict(koala.min.lm,as.data.frame(wombat.x.predict),interval="predict") #produces predicted values, obtained by evaluating the regression function in the data frame
# #plot(koala.min.lm)
# 
# 
# ############ Lambda 1se Section
# koala.1se.where=which(koala.1se==koala.cv$lambda)
# 
# 
# koala.1se.coef=predict(koala,s=koala.1se,type="coef") # extract coefficients at a single value of lambda
# koala.1se.pred=predict(koala,newx=wombat.x.predict,s=koala.1se) # make predictions
# koala.1se.pred.exact=predict(koala,newx=wombat.x.predict,s=koala.1se,exact=TRUE) # make predictions
# koala.1se.R2 <- koala$dev.ratio[koala.1se.where]
# koala.1se.pred.text = as.character(round(unclass(koala.1se.pred),6))
# koala.1se.R2.text = as.character(round(koala.1se.R2,6))
# 
# koala.1se.Index <- which(as.vector(koala.1se.coef) != 0)
# koala.1se.Coefficients <- koala.1se.coef[koala.1se.Index]
# koala.1se.Xs = koala.1se.Index-1
# koala.1se.Xs = koala.1se.Xs[koala.1se.Xs!=0]
# 
# koala.1se.formula=paste0(wombat.y.name,"~",paste0(wombat.x.names[koala.1se.Xs],collapse=' + '))
# koala.1se.lm=lm(koala.1se.formula,data=df)
# koala.1se.lm.predict=predict(koala.1se.lm,as.data.frame(wombat.x.predict),interval="predict")
# 
# 
# source("BEDROCK-DOC.R")
