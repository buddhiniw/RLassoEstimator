#  This is a code to do Lasso on realestate data using glmnet package
# 
#  Reference - "Fitting the Lasso Estimator using R" by Finch, W. Holmes; Finch, Maria E. Hernandez
#  Practical Assessment, Research & Evaluation, 
#  v21 n7 May 2016
#  Code updates by B. Waidyawansa (04/30/2018)
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

# Standardize the variables prior to conducting data analysis
# because the penalization will then treat different scale explanatory variables 
# on a more equal footing
dataIn.z <- scale(dataIn, center=TRUE, scale=TRUE)

# The last line of the loaded data set should be removed from the model to get the training set. 
# The last line contains data to test the resulting model.
dataIn.z.train <- dataIn.z[1:(nrow(dataIn.z)-1),]

# glmnet() requires x to be in matrix class, so saving out 
# the separate variables to be used as Y and X.
x <- as.matrix(dataIn.z.train[,])
#y <- is.na(dataIn[[1]])
y <- dataIn.z.train[,1]

# Fit LASSO by glmnet(y=, x=). Gaussian is default, but other families are available  
# Function produces series of fits for many values of lambda.  

fit <- glmnet(x,y,standardize=TRUE) #contains all the relevant information of the fitted model
plot(fit)


#dataIn.z.lasso.cv <- cv.glmnet(x,y, type.measure="mse", nfolds=10) 
#plot(dataIn.z.lasso.cv)

# ## Fit the model using glmnet
# koala=glmnet(wombat.x,wombat.y,standardize=TRUE) #contains all the relevant information of the fitted model
# # Do folds=sample size cross-validation


# what is going to be predicted
#predictY=is.na(dataIn[[1]])
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
