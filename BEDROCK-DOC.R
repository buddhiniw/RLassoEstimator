#############################
# Load required packages
#############################

library(methods)
library(datasets)
library(utils)
library(grDevices)
library(graphics)
library(stats)
library(glmnet)
library(ReporteRs)
library(selectiveInference)
library(plotmo)
library(prettyR)
library(data.table)
library(xtable)

###################################################################
#  Prepare to generate MS Word Document
###################################################################
fileName=gsub(".csv$",".docx",inFileName)
fileName=gsub("data/","",fileName)
outfileName=paste("out/elnet-lars-out-",fileName)

# set default font size to 10
options( "ReporteRs-fontsize" = 10 )
options( "ReporteRs-default-font" = "Arial" )


#Create a new word document
print('Open new doccument to save output')
doc <- docx()
doc = map_title(doc, stylenames = c("Titre1", "Titre2", "Titre3",
                                    "Titre4", "Titre5", "Titre6",
                                    "Titre7", "Titre8", "Titre9" ) )
doc <- addParagraph( doc, "Real Estate Price Prediction with Elastic-net Regression", stylename = "TitleDoc" )
doc <- addParagraph( doc , paste("Input Data file:",args[1],collapse=" ")  , stylename="Citationintense")

###################################################################
# Descriptive statistics from unscaled data
###################################################################
print('Get Descriptive Stats')
# Get the original data frame minus the test data set
df=dataIn[1:(nrow(dataIn)-1),]
# Prepare Descriptive Satistics FlexTable
dataIn.desc.ft=FlexTable.descriptive(df)
doc <- addTitle( doc, value = "Basic summary statistics",level=2)
dataIn.desc.ft<-setFlexTableWidths(dataIn.desc.ft,widths = c(1.5,0.5,1.2,1.2,1.2,1.2))
doc <- addFlexTable( doc, dataIn.desc.ft)
doc <- addParagraph( doc, " NOTE - No summary statistics are provided for categorical variables.",stylename = "Normal")


###################################################################
# Check for correlations between predictors
###################################################################
print('Check for Correlations in Predictors')
corrplot::corrplot(cov(as.matrix(x.scaled)),method = "number")
doc <- addTitle( doc, "Correlations Between Predictors", level =  2 )
plotFunc<-function(){
  corrplot::corrplot(cov(as.matrix(x.scaled)),method = "number")
}
doc <- addPlot(doc, plotFunc, width=6, height = 4)
doc <- addPageBreak( doc )


###################################################################
# Selection of Lambda and S/alpha
###################################################################
print('Select Best Lambda and s Using LOOCV')
doc <- addTitle( doc, value = "Tuning Parameter Selection Using LOOCV",level=2)
plotFunc<-function(){
  par(mar = c(5,5,6,5))
  print(plot(train.enet,plotType = "line", xlab="Fraction (s)",scales=list(x=list(cex=0.75), y=list(cex=0.75))))
  #title("Regularization parameter vs RMSE curve from LOOCV)", adj = 0.5, line = 4)
}
trellis.par.set(caretTheme())
doc <- addPlot(doc, plotFunc)
doc <- addParagraph( doc, sprintf("\nFrom above plot, lambda =%.2f and s =%.2f gives the minimum RMSE model.",
                                    best.lambda, best.fraction),stylename = "Normal")
doc <- addPageBreak( doc )



###################################################################
# Plot Variable Importance
###################################################################
print('Plot variable importance')
doc <- addTitle( doc, value = "Plot Variable Importance",level=2)
plotFunc<-function(){
  #par(mar = c(5,5,6,5))
  print(plot(varImp(train.enet)))
  #title("The absolute value of the t–statistic )", adj = 0.5, line = 4)
}

doc <- addPlot(doc, plotFunc,  width=5, height = 4)
doc <- addPageBreak( doc )

################ Model coefficients ################
print('Extract Model Coefficients')
doc <- addTitle( doc, "Standardized Model Coefficients", level =  2 )
FTablecoef<-FlexTable.enet.coef(beta.hat.enet.scaled)
doc <- addFlexTable(doc,FTablecoef)
doc <- addParagraph( doc, " NOTE std. errors are calculated using bootstrapping which is the only way to determine coef. errors for a penalized regression. But the errors should be only used for reference. It is yet unclear how meaningful the std. errors are in penalized regression.",stylename = "Normal")


################ MODEL PREDICTION ################
print('Make Predictions')
doc <- addTitle( doc, value = "Model Prediction",level=2)
FTable.predict <- FlexTable.predict(y.hat.enet.unscaled,prediction.error, prediction.rsquared)
doc <- addFlexTable( doc, FTable.predict )


# write the doc
writeDoc( doc, file = outfileName )
print('Done!')
#source("BEDROCK-HELP-FUNC.R")
