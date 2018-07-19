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
#outfileName="~/MyWork/Upwork/R_bedrock_gui/RLassoEstimator/elnet-lars-test.docx"
outfileName=paste("~/MyWork/Upwork/R_bedrock_gui/RLassoEstimator/elnet-lars-test-",fileName)
# set default font size to 10
options( "ReporteRs-fontsize" = 10 )
options( "ReporteRs-default-font" = "Arial" )


#Create a new word document
print('Open new doccument')
doc <- docx()
doc = map_title(doc, stylenames = c("Titre1", "Titre2", "Titre3",
                                    "Titre4", "Titre5", "Titre6",
                                    "Titre7", "Titre8", "Titre9" ) )
doc <- addParagraph( doc, "Real Estate Price Prediction with Elastic-net Regression", stylename = "TitleDoc" )
doc <- addParagraph( doc , paste("Input Data file:",args[1],collapse=" ")  , stylename="Citationintense")

###################################################################
# Descriptive statistics from unscaled data
###################################################################
print('Descriptive Stats')
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
print('Correlations')
corrplot::corrplot(cov(as.matrix(x.scaled)),method = "number")
doc <- addTitle( doc, "Correlations Between Predictors", level =  2 )
plotFunc<-function(){
  corrplot::corrplot(cov(as.matrix(x.scaled)),method = "number")
}
doc <- addPlot(doc, plotFunc, width=6, height = 4)
doc <- addPageBreak( doc )


###################################################################
# Plot coefficients for best model from caret
###################################################################
# doc <- addTitle( doc, "Plot Coefficients by L1 Norm for Elastic-net", level =  2 )
# plotFunc<-function(){
#   par(mar = c(5,5,6,5))
#   plot(final.enet.model)
#   title("Size of Coefficient by L1 Norm \nand Number of Non-Zero Coefficients (top axis)", adj = 0.5, line = 4)
# }
# 
# 
# #### Coefficients
# doc <- addParagraph( doc, value = "Model Coefficients by Log of Lambda for the Elastic-net",stylename = "rPlotLegend")
# doc <- addParagraph( doc, value = "The axis above indicates the number of nonzero coefficients at the current λ,
# which is the effective degrees of freedom.  The larger lambda the more coefficients are forced to zero.",stylename = "Normal")
# doc <- addPageBreak( doc )

###################################################################
# Selection of Lambda and S/alpha
###################################################################
print('Lambda and Alpha selection')
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



################ Regression equation from LASSO ################
#doc <- addTitle( doc, "Regression equation from LASSO", level =  2 )
#doc <- addParagraph( doc, value = (formula.lasso),stylename = "Normal")

###################################################################
# Plot Variable Importance
###################################################################
print('Plot variable importance')
doc <- addTitle( doc, value = "Variable Importance",level=2)
plotFunc<-function(){
  #par(mar = c(5,5,6,5))
  print(plot(varImp(train.enet)))
  #title("The absolute value of the t–statistic )", adj = 0.5, line = 4)
}

doc <- addPlot(doc, plotFunc,  width=5, height = 4)
doc <- addPageBreak( doc )

################ Model coefficients ################
print('Model Coefficients')
doc <- addTitle( doc, "Standardized Model Coefficients", level =  2 )
FTablecoef<-FlexTable.enet.coef(beta.hat.enet.scaled)
doc <- addFlexTable(doc,FTablecoef)
doc <- addParagraph( doc, " NOTE std. errors are calculated using bootstrapping which is the only way to determine coef. errors for a penalized regression. But the errors should be only used for reference. It is yet unclear how meaningful the std. errors are in penalized regression.",stylename = "Normal")

################ ANOVA ################
# doc <- addTitle( doc, "ANOVA comparing LASSO and OLS Models", level =  2 )
# FTable.2mod.anova <- FlexTable.2mod.anova(mod.lasso,mod.ols)
# doc <- addFlexTable( doc, FTable.2mod.anova)


################ Goodness of Fit ################
#doc <- addTitle( doc, value = "Goodness of Fit Statistics",level=2)
#doc <- addParagraph( doc, sprintf("\n R^2 =%.2f",prediction.rsquared),stylename = "Normal")
#FTable.summary <- FlexTable.lm.quality(mod.lasso)
#koala.min.ft.lm.quality<-setFlexTableWidths(koala.min.ft.lm.quality,widths = c(2, 3.5))
#doc <- addFlexTable( doc, FTable.summary )
#doc <- addPageBreak( doc )

################ MODEL PREDICTION ################
print('Price Prediction')
doc <- addTitle( doc, value = "Model Prediction",level=2)
FTable.predict <- FlexTable.predict(y.hat.enet.unscaled,prediction.error)
doc <- addFlexTable( doc, FTable.predict )


# write the doc
writeDoc( doc, file = outfileName )
print('Done!')
#source("BEDROCK-HELP-FUNC.R")
