#  This is a code to replicate the analyses method in:
#  Fitting the Lasso Estimator using R by Finch, W. Holmes; Finch, Maria E. Hernandez
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
library(flextable)
library(xtable)

#############################
# Helper functions to create Tables For MS Word .docx
#############################

# create a FlexTable of coeficients from a Linear Model
FlexTable.lm.coef <- function(datalm) {
   data = as.data.frame( summary(datalm)$coefficients )

   # get signif codes
   signif.codes = cut( data[,4]
                       , breaks = c( -Inf, 0.001, 0.01, 0.05, Inf)
                       , labels= c("***", "**", "*", "" ) )

  # format the data values
  data[, 1] = formatC( data[, 1], digits=3, format = "f")
  data[, 2] = formatC( data[, 2], digits=3, format = "f")
  data[, 3] = formatC( data[, 3], digits=3, format = "f")
  data[, 4] = ifelse( data[, 4] < 0.001, "< 0.001", formatC( data[, 4], digits=5, format = "f"))
  # add signif codes to data
  data$Signif = signif.codes

  # create an empty FlexTable
  coef_ft = FlexTable( data = data, add.rownames=TRUE
                          , body.par.props = parRight(), header.text.props = textBold()
                          , header.columns = T
  )

  # center the first column and set text as bold italic
   coef_ft[,1] = parCenter()
   coef_ft[,1] = textBoldItalic()

   # define borders

   coef_ft = setFlexTableBorders( coef_ft
                                  , inner.vertical = borderNone(), inner.horizontal = borderDotted()
                                  , outer.vertical = borderNone(), outer.horizontal = borderSolid()
   )
  # coef = add
   coef_ft
}


# create a FlexTable of quality statistics from a Linear Model
  FlexTable.lm.quality <- function(datalm) {
  datasum = summary(datalm)
  datasum$pstatistic=1-pf(datasum$fstatistic[1],datasum$fstatistic[2],datasum$fstatistic[3])

  datasum$FString=paste0(paste(c("F =","on", "and"),datasum$fstatistic,collapse=" ")," DF")

  datasum$FPcode = cut( datasum$pstatistic
                     , breaks = c( -Inf, 0.001, 0.01, 0.05, Inf)
                     , labels= c("***", "**", "*", "" ) )



  quality=data.frame(
  Statistic=c("Residual Standard Error",
                 "R2",
                 "R2 Adjusted",
                 "F-Statistic",
                 "p-value",
                 "df (p,n-p,p*)"),
     Value=c(as.character(datasum$sigma),
             as.character(datasum$r.squared),
             as.character(datasum$adj.r.squared),
             datasum$FString,
             paste("p =",datasum$pstatistic,datasum$FPcode,sep=" "),
             paste0(datasum$df,sep=" ",collapse = "")),
  stringsAsFactors=FALSE)
  FlexTable(quality,body.par.props = parRight(),header.text.props = textBold())
}


FlexTable.gc <- function(datagc,xnames) {
  n = nrow(datagc)
  c0=round(as.vector(datagc),3)
  data= data.frame(dimnames(koala.min.coef)[1], Vals=c0)
  ftable = FlexTable(data,header.columns=FALSE,add.rownames = FALSE,
            body.par.props = parRight(),
            header.text.props = textBold())
  ftable = addHeaderRow( ftable, value = c("", "Value") )


}

# create a FlexTable of Descriptive Statistics ###
FlexTable.desc <- function(datain){
  dataIn.desc.df <- as.data.frame(describe(datain,num.desc=c("valid.n","mean","sd","min","max"))$Numeric)
  dataIn.desc.df.t <-t(dataIn.desc.df)

  ftable = FlexTable(dataIn.desc.df.t,header.columns=FALSE,add.rownames = TRUE,
                   body.par.props = parRight(),
                   header.text.props = textBold())
  ftable = addHeaderRow( ftable, value = c("", "N", "Mean", "Std Dev", "Min", "Max") )

  sirt = ftable
}

# Prepare Descriptive Satistics FlexTable
dataIn.desc.ft=FlexTable.desc(df)

# FlexTables min for Word Document
koala.min.ft.lm.coef = FlexTable.lm.coef(koala.min.lm)
koala.min.ft.lm.quality = FlexTable.lm.quality(koala.min.lm)
koala.min.ft.lm.predict = FlexTable(koala.min.lm.predict)


################  Prepare to generate MS Word Document ################
#outfileName=gsub(".csv$",".docx",inFileName)
outfileName="~/test.docx"
# set default font size to 10
options( "ReporteRs-fontsize" = 10 )
options( "ReporteRs-default-font" = "Arial" )


#Create a new word document
doc <- docx()
doc = map_title(doc, stylenames = c("Titre1", "Titre2", "Titre3",
                                    "Titre4", "Titre5", "Titre6",
                                    "Titre7", "Titre8", "Titre9" ) )
doc <- addParagraph( doc, "Real Estate Price Prediction with Lasso Regression", stylename = "TitleDoc" )
doc <- addParagraph( doc , paste("Input Data file:",args[1],collapse=" ")  , stylename="Citationintense")

################ Descriptive Statistics ################
doc <- addTitle( doc, value = "Basic summary statistics",level=2)
dataIn.desc.ft<-setFlexTableWidths(dataIn.desc.ft,widths = c(1.5,0.5,1.2,1.2,1.2,1.2))
doc <- addFlexTable( doc, dataIn.desc.ft)

################ Summary Report ################
doc <- addTitle( doc, value = "Results summary report",level=2)
koala.min.ft.lm.quality<-setFlexTableWidths(koala.min.ft.lm.quality,widths = c(2, 3.5))
doc <- addFlexTable( doc, koala.min.ft.lm.quality )
doc <- addPageBreak( doc )

################ PLOT CROSS VALIDATION ################
doc <- addTitle( doc, value = "Leave-One-Out cross-validation",level=2)
plotFunc<-function(){
  par(mar = c(5,5,6,5))
  plot(koala.cv)
  title("Mean Squared Error by Log of Lambda \nand Number of Non-Zero Coefficients (top axis)", adj = 0.5, line = 4)
}

doc <- addPlot(doc, plotFunc)

doc <- addParagraph( doc, value = "The cross-validation curve is shown as a red dotted line, with error bars representing upper and lower standard deviation curves along the Lambda sequence. The lowest point in the curve indicates the optimal lambda: the log value of lambda that best minimised the error in cross-validation.",
                                  stylename = "rPlotLegend")

doc <- addParagraph( doc, paste("\nFrom the above graph Lambda Minimum = ",koala.min,collapse=" "),stylename = "Normal")

################ REGRESSION EQUATION ################
doc <- addTitle( doc, "Lambda minimum regression equation", level =  2 )
doc <- addParagraph( doc, value = (koala.min.formula),stylename = "Normal")

################ Lasso Reg Coefficients ################
## Coefficients at the value of λ that gives minimum mean cross-validated error
## Linear regression with Lasso after leave-one-out cross-validation
doc <- addTitle( doc, "Lasso regression coefficients at Lambda minimum", level =  2 )
FTablegc<-FlexTable.gc(koala.min.coef,wombat.x.names)
FTablegc <- setFlexTableWidths(FTablegc,widths = c(1.5, 1.5))
doc <- addFlexTable(doc,FTablegc)
#doc <- addFlexTable( doc, koala.1se.ft.lm.coef )

################ Lin Reg Coefficients ################
doc <- addTitle( doc, "Linear regression coefficients at Lambda minimum", level =  2 )
koala.min.ft.lm.coef<-setFlexTableWidths(koala.min.ft.lm.coef,widths = c(1.5, 1.5, 1.0, 1.0, 1.0, 1.0))
doc <- addFlexTable( doc, koala.min.ft.lm.coef )



################ ANOVA ################
doc <- addTitle( doc, "Analysis of variance", level =  2 )
koala.min.ft.aov <- FlexTable(xtable(anova(koala.min.lm)),
                              add.rownames = TRUE,
                              body.par.props = parRight(),
                              header.text.props = textBold())
koala.min.ft.aov <- setFlexTableWidths(koala.min.ft.aov,widths = c(1.3, 0.5, 1.5, 1.5, 1.0, 1.3))
doc <- addFlexTable( doc, koala.min.ft.aov)

################ MODEL PREDICTION ################
doc <- addTitle( doc, value = "Predicted values",level=2)
koala.min.ft.lm.predict<-setFlexTableWidths(koala.min.ft.lm.predict,widths = c(1.5, 1.5, 1.5))
doc <- addFlexTable( doc, koala.min.ft.lm.predict )



# write the doc
writeDoc( doc, file = outfileName )

