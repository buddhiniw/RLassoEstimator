#############################
# Helper functions to create Tables For MS Word .docx
#############################
#  Author by B. Waidyawansa (04/30/2018)


library(flextable)

#r2 <- fit$glmnet.fit$dev.ratio[which(fitnet$glmnet.fit$lambda == fitnet$lambda.min)]

# create a FlexTable of Descriptive Statistics ###
FlexTable.descriptive <- function(datain){
  data <- as.data.frame(describe(datain,num.desc=c("valid.n","mean","sd","min","max"))$Numeric)
  data.t <- as.matrix(t(data))
  data.t <- formatC(data.t, digits = 2, format = "d", flag = "0")
  
  ftable = FlexTable(data.t,header.columns=FALSE,add.rownames = TRUE,
                     body.par.props = parRight(),
                     header.text.props = textBold())
  ftable = addHeaderRow( ftable, value = c("", "N", "Mean", "Std Dev", "Min", "Max") )
  
  sirt = ftable
}

# FlexTable of coefficients from glmnet and ols
FlexTable.coef <- function(beta.glmnet, beta.lm) {
  data <- as.matrix(cbind(beta.glmnet,beta.lm[,1:2]))

  # format the data values
  data <- formatC(data, digits = 2, format = "f", flag = "0")
  
  # create an empty FlexTable
  ftable <- FlexTable(data, header.columns=FALSE,add.rownames = TRUE,
                     body.par.props = parRight(),
                     header.text.props = textBold())
  ftable <- addHeaderRow( ftable, value = c("Predictor", "Elastic Net","Least Squares"), colspan = c(1,1,2) )
  ftable <- addHeaderRow( ftable, value = c("", "Coefficient","Coefficient","STD error"))
  
  ftable <- setFlexTableWidths(ftable,widths = c(1.5, 1.5, 1.5,1.5))
  ftable
}

# create flextable of coefficients from enet ith errors from bootstrap
FlexTable.enet.coef.err <- function(beta.hat.scaled, boot.samples) {
  capture.output(boot.samples, file = "boot_out.txt")
  dat <- read.table("boot_out.txt", skip=10)
  
  ## replace 0 coef values with N/A to indicate those variables are not selected
  #dat[dat == 0] <- NA
  ## Get the coef values and RMSE from bootstrap output
  df<- as.data.frame(dat)[,c("V4")]
  ## Set RMSE for NA vaiables NA
  #df[!complete.cases(df),] <- NA

  data <- as.matrix(cbind(beta.hat.scaled$coefficients, as.matrix(df)))
  
  # format the data values
  data <- formatC(data, digits = 3, format = "f", flag = "0")
  
  # create an empty FlexTable
  ftable <- FlexTable(data, header.columns=FALSE,add.rownames = TRUE,
                      body.par.props = parRight(),
                      header.text.props = textBold())
  #ftable <- addHeaderRow( ftable, value = c("Variable", "Estimate"))
  ftable <- addHeaderRow( ftable, value = c("Variable","Coefficient", "Std. Error"))
  ftable <- setFlexTableWidths(ftable,widths = c(2, 1.5, 1.5))
  ftable
}

# create flextable of coefficients from enet without errors
FlexTable.enet.coef <- function(beta.hat.scaled) {
  
  data <- as.matrix(beta.hat.scaled$coefficients)
  
  # format the data values
  data <- formatC(data, digits = 3, format = "f", flag = "0")
  
  # create an empty FlexTable
  ftable <- FlexTable(data, header.columns=FALSE,add.rownames = TRUE,
                      body.par.props = parRight(),
                      header.text.props = textBold())
  ftable <- addHeaderRow( ftable, value = c("Variable", "Estimate"))
  ftable <- setFlexTableWidths(ftable,widths = c(2, 1.5))
  ftable
}


# create a FlexTable of ANOVA results for 2 model comparision
FlexTable.2mod.anova <- function(mod1,mod2){
  #assuming mod1 is a subset of mod2
  comp.models <- anova(mod1,mod2)
  data <- as.matrix(comp.models)
  ftable <- FlexTable(data, add.rownames = TRUE, body.par.props = parRight(),
                      header.text.props = textBold())
  ftable <- setFlexTableWidths(ftable,widths = c(1.3, 0.5, 1.0, 1.0, 1.0, 1.0,1.0))
                                
}

# create flextable for single model anova results
FlexTable.anova <- function(model){
  data <- as.matrix(anova(model))
  # format the data values
  data <- formatC(data, digits = 3, format = "f", flag = "0")
  # format the data values
  #data[, 2] <- formatC( data[, 2], digits=0, format = "d")
  #data[, 2] <- formatC( data[, 2], digits=2, format = "d")
  #data[, 3] <- formatC( data[, 3], digits=3, format = "f")
  
  ftable <- FlexTable(data,add.rownames = TRUE,body.par.props = parRight(),
                      header.text.props = textBold())
  ftable <- setFlexTableWidths(ftable,widths = c(1.3, 1.0, 1.0, 1.0, 1.0, 1.0))
  
}

# create a FlexTable of predicted value and prediction error
FlexTable.predict <- function(value,error, r2){
  data <- as.matrix(cbind(value,error,r2))
  data <- formatC(data, digits = 3, format = "f", flag = "0")
  ftable <- FlexTable(data,header.columns=FALSE, add.rownames = FALSE,body.par.props = parRight(),
                      header.text.props = textBold())
  ftable <- addHeaderRow( ftable, value = c("Predicted Value", "Prediction Error", "R2"))
  ftable <- setFlexTableWidths(ftable,widths = c(1.5,1.5,1.5))
  
  
}

# create a FlexTable of quality statistics from a Linear Model
FlexTable.lm.quality <- function(mod.ols) {
  datasum = summary(mod.ols)
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
    ftable <- FlexTable(quality,body.par.props = parRight(),header.text.props = textBold())
    ftable <- setFlexTableWidths(ftable,widths = c(2, 3.5))
}


