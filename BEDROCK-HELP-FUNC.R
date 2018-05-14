#############################
# Helper functions to create Tables For MS Word .docx
#############################
#  Author by B. Waidyawansa (04/30/2018)


library(flextable)

# create a FlexTable of Descriptive Statistics ###
FlexTable.descriptive <- function(datain){
  dataIn.desc.df <- as.data.frame(describe(datain,num.desc=c("valid.n","mean","sd","min","max"))$Numeric)
  dataIn.desc.df.t <-t(dataIn.desc.df)
  
  ftable = FlexTable(dataIn.desc.df.t,header.columns=FALSE,add.rownames = TRUE,
                     body.par.props = parRight(),
                     header.text.props = textBold())
  ftable = addHeaderRow( ftable, value = c("", "N", "Mean", "Std Dev", "Min", "Max") )
  
  sirt = ftable
}

# create a FlexTable of coeficients from a Linear Model
FlexTable.lm.coef <- function(datalm) {
  print("here")
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

