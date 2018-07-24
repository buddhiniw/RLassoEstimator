Real Estate Price Prediction Using ElasticNet Regression
=========================================================
A tool used for real estate price prediction using elastic net regression
Reference - "Regularization and variable selection via the
elastic net"Fitting the Lasso Estimator using R" by Hui Zou and Trevor Hastie

J. R. Statist. Soc. B (2005) 67 , Part 2 , pp. 301–320

R code author - B. Waidyawansa
Date - 04-30-2018

Usage :
 Rscript BEDROCK-ELASTICNET_LARS.R <location/data_file_name.csv>

NOTE - No data per-processing done inside this script. All provided
data files are assumed to be CLEAN and in a clear format with the first
column being the responce variable and the rest of the columns being the 
predictors variables. The last row is assumed to contain a test dataset that
can be used to test the final model. 
The data file should be of type *.csv.

Program Files:

BEDROCK-ELASTICNET-LARS.R - Contains the main analysis code
BEDROCK-DOC.R - Contains the document writing functions
BEDROCK-HELP-FUNC.R - Contains all the functions used for analysis and doccument writing

The program folder contains the program files and two directories /data and /out.
When running the script from the command line, the script assumes it is being 
executed from the working directory. Therefore it looks for the data file in the 
/data folder and saves the output in the /out folder. 
 
Program Description:

This program performs the ElasticNet regression on the provided real estate data assuming the
data files are clean and in a format described above under NOTE section.

The basic flow of the pogram is-
1. Read the datafile in to a dataframe
2. Output descriptive statistics from unscaled data.
3. Check for correlations between predictors.
4. Standardize predictor variables (assumes the provided data are not scaled)
                   Standardizing/scaling the predictor variables prior to fitting the model will ensure
                   the elasticnet penalization will treat different scale explanatory variables on a more 
                   equal footing. 
                  BUT only continuous predictors needs to be standardized.
                   DONOT scale factor variables (using 1/0) used for yes/no situations.

5. Standardize response variables
6. Separate the train data from the test data.
7. Train the elasticnet model using caret package to find the optimum s and lambda which gives the minimum RMSE under LeaveOneOutCross-Vallidation procedure.
8. Plot the LOOCV plot
9. Plot variable importance plot

10. Get the best model and the tuning parameters.
11. Get the model coefficient for the best model.
12. Print the model coefficient from the best model
13. Make predictions using final model.
14. Print the model prediction.

 The program summary is saved into a *.docx files inside the /out folder for future reference.