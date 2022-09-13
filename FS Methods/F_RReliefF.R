# RReliefF filter
# Please read README.md for further explanations 

# SECTION 1/5: REQUIRED LIBRARIES
.libPaths("D:/Users/XYZ") #set to path where your R libraries are stored
library(FSelector)
library(data.table)
library(caret)

# SECTION 2/5: GLOBAL VARIABLES (BASIC SETUP)
dataFolder <- "C:/Users/XYZ"
files <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16")
iter <- 5
response <- "Response"
resultCon <- list()

# SECTION 3/5: AUXILIARY FUNCTIONS
# a) reads dataset-file from .csv and transforms categorial values into dummy variables
# NOTE: in order for dummyVars to work, categorial values have to be casted as 'factor' first
getDummifiedData <- function (file) {
  for (i in 1:ncol(file)) {
    if (!is.numeric(file[1, i])) {
      file[[i]]<-as.factor(file[[i]])
    }
  }
  dummyBuilder <- dummyVars(~ ., data = file, sep = "_", fullRank = TRUE)
  dummy <- as.data.frame(predict(dummyBuilder, file))
  return(FResult=dummy)
}

# b) returns a list object that contains the names of all relevant and all irrelevant variables of a dataset. 
# Relevant variables are those that are used in the formula to calculate the measure in column "Response". 
getRelevanceInfo <- function(file) {
  if (ncol(file) > 11) {
    REL <- c("V01", "V02", "V03", "V06", "V08", "V12", "V13", "V14", "V15", "V19")
    IRR <- c("V04", "V05", "V07", "V09", "V10", "V11", "V16", "V17", "V18", "V20")
  } else {
    REL <- c("V01", "V02", "V03", "V06", "V08")
    IRR <- c("V04", "V05", "V07", "V09", "V10")
  }
  FResult <- list(REL=REL, IRR=IRR)
  return(FResult)
}

# c) Implementation of success measure Suc, according to [BOLO-13]
Suc <- function (bestFeatures, file) {
  relInfo <- getRelevanceInfo(file)
  REL <- relInfo$REL
  IRR <- relInfo$IRR
  RELt <- length(REL)
  IRRt <- length(IRR)
  RELs <- 0
  IRRs <- 0
  
  # try-Catch block prevents code from termination in case that some feature values from 
  # the dataset are not present in the training data (EXP: see DS13 with no V06_M3)
  for (i in REL) {
    tryCatch(            
      expr = {ifelse(bestFeatures[1, i] == 1, RELs <- RELs + 1, RELs <- RELs)},
      error = function(e) {}
    )
  }
  
  for (i in IRR) {
    tryCatch(
      expr = {ifelse(bestFeatures[1, i] == 1, IRRs <- IRRs + 1, IRRs <- IRRs)},
      error = function(e) {}
    )
    
  }
  
  alpha <- min(0.5, RELt/IRRt)
  Suc <- RELs/RELt-alpha*(IRRs/IRRt)
  return(Suc)
}

# d) Implementation of Stability Measures, according to [NOGU-18]
calcStability <- function(stabilityMatrix, significance) {
  # 01_stability matrix properties:
  #general
  M <- nrow(stabilityMatrix)
  d <- ncol(stabilityMatrix)
  meank <- sum(stabilityMatrix)/M
  alpha <- significance
  #featureWise (vertical)
  pfVector <- apply(stabilityMatrix, MARGIN=2, mean)
  sfVector <- sapply(pfVector, FUN = function(pf) {M/(M-1)*pf*(1-pf)})
  #setWise (horizontal)
  kiVector <- apply(stabilityMatrix, MARGIN = 1, sum)
  
  # 02_calculation
  # 02.1_stability estimator
  stabilityEstimator <- 1-(mean(sfVector)/((meank/d)*(1-(meank/d))))
  
  # 02.2_confidence intervals
  # Phi(i)
  Phi_iVector <- c()
  for (i in 1:M) {
    firstTerm <- (1/((meank/d)*(1-(meank/d))))
    
    secondTermSum <- 0
    for (f in 1:d) {
      secondTermSum <- secondTermSum + (stabilityMatrix[i,f]*pfVector[f])
    }
    secondTerm <- secondTermSum/d
    
    thirdTerm <- (kiVector[i]*meank)/(d^2)
    
    fourthTerm <- (stabilityEstimator/2)*((2*meank*kiVector[i])/(d^2) - (kiVector[i]/d) - (meank/d) + 1)
    
    Phi_iVector[i] <- firstTerm*(secondTerm-thirdTerm+fourthTerm)
  }
  #Phi(.)
  Phi_sum <- 0
  for (i in 1:M) {
    Phi_sum <- Phi_sum + Phi_iVector[i]
  }
  Phi_mean <- Phi_sum/M
  #variance Estimator
  varianceEstimatorSum <- 0
  for (i in 1:M) {
    varianceEstimatorSum <- varianceEstimatorSum + (Phi_iVector[i]-Phi_mean)^2
  }
  stabilityVarianceEstimator <- (4/(M^2))*varianceEstimatorSum
  #confidence Intervals
  confInterval_lowerBound <- stabilityEstimator - qnorm(1-alpha/2) * stabilityVarianceEstimator
  confInterval_upperBound <- stabilityEstimator + qnorm(1-alpha/2) * stabilityVarianceEstimator
  confInterval <- data.frame(lowerBound = confInterval_lowerBound, upperBound = confInterval_upperBound)
  #scale To 0 To 1 (LBo - LowerBoundOld, UBo - UpperBoundOld, LBn - LowerBoundNew, UPn - UpperBoundNew)
  scalingParameters <- list(LBo = -(1/(M-1)), UBo = 1, LBn = 0, UBn = 1)
  attach(scalingParameters)
  confInterval[2,] <- apply(confInterval[1,], MARGIN = 2, FUN = function(x) {LBn+((x-LBo)/(UBo-LBo))*(UBn-LBn)})
  stabilityEstimatorScaled <- LBn+((stabilityEstimator-LBo)/(UBo-LBo))*(UBn-LBn)
  detach(scalingParameters)
  rownames(confInterval) <- c("original", "scaled")
  FResult <- list(stabilityEstimator = stabilityEstimator, stabilityEstimatorScaled = stabilityEstimatorScaled,
                  confidenceInterval = confInterval,
                  lowerBound = confInterval[1,1], upperBound = confInterval[1,2],
                  lowerBoundScaled = confInterval[2,1], upperBoundScaled = confInterval[2,2],
                  significance = alpha)
  return(FResult)
}

# d) defines threshold for RReliefR results and returns a dataframe that marks which features have been selected (1) according to the threshold
selectThreshold <- function(weights, threshold) {
  for (i in 1:ncol(weights)) {
    if (weights[1,i] >= threshold) {
      weights[1,i] = 1
    }
    else {
      weights[1,i] = 0
    }
  }
  return(bestFeatures = weights)
}

# e) RReliefR filter implementation using FSelector from Romanski, P.; Kotthoff, L. (2018):
FSCalculationRelief <- function() {
  featureValues <- as.data.frame(t(relief(Response~., file, neighbours.count = 5, sample.size = 0.9*nrow(file))))
  bestFeatures <- selectThreshold(featureValues, 0.005)
  Suc <- Suc(bestFeatures, file)
  FResults <- list(featureValues=featureValues, bestFeatures=bestFeatures, Suc=Suc)
  return(FResults)
}

# SECTION 4/5: CODE EXECUTION (MAIN)
# Manages execution order of auxiliary functions in order to run the FS method on multiple datasets multiple times 
# and to assure a standardized format of the results
for (f in files) {
  file <- read.csv(file.path(paste0(dataFolder, f, ".csv")))
  resultCon[[f]]<- list(bestFeatures = data.frame(), featureValues = data.frame(), Suc = data.frame())
  for (i in 1:iter) { 
    startTime <- Sys.time()
    results <- FSCalculationRelief()
    resultCon[[f]][["bestFeatures"]] <- rbind(resultCon[[f]][["bestFeatures"]], results$bestFeatures)
    resultCon[[f]][["featureValues"]] <- rbind(resultCon[[f]][["featureValues"]], results$featureValues)
    resultCon[[f]][["Suc"]] <- rbind(resultCon[[f]][["Suc"]], results$Suc)
    names(resultCon[[f]][["Suc"]]) <- "Suc"
    resultCon[[f]][["Merged"]] <- cbind(resultCon[[f]][["bestFeatures"]], resultCon[[f]][["Suc"]])
    endTime <- Sys.time()
    timeTaken <- endTime-startTime
    print(paste("done:", f, i, "///// (total) time taken:", timeTaken))
  }
}

# SECTION 5/5: RESULTS: Extraction of relevant information from result container 'resultCon')
# a) entire result container
resultCon

# b) selected features and Suc in all runs on all selected datasets
results <- list()
for (f in files) {
  results[[f]] <- resultCon[[f]][["Merged"]]
}
results

# c) stability measures
stabilityInd <- list()
for (f in files) {
  stabilityMatrix <- resultCon[[f]][["Merged"]][,!(names(resultCon[[f]][["Merged"]]) %in% c("Suc"))]
  significance <- 0.05
  stabilityInd[[f]] <- calcStability(stabilityMatrix, significance)
}
stabilityInd
