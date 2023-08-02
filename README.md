# Feature-Selection-For-Measurement-Models_


This is the code and the data set necessary to reproduce our results from the article Feature selection for measurement models in the International Journal for Quality and Reliability Management.


Authors: Mueller, Tobias; Segin, Alexander; Weigand, Christoph; Schmitt, Robert

Chair of Production Metrology and Quality Management at the Laboratory for Machine Tools and Production Engineering (WZL) of RWTH Aachen University - Campus Boulevard 30, 52074 Aachen, Germany

Please cite this code or the data set as:  Mueller, T., Segin, A., Weigand, C. and Schmitt, R.H. (2023), "Feature selection for measurement models", International Journal of Quality & Reliability Management, Vol. 40 No. 3, pp. 777-800. https://doi.org/10.1108/IJQRM-07-2021-0245

---

## 1. Requirements
All the code was run on Windows 10, R 4.0.2 and RStudio 1.3.1073.
Please note that parts of the code are primarily tailored for data delivered in a form that complies with the tested datasets. Other datasets might require minor changes.

## 2. Goal
The goal of the FS method implementations is to unify the application of different FS methods on the artificial datasets and return the respective results in a standardized format. The results can be used to easily compare the results of different FS methods on the artificial datasets. 

## 3. Usage
The code is divided in 5 main sections. Sections 2, 3 and 4 contain functions whose parameters can be used to alter the behavior of the FS methods. In the following, all sections will be explained more thoroughly:

**Section 1: Required Libraries**

This Section contains all packages that will be used to perform the calculation of the FS method.

**Section 2: Global Variables**

This Section defines global variables that are important throughout the code. 
These variables have to be changed before execution. 

1. `dataFolder`: string holding the file path of the dataset folder
2. `files`: vector of strings containing the name(s) of desired datasets (without .csv datatype ending)
3. `iter`: int to specify the desired number of iterations/repetitions of one FS method on one dataset
4. `response`: string holding the name of the target variable/response variable in the datasets
5. `resultCon <- list()`: creation of a container object to store all calculated results in

After these variables have been updated, the code is ready to run.

**Section 3: Auxiliary Functions**

This Section defines all auxiliary functions that are used to calculate the goodness of an FS method and to return the results for all FS methods in a standardized data frame. They include the following functions:

1. `getDummifiedData`: reads dataset-file from .csv and transforms categorial values into dummy variables
2. `getRelevanceInfo`: returns a list object that contains the names of all relevant and all irrelevant variables of a dataset. Relevant variables are those that are used in the formula to calculate the measure in column „Response“. Note that this formula is only valid for the attached datasets and might be changed for other datasets.
3. `partData`: separates dataset into one training set and one testset (usually 70/30 split) using caret‘s createDataPartition
4. `preProc` : normalizes data in the range of [0,1]
5. `DeDummifyResults`: some FS methods require the definition of dummy variables, meaning that some factor variables are split in multiple dummy variables. This function merges all dummy variables of a factor variables back together such that the entire factor variable is labeled 'relevant' if one of the respective dummy variables has been labeled 'relevant'. This results in a better interpretation of results.  (Example: V06_one = 0, V06_two = 0, V06_three = 1 -&gt; V06 = 1)
6. `Suc`: Implementation of Success-Measure _Suc_ according to [BOLO-13]
7. `calcStability`: Implementation of Stability-Measures according to [NOGU-18]
8. `selectThreshold`: some FS methods require the definition of a threshold. For values above that specified threshold, a feature is considered 'relevant', otherwise it is considerered 'irrelevant'. This function defines that threshold and translates the calculated values in a binary dataframe with relevant (1) and irrelevant (0) features
9. `FSCalculation` (actual function name might differ): This function holds the core function that is used to perform the feature selection. Also, it ensures that the results comply with the standardized result format. All relevant parameters that affect the performance of the FS method are contained in this function 

**Section 4: Code Execution (main)**

This Section is the main code and manages the execution order of the necessary auxiliary functions. It consists of two nested for-loops that run the FS method on multiple specified dataset for a specified number of repetitions and ensures a standardized output of the results.

**Section 5: Result Output**

This section returns all relevant results that were calculated. `resultCon` is a result container that contains all calculations, `results` and `stabilityInd` return more formatted information about selected features, *Suc* measures and stability measures.

## 4. Sources
The modelling process necessary for all wrapper and embedded FS approaches is supported by functions from the caret package (Kuhn, M. (2020): caret. Classification and Regression Training. R package version 6.0-86). The interaction of learning function and search function in wrapper approaches is performed by the FSinR package from Aragón-Royón, F.; Jiménez-Vílchez, A.; Arauzo-Azofra, A.; Benitez, J. (2020).
The respective FS core functions are provided by the following authors:

1. CFS filter: *Romanski, P.; Kotthoff, L. (2018): FSelector. Selecting Attributes. R package version 0.31.*
2. mRMR filter: *Kratzer, G.; Furrer, R. (2018): varrank. An R Package for Variable Ranking Based on Mutual Information with Applications to Systems Epidemiology. R package version 0.3.*
3. RReliefF filter: *Romanski, P.; Kotthoff, L. (2018): FSelector. Selecting Attributes. R package version 0.31.*
4. lm/SequentialForwardSelection wrapper: *Kuhn, M. (2020): caret. Classification and Regression Training. R package version 6.0-86* 
5. lm/SequentialBackwardSelection wrapper: *Kuhn, M. (2020): caret. Classification and Regression Training. R package version 6.0-86*
6. ANN/SequentialBackwardSelection wrapper: *Kuhn, M. (2020): caret. Classification and Regression Training. R package version 6.0-86*
7. ANN/Ant Colony Optimization wrapper: *Kuhn, M. (2020): caret. Classification and Regression Training. R package version 6.0-86*
8. LASSO with glm (embedded): *Friedman, J.; Hastie, T.; Tibshirani, R. (2010): Regularization Paths for Generalized Linear Models via Coordinate Descent. Journal of Statistical Software, 33(1), 1-22.*
9. RandomForest (embedded): *Liaw, A.; Wiener, M. (2002): Classification and Regression by randomForest. R News 2(3), 18--22.*

[BOLO-13] Bolón-Canedo, V., Sánchez-Maroño, N. and Alonso-Betanzos, A. (2013), “A review of feature selection methods on synthetic data”, Knowledge and Information Systems, Vol. 34 No. 3, pp. 483–519.

[NOGU-18] Nogueira, S., Sechidis, K. and Brown, G. (2018), “On the stability of feature selection algorithms”, Journal of Machine Learning Research, 2018, pp. 1–54

## 5. License

Copyright 2021 Tobias Mueller - RWTH Aachen Univerity - Laboratory for Machine Tools and Production Engineering WZL of RWTH Aachen

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
