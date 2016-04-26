# Machine Learning
J Faleiro  
April 19, 2015  

# Required libraries


```r
if (!require("pacman")) install.packages("pacman")
pacman::p_load(caret, e1071, kernlab, ggplot2)
```


# Prediction

## Building a predictor

Steps:

1. Question
2. Input data
3. Features
4. Algorithm
5. Parameters
6. Evaluation

Spam example:


```r
library(kernlab)
data(spam)
head(spam$your)
```

```
## [1] 0.96 1.59 0.51 0.31 0.31 0.00
```

Density distributions of word 'your' in spam and non-spam emails


```r
plot(density(spam$your[spam$type=='nonspam']), 
     col='blue', main='', xlab="frequency of word 'your'")
lines(density(spam$your[spam$type=='spam']), col='red')
```

![](index_files/figure-html/unnamed-chunk-3-1.png)

* what blue shows: a lot of non-spam emails have close to zero 'your'
* what red shows: a lot of spam emails have from 0 to 4, some with 8, words 'your'

Algorithm. put a cutoff threshold, right after the peak of blue non-spam emails. Anything having more 'your' word above that, is spam, not spam otherwise.


```r
prediction <- ifelse(spam$your > 0.5, 'spam', 'nonspam')
table(prediction, spam$type)/length(spam$type)
```

```
##           
## prediction   nonspam      spam
##    nonspam 0.4590306 0.1017170
##    spam    0.1469246 0.2923278
```

Meaning, with this simple algorithm you will be right about spam detection approximatelly 75% of the time:


```r
0.4590306 + 0.2923278
```

```
## [1] 0.7513584
```

## In sample error vs out of sample errors

Building a predictor in terms of average number of capital letters (`spam$capitalAve`):


```r
library(kernlab)
data(spam)
set.seed(333)
smallSpam <- spam[sample(dim(spam)[1], size=10),]
spamLabel <- (smallSpam$type=='spam')*1 + 1
plot(smallSpam$capitalAve, col=spamLabel)
```

![](index_files/figure-html/unnamed-chunk-6-1.png)

We will build a predictor that separate red dots (spam) from black dots (ham) based on the average of capital letters:


```r
rule1 <- function(x) {
    prediction <- rep(NA, length(x))
    prediction[x > 2.7] <- 'spam'
    prediction[x < 2.4] <- 'nonspam'
    prediction[x >= 2.4 & x <= 2.45] <- 'spam'
    prediction[x > 2.45 & x <= 2.70] <- 'nonspam'
    return(prediction)
}
table(rule1(smallSpam$capitalAve), smallSpam$type)
```

```
##          
##           nonspam spam
##   nonspam       5    0
##   spam          0    5
```

100% correct, in sample error is zero


```r
rule2 <- function(x) {
    prediction <- rep(NA, length(x))
    prediction[x > 2.8] <- 'spam'
    prediction[x <= 2.8] <- 'nonspam'
    return(prediction)
}
table(rule2(smallSpam$capitalAve), smallSpam$type)
```

```
##          
##           nonspam spam
##   nonspam       5    1
##   spam          0    4
```

About 90% correct, in sample error is 10%

What if we apply to all data?


```r
table(rule1(spam$capitalAve), spam$type)
```

```
##          
##           nonspam spam
##   nonspam    2141  588
##   spam        647 1225
```

The out-sample error increased from 0 to 26.8419909% - this is an example of **overfitting**  - the rules were defined too tightly to the sample dataset.


```r
table(rule2(spam$capitalAve), spam$type)
```

```
##          
##           nonspam spam
##   nonspam    2224  642
##   spam        564 1171
```

How many times were we right for each rule (accuracy)?


```r
c(sum(rule1(spam$capitalAve) == spam$type),
  sum(rule2(spam$capitalAve) == spam$type))
```

```
## [1] 3366 3395
```

## Types of Errors

Suppose that we have created a machine learning algorithm that predicts whether a link will be clicked with 99% sensitivity and 99% specificity. The rate the link is clicked is 1/1000 of visits to a website. If we predict the link will be clicked on a specific visit, what is the probability it will actually be clicked?


```r
sensitivity <- specificity <- 0.99
population <- 100000 
rateClick <- 1/1000
```

$sensitivity = \frac {TP}{(TP + FN)}$


```r
FN <- 1
TP <- FN*sensitivity*100
```

$population = TP + FN + FP + TN$

$FP + TN = population - (FN + TP)$


```r
FPplusTN <- population - (FN + TP)
```

$specificity = \frac {TN}{(TN + FP)}$


```r
TN <- specificity * FPplusTN
FP <- population - (FN + TP + TN)
```


```r
c(TP, FP)
```

```
## [1]  99 999
```

```r
c(FN, TN)
```

```
## [1]     1 98901
```

Positive predictive value is the probablity the link will be clicked:

$PPV = \frac {TP}{(TP + FP)}$


```r
TP/(TP+FP)
```

```
## [1] 0.09016393
```

i.e. ~ **9.01%**

# Caret Package

## SPAM example

### Data Splitting

#### Partitioning

How to create a partition of training/test data, using 75% for training and 23% for testing, for an outcome `spam$type`:


```r
library(caret); library(e1071); library(kernlab)
data(spam)
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)
```

`inTrain` has indexes of all items selected to be in the training data set, so we subset `training` and `testing`.


```r
training <- spam[inTrain,]
testing <- spam[-inTrain,]
dim(training)
```

```
## [1] 3451   58
```

Now, `training` has all index of selected in `inTrain` and `testing` all that is `-inTrain` (not in).

#### K-Fold 

Splitting with K-folds, we pass the outcome, number of folds we want to create. We want each fold to be a list, and to return the training set.


```r
set.seed(32323)
folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain=TRUE)
sapply(folds, length)
```

```
## Fold01 Fold02 Fold03 Fold04 Fold05 Fold06 Fold07 Fold08 Fold09 Fold10 
##   4141   4140   4141   4142   4140   4142   4141   4141   4140   4141
```

The size of each fold is ~ 4141.

And here's how we look at samples in fold one:


```r
folds[[1]][1:10]
```

```
##  [1]  1  2  3  4  5  6  7  8  9 10
```

We can also returns the test samples in each fold (`returnTrain=FALSE`):


```r
set.seed(32323)
folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain=FALSE)
sapply(folds, length)
```

```
## Fold01 Fold02 Fold03 Fold04 Fold05 Fold06 Fold07 Fold08 Fold09 Fold10 
##    460    461    460    459    461    459    460    460    461    460
```

The size of each fold is ~ 461 (smaller than training, they are split in a 75/25 proportion).

And here's how we look at samples in fold one:


```r
folds[[1]][1:10]
```

```
##  [1] 24 27 32 40 41 43 55 58 63 68
```

#### Resampling


```r
set.seed(32323)
folds <- createResample(y=spam$type, times=10, list=TRUE)
sapply(folds, length)
```

```
## Resample01 Resample02 Resample03 Resample04 Resample05 Resample06 
##       4601       4601       4601       4601       4601       4601 
## Resample07 Resample08 Resample09 Resample10 
##       4601       4601       4601       4601
```

The size of each fold is ~ 4601.

And here's how we look at samples in fold one:


```r
folds[[1]][1:10]
```

```
##  [1]  1  2  3  3  3  5  5  7  8 12
```

Since we are resampling, we are seeing some og the items repeated in a fold.

#### Time Slices

Simple random sampling of time series is probably not the best way to resample times series data. [Hyndman and Athanasopoulos (2013)](https://www.otexts.org/fpp/2/5) discuss rolling forecasting origin techniques that move the training and test sets in time. 


```r
set.seed(32323)
tme <- 1:1000
folds <- createTimeSlices(y=tme, initialWindow=20, horizon=10)
names(folds)
```

```
## [1] "train" "test"
```



```r
folds$train[[1]]
```

```
##  [1]  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
```

We have 20 values (matching `initialWindow`)


```r
folds$test[[1]]
```

```
##  [1] 21 22 23 24 25 26 27 28 29 30
```

We have 10 values (matching `horizon`)

### Fit a Model 

Now let's predict `type` using all the other features in the `training` dataset we created before, using a generalized linear model, glm, as a method:


```r
set.seed(32343)
modelFit <- train(type ~ ., data=training, method='glm')
modelFit
```

```
## Generalized Linear Model 
## 
## 3451 samples
##   57 predictor
##    2 classes: 'nonspam', 'spam' 
## 
## No pre-processing
## Resampling: Bootstrapped (25 reps) 
## Summary of sample sizes: 3451, 3451, 3451, 3451, 3451, 3451, ... 
## Resampling results
## 
##   Accuracy   Kappa      Accuracy SD  Kappa SD  
##   0.9160009  0.8231214  0.005584604  0.01184724
## 
## 
```

As you can see, 57 predictors, 1 outcome (class nonspam/spam) and 3451 samples match the results from `dim(training)` (3451 rows and 58 columns). Some additional information, like the resampling method, in this case `Bootstrapped` (random sampling with replacement, aka bootstraping), with 25 repetitions.

Resampling brought accuracy to about **91.6%**.

### Final Model

To take a peek of the model, in this case, coefficients between class (spam/nospam) and each of the 57 predictors:


```r
modelFit$finalModel
```

```
## 
## Call:  NULL
## 
## Coefficients:
##       (Intercept)               make            address  
##        -1.438e+00         -1.839e-01         -1.489e-01  
##               all              num3d                our  
##         1.175e-01          2.480e+00          5.420e-01  
##              over             remove           internet  
##         9.038e-01          2.457e+00          4.973e-01  
##             order               mail            receive  
##         5.102e-01          1.053e-01         -5.919e-01  
##              will             people             report  
##        -1.932e-01         -2.159e-01          2.981e-01  
##         addresses               free           business  
##         8.834e-01          8.883e-01          9.754e-01  
##             email                you             credit  
##         1.735e-01          7.511e-02          9.960e-01  
##              your               font             num000  
##         2.553e-01          1.483e-01          1.911e+00  
##             money                 hp                hpl  
##         6.091e-01         -1.837e+00         -8.798e-01  
##            george             num650                lab  
##        -1.179e+01          4.687e-01         -2.362e+00  
##              labs             telnet             num857  
##        -3.192e-01         -1.544e-01          1.026e+00  
##              data             num415              num85  
##        -8.858e-01          5.891e-01         -2.025e+00  
##        technology            num1999              parts  
##         9.146e-01          3.811e-02          4.856e-01  
##                pm             direct                 cs  
##        -8.030e-01         -4.246e-01         -5.553e+02  
##           meeting           original            project  
##        -2.624e+00         -1.211e+00         -2.089e+00  
##                re                edu              table  
##        -7.711e-01         -1.383e+00         -2.202e+00  
##        conference      charSemicolon   charRoundbracket  
##        -3.981e+00         -1.174e+00         -1.180e-01  
## charSquarebracket    charExclamation         charDollar  
##        -4.938e-01          2.642e-01          5.037e+00  
##          charHash         capitalAve        capitalLong  
##         2.437e+00          3.563e-03          1.021e-02  
##      capitalTotal  
##         8.545e-04  
## 
## Degrees of Freedom: 3450 Total (i.e. Null);  3393 Residual
## Null Deviance:	    4628 
## Residual Deviance: 1408 	AIC: 1524
```

### Prediction

To predict if items on dataset `testing` belong to any of the classes spam/nospam:


```r
predictions <- predict(modelFit, newdata=testing)
head(predictions)
```

```
## [1] spam    spam    spam    spam    nonspam spam   
## Levels: nonspam spam
```

### Confusion Matrix

To validade how close your predictions were to the class of the testing dataset, we generate a confusion matrix:


```r
confusionMatrix(predictions, testing$type)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction nonspam spam
##    nonspam     659   36
##    spam         38  417
##                                           
##                Accuracy : 0.9357          
##                  95% CI : (0.9199, 0.9491)
##     No Information Rate : 0.6061          
##     P-Value [Acc > NIR] : <2e-16          
##                                           
##                   Kappa : 0.8653          
##  Mcnemar's Test P-Value : 0.9075          
##                                           
##             Sensitivity : 0.9455          
##             Specificity : 0.9205          
##          Pos Pred Value : 0.9482          
##          Neg Pred Value : 0.9165          
##              Prevalence : 0.6061          
##          Detection Rate : 0.5730          
##    Detection Prevalence : 0.6043          
##       Balanced Accuracy : 0.9330          
##                                           
##        'Positive' Class : nonspam         
## 
```

The reference gives us some idea of hits/misses, as well as others as accuracy, specificity, sensitivity, confidence intervals and Kappa values.
