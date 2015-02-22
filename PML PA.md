## Practical Machine Learning - Peer Assignment 

### Data Processing

Load the csv file from working directory:


```r
data <- read.csv("C:\\Users\\Wu\\Documents\\pml-training.csv", 
                 stringsAsFactors=FALSE)
```

A function is coded to check the number of missing values in each columns:


```r
check_missing <- function(input) {
    col_types <- as.vector(sapply(input, class))
    col_num <- length(col_types)
    col_missing_cnt <- rep(col_num, 0)
    for (i in 1:col_num) {
        if (col_types[i] == "character") {
            col_missing_cnt[i] <- 
              length(input[is.na(input[,i]) | input[,i] == "",i])
        }
        else {
            col_missing_cnt[i] <- length(input[is.na(input[,i]), i])
        }
    }
    col_missing_cnt
}

col_missing <- check_missing(data)
#col_missing
```

As many columns see 90% of the values missing, only those with no missing values are kept for modelling.


```r
data_mdl <- data[, col_missing == 0]
#names(data_mdl)
```

Among the 60 variables left, the target (**classe**) should not be determined by inputs such as **user_name** and timestamp variables, so I chose to remove those input variables as well.


```r
data_mdl <- data_mdl[8:length(data_mdl)]
dim(data_mdl)
```

```
## [1] 19622    53
```

### Model Build 

The method recommended was Random Forest but it took really long to run for the first trial. So I first reduce the # of columns by using caret package's principal component analysis function.


```r
library(caret)
prComp <- prcomp(data_mdl[,1:52])
sum(prComp$sdev[1:25])/sum(prComp$sdev)
```

```
## [1] 0.962032 
```

As the data shows, the first 25 principal components account for >95% of total standard deviations. So I proceeded to divide data into training and testing datasets and apply PCA and Random Forest on the training sample.

```r
# Data Partition into Training and Testing samples
inTrain = createDataPartition(y=data_mdl$classe, p=0.7, list=FALSE)
training = data_mdl[inTrain, ]; testing = data_mdl[-inTrain, ]
# Principal COmponent Analysis
preProc <- preProcess(training[,1:52], method = "pca", pcaComp = 25)
trainPC <- predict(preProc, training[,1:52])
trainPC$classe <- as.factor(training$classe)
# Random Forest Training
require(randomForest)
modFit <- train(classe ~ ., method = "rf", data = trainPC)
# Apply PCA and RF model on the testing output
table(predict(modFit, predict(preProc, testing[,1:52])), testing$classe)
```

```
##    
##        A    B    C    D    E
##   A 1674    0    0    0    0
##   B    0 1139    0    0    0
##   C    0    0 1026    0    0
##   D    0    0    0  965    0
##   E    0    0    0    0 1082
```

The cross validation on testing sample turns out to be 100% accuracy.  

Finally, the PCA transform + model were applied on test data:

```r
data <- read.csv("C:\\Users\\Wu\\Documents\\pml-testing.csv", 
                 stringsAsFactors=FALSE)
test_mdl <- test[, col_missing == 0]
test_mdl <- test_mdl[,8:59]
predict(modFit, predict(preProc, test_mdl[,1:52]))
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E

```