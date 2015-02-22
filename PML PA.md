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
col_missing
```

```
##   [1]     0     0     0     0     0     0     0     0     0     0     0
##  [12] 19216 19216 19216 19216 19216 19216 19216 19216 19216 19216 19216
##  [23] 19216 19216 19216 19216 19216 19216 19216 19216 19216 19216 19216
##  [34] 19216 19216 19216     0     0     0     0     0     0     0     0
##  [45]     0     0     0     0     0 19216 19216 19216 19216 19216 19216
##  [56] 19216 19216 19216 19216     0     0     0     0     0     0     0
##  [67]     0     0 19216 19216 19216 19216 19216 19216 19216 19216 19216
##  [78] 19216 19216 19216 19216 19216 19216     0     0     0 19216 19216
##  [89] 19216 19216 19216 19216 19216 19216 19216 19216 19216 19216 19216
## [100] 19216 19216     0 19216 19216 19216 19216 19216 19216 19216 19216
## [111] 19216 19216     0     0     0     0     0     0     0     0     0
## [122]     0     0     0 19216 19216 19216 19216 19216 19216 19216 19216
## [133] 19216 19216 19216 19216 19216 19216 19216     0 19216 19216 19216
## [144] 19216 19216 19216 19216 19216 19216 19216     0     0     0     0
## [155]     0     0     0     0     0     0
```

As many columns see 90% of the values missing, only those with no missing values are kept for modeling.


```r
data_mdl <- data[, col_missing == 0]
names(data_mdl)
```

```
##  [1] "X"                    "user_name"            "raw_timestamp_part_1"
##  [4] "raw_timestamp_part_2" "cvtd_timestamp"       "new_window"          
##  [7] "num_window"           "roll_belt"            "pitch_belt"          
## [10] "yaw_belt"             "total_accel_belt"     "gyros_belt_x"        
## [13] "gyros_belt_y"         "gyros_belt_z"         "accel_belt_x"        
## [16] "accel_belt_y"         "accel_belt_z"         "magnet_belt_x"       
## [19] "magnet_belt_y"        "magnet_belt_z"        "roll_arm"            
## [22] "pitch_arm"            "yaw_arm"              "total_accel_arm"     
## [25] "gyros_arm_x"          "gyros_arm_y"          "gyros_arm_z"         
## [28] "accel_arm_x"          "accel_arm_y"          "accel_arm_z"         
## [31] "magnet_arm_x"         "magnet_arm_y"         "magnet_arm_z"        
## [34] "roll_dumbbell"        "pitch_dumbbell"       "yaw_dumbbell"        
## [37] "total_accel_dumbbell" "gyros_dumbbell_x"     "gyros_dumbbell_y"    
## [40] "gyros_dumbbell_z"     "accel_dumbbell_x"     "accel_dumbbell_y"    
## [43] "accel_dumbbell_z"     "magnet_dumbbell_x"    "magnet_dumbbell_y"   
## [46] "magnet_dumbbell_z"    "roll_forearm"         "pitch_forearm"       
## [49] "yaw_forearm"          "total_accel_forearm"  "gyros_forearm_x"     
## [52] "gyros_forearm_y"      "gyros_forearm_z"      "accel_forearm_x"     
## [55] "accel_forearm_y"      "accel_forearm_z"      "magnet_forearm_x"    
## [58] "magnet_forearm_y"     "magnet_forearm_z"     "classe"
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

The method recommended was Random Forest but it took really long to run even with PCA. So I chose **method = "multinom"** as multi-class logistic regression which at least produce some results within an hour. No knitr is done here due to still long running time. 

Cross validation and out of sample error are not estimated either, which I hope to read some good submitted write-ups to learn more about them.
