Homework 1
================
DS Student
January 18, 2024

## Homework Description

For this homework, I will do the following tasks:

- from the file, `mixture-data-lin-knn.R`, rewrite the function `fit_lc`
  and `predict_lc` using `lm`.
- Make linear classifier more flexible by adding squared terms for x1,
  and x2
- Describe how the flexible models would affect the bias-variance
  tradeoff

## Code Execution

``` r
library('class')
library('dplyr')
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
set.seed(123)

## load binary classification example data from author website 
## 'ElemStatLearn' package no longer available
load(url('https://biostat.app.vumc.org/wiki/pub/Main/CourseDSI5640/ESL.mixture.rda'))
dat <- ESL.mixture

plot_mix_data <- function(dat, datboot=NULL) {
  if(!is.null(datboot)) {
    dat$x <- datboot$x
    dat$y <- datboot$y
  }
  plot(dat$x[,1], dat$x[,2],
       col=ifelse(dat$y==0, 'blue', 'orange'),
       pch=20,
       xlab=expression(x[1]),
       ylab=expression(x[2]))
  ## draw Bayes (True) classification boundary
  prob <- matrix(dat$prob, length(dat$px1), length(dat$px2))
  cont <- contourLines(dat$px1, dat$px2, prob, levels=0.5)
  rslt <- sapply(cont, lines, col='purple')
}

## fit linear classifier
fit_lc <- function(y, x) {
  ## Adding squared terms for x1 and x2
  x_expanded <- cbind(1, x1= x[,1], x2= x[,2], x1_squared= x[,1]^2, x2_squared = x[,2]^2 )
  model <- lm ( y~., data = as.data.frame(x_expanded))
  return(model)
  }

## make predictions from linear classifier
predict_lc <- function(model, x) {
  x_expanded <- cbind(1, x1= x[,1], x2= x[,2], x1_squared= x[,1]^2, x2_squared = x[,2]^2 )
  predictions <- predict(model, newdata = as.data.frame(x_expanded))
  return(predictions)
}

## Fit model to mixture data and make predictions
lc_model <- fit_lc(dat$y, dat$x)
lc_pred <- predict_lc(lc_model, dat$xnew)

## reshape predictions as a matrix
lc_pred_matrix  <- matrix(lc_pred, length(dat$px1), length(dat$px2))
contour(lc_pred_matrix,
        xlab=expression(x[1]),
        ylab=expression(x[2]))
```

![](trhoang_homework1_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
## find the contours in 2D space such that lc_pred == 0.5
lc_cont <- contourLines(dat$px1, dat$px2, lc_pred_matrix, levels=0.5)

## plot data and decision surface
plot_mix_data(dat)
sapply(lc_cont, lines, col='purple')
```

![](trhoang_homework1_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

    ## [[1]]
    ## NULL

## Comments/Observation

By adding squared terms of x1 and x2, it increases the model’s
flexibility to capture more complexed relationship, potentially reducing
bias if the true relationship is indeed nonlinear. However, with
increased model complexity comes the risk of higher variance – the model
might start to fit the noise in the data rather than just the underlying
trend
