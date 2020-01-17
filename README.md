---
title: "Readme for chicagocrime package"
output: html_document
---


## Installation
This package can be installed using devtools and the command
```{r}
devtools::install_github("https://github.com/jakespiteri/chicagocrime")
```

## Introduction

The `chicagocrime` package allows access to multiple processes. For classification, it allows:

 - The fitting of a logistic regression model as an `lr` class
 - Predict from the `lr` model; one of 'predictions', 'predicted probabilities' and 'predicted values'
 - Producing mean squared error (MSE) and average log score diagnostics from an `lr` model
 - Performing cross validation on an `lr` model
 - Plotting a Recever Operating Characteristic (ROC) curve and outputting the Area Under Curve (AUC) diagnostic for an `lr` model
 


The package also allows access to several data sets, which are

 - The crimes data from the city of Chicago data portal, from 2001 to 2019
 - Chicago census data from 2010
 - Chicago population data
 - Police station locations in Chicago
 
There are also functions that modify and clean the data. These are

 - Pre-process the data, removing outliers and missing entries
 - Format the data, by adding census, police station and population data
 
## Data
To load the data, you can use the function `data`, and choose one of `all_crime`, `sub_all_crime`, `census`, `police_stations` or `chicago_population`. The data frame `all_crime` is very large, with 6,336,525 rows and 14 columns. Most work is performed on `sub_all_crime`, a randomly shuffled subset of `all_crime`, with 50,000 rows and 14 columns. For this readme, we will use the subsetted data.

First, we load the package, load in the data, and clean and format it.
```{r, cache=TRUE}
library(chicagocrime)
data(sub_all_crime)
sub_all_crime = cleanData(sub_all_crime)
sub_all_crime = formatData(sub_all_crime)
```
When fitting, we can also split our data into a training set and a testing set. For this example, we will split the final 20% of the data set off as a testing set. Since the data is already shuffled, we do not need to shuffle it here.
```{r}
cut = round(0.8*nrow(sub_all_crime))
training_set = sub_all_crime[1:cut,]
testing_set = sub_all_crime[(cut+1):nrow(sub_all_crime),]
```
## Logistic Regression
To implement logistic regression, you may use the function `lr`, and specify two arguments; `formula` and `data`. Respectively, these are a symbolic description of the model to be fitted, and the data frame which the formula corresponds to. A simple model with minimial covariates is fit as
```{r}
fit = lr(Arrest ~ Year + `HARDSHIP INDEX` + `Primary Type`, data = training_set)
```
The `print` function will show covariate estimates for this model.
```{r}
print(fit)
```
And to predict from this model also, the `predict` function is used, with different specification of what output to produce. To produce training set predictions, no other arguments need to be specified to `predict`.
```{r}
p_preds = predict(fit, type="preds")
p_probs = predict(fit, type="probs")
p_vals = predict(fit, type="vals")
```
The `p_preds` vector is be a vector of in-sample predictions, i.e. a vector of 0's and 1's. `p_probs` is a vector of probabilities for predicting the positive class, i.e. probability of predicting an arrest (so that `1-p_probs` will be a vector of probabilities for predicting the negative class). Finally, `p_vals` is a vector formed from multiplying the model coefficients against the model matrix formed during fitting, and no other processing. We can inspect these.
```{r}
table(p_preds)
par(mfrow=c(1,2))
hist(p_probs, main = "Probabilities")
hist(p_vals, main = "Predicted Values")
```

These predictions are the basis for providing diagnostics in the form of MSE, log score and AUC. Since MSE is a simple diagnostic, we can simply code
```{r}
mean((p_preds - training_set$Arrest)^2, na.rm=TRUE)
```
For log score and AUC, we can use the functions `roc.lr` and `log_score`. Since the ROC curve is also a plotting diagnostic, we can specify the argument `plot=TRUE` or `plot=FALSE` to include the plot (and the AUC), or just the AUC. The `roc.lr` function takes the argument of the model fit, whereas `log_score` only needs the prediction and the actual response.
```{r}
log_score(p_probs, training_set$Arrest)
roc.lr(fit, plot = TRUE)
```

Cross-validation is implemented by the function `cv.lr`. This takes the argument `lrfit` - the model fit by `lr`. It also can take `metric`; one of 'mse', 'auc', 'log' or 'all', meaning MSE, AUC, log score or all three metrics, `newdata`; new data to implement for, `leave_out`; the number of points in the data set to leave out on each iterations, `verbose` a logical indicating whether a loading bar will appear, and `seed`; the seed to set (`set.seed(seed)`) when shuffling the data set. We can implement this on our model for internal validation:
```{r}
cv.lr(fit, metric="all", seed=3)
```

Likewise, we can also perform these operations on our test set.
```{r}
p_test_preds = predict(fit, newdata=testing_set, type="preds")
p_test_probs = predict(fit, newdata=testing_set, type="probs")
p_test_vals = predict(fit, newdata=testing_set, type="vals")

mean((p_test_preds - testing_set$Arrest)^2, na.rm=TRUE)
log_score(p_test_probs, testing_set$Arrest)
roc.lr(fit, newdata=testing_set, plot=TRUE)
```

### Model Selection
We can choose which covariates to include using different methods. One way is to try different combinations of methods and see which reduces the overall MSE, AUC and log score, using some measure incorporating all three. A more robust method involves using regularised logistic regression, and plotting the value of the coefficient relating to each method as the $L_1$ norma in regularisation decreases. The coefficients that are most useful to the model will reduce to zero as the $L_1$ norm decreases.

This can be implemented with the package `glmpath`, and fitting a logistic regression model using the `glmpath` function. Firstly, we fit a model with the `Primary Type` factors only, and seeing which are most important.
```{r}
library(glmpath)
y = training_set$Arrest
X = model.matrix(~`Primary Type`, data = training_set)
glmpath_fit = glmpath(y=y,x=X)
par(mar=c(5,4,4,16.6))
plot(glmpath_fit)
```

So the primary crime types that are most important are *narcotics*, *prostitution*, *criminal trespassing* and *weapons violation*. Some of the crime types aren't as important to predicting arrests, but overall it is an important predictor to include. Now we can look at other possible covariates.
```{r}
X = model.matrix(~Domestic + `Population Density` + `Crime Density` + `HARDSHIP INDEX` + Hour + dist_from_station, data = training_set)
glmpath_fit = glmpath(y=y,x=X)
par(mar=c(5,4,4,16.6))
plot(glmpath_fit)
```
So all of these covariates are important, with maybe the exception of *population density*. Now we can fit a model with these covariates.
```{r}
final_fit = lr(Arrest~Domestic + `Primary Type` + `Crime Density` + `HARDSHIP INDEX` + Hour + dist_from_station, data = training_set)
```
And find the training error
```{r}
p_preds = predict(fit, type="preds")
p_probs = predict(fit, type="probs")

mean((p_preds - training_set$Arrest)^2, na.rm=TRUE)
log_score(p_probs, training_set$Arrest)
roc.lr(fit, plot = TRUE)
cv.lr(fit, metric="all", seed=3)
```
And the testing error
```{r}
p_test_preds = predict(final_fit, newdata=testing_set, type="preds")
p_test_probs = predict(final_fit, newdata=testing_set, type="probs")

mean((p_test_preds - testing_set$Arrest)^2, na.rm=TRUE)
log_score(p_test_probs, testing_set$Arrest)
roc.lr(fit, newdata=testing_set, plot=TRUE)
```
And compare to a model containing all (non-correlated and working) covariates.
```{r}
full_fit = lr(Arrest~Domestic + `Primary Type` + `Crime Density` + `HARDSHIP INDEX` + Hour + dist_from_station + Year + `Population Density` + Latitude + Longitude, data = training_set)
```
And find the training error
```{r}
p_preds = predict(full_fit, type="preds")
p_probs = predict(full_fit, type="probs")

mean((p_preds - training_set$Arrest)^2, na.rm=TRUE)
log_score(p_probs, training_set$Arrest)
roc.lr(full_fit, plot = TRUE)
cv.lr(full_fit, metric="all", seed=3)
```
And the testing error
```{r}
p_test_preds = predict(full_fit, newdata=testing_set, type="preds")
p_test_probs = predict(full_fit, newdata=testing_set, type="probs")

mean((p_test_preds - testing_set$Arrest)^2, na.rm=TRUE)
log_score(p_test_probs, testing_set$Arrest)
roc.lr(full_fit, newdata=testing_set, plot=TRUE)
```

# Adding spatial features to data

Add the spatial features to the dataset by inputting the crime dataset, the census block boundaries dataset and the census population dataset into the `create_spatial_attributes` function.

```{r, eval = FALSE}
data_w_spatial_attributes <- create_spatial_attributes(sub_all_crime, census_blocks, block_populations)
```

