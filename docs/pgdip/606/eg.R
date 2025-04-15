##########################################################################################
#                                                                                        #
#                                  H2O Package                                           #
#                                                                                        #
##########################################################################################

# H2O is an open-source, high-performance machine learning and AI platform. The R package provides an interface to the H2O backend engine, which runs in Java and is designed for distributed in-memory computing — making it extremely fast, even on large datasets.

# Supports many ML models:	GLMs, random forests, gradient boosting, deep learning (feed forward NNs only), naive Bayes, etc.
# AutoML	Automatically trains and tunes multiple models, stacking the best ones
# Handles imbalanced data	Built-in class balancing, weighted models
# Model interpretation: Variable importance, SHAP values, partial dependence plots
# Scales well:	Efficient with big datasets, supports multicore and distributed setups
# Cross-validation:	Built-in K-fold CV, early stopping, and grid search
# Supports regression, classification, clustering	Versatile across problem types

################################## Installing and loading H2O #############################

install.packages("h2o") # this only needs to be run once on your laptop
library(h2o)

# Start the H2O cluster:

# In H2O, starting a cluster means starting the H2O backend engine, which runs in Java. Even if you’re just using it on your laptop, H2O still sets up a local "cluster" — a computing environment that can process data and train models in memory. "In memory" means that your data and computations are stored and processed directly in your computer’s RAM (Random Access Memory), not on disk (like your hard drive or SSD). Accessing data from RAM is much faster than reading/writing to disk.

# Ensure you have the 64 bit version of JAVA: See this docuemnt on how to download and install it: https://drive.google.com/file/d/1nICPoxk45M26m9hhZgjdMTqxQdSyqZfk/view?usp=sharing

Sys.setenv(JAVA_HOME = "C:\\Program Files\\Java\\jdk-24")
h2o.init()

######################################### Load additional packages ##############################

library(dplyr) # for data manipulation and preprocessing
library(caTools) # for splitting into train and test sets
library(caret) # used for performance metric functions
library(pROC) # used for obtaining AUC

######################################## Loading the data####################################

# For these demonstrations, we will make use of the AdultUCI data set:

install.packages("arules")
library(arules)

?AdultUCI

data("AdultUCI")

summary(AdultUCI) # our target will be income (small: <= 50k, large: >50k)

# notice the NAs in a lot of the variables. Let's remove these:

AdultUCI <- na.omit(AdultUCI)

summary(AdultUCI)

str(AdultUCI)

# Let's drop variables that we don't need:
# The pipe operator (%>%) is used to chain together a sequence of operations. It passes the result of one function to the next function, making code more readable and easier to follow. Think of %>% as saying 'and then'. It helps us write code like a sentence: Take the data, and then filter it, (and then group it, and then summarize it etc.).

AdultUCI_new <- AdultUCI %>%
  select(-fnlwgt, -relationship, -`capital-gain`, -`capital-loss`,-`education-num`)

# The AdultUCI object from the arules package. When you load AdultUCI, it behaves a bit differently than a standard data frame. For us to edit the properties of this object, we must convert the entire dataset to a regular data frame:

AdultUCI_new <- as.data.frame(AdultUCI_new)

str(AdultUCI_new)

# notice that there are ordered variables in the data set. When we want to use the H2O package, it doesn't recognize ordered columns so we must unorder these variables:

AdultUCI_new$income <- factor(AdultUCI_new$income, ordered = FALSE)
AdultUCI_new$education <- factor(AdultUCI_new$education, ordered = FALSE)

prop.table(table(AdultUCI_new$income))

# H2O by default models the last class label based on alphabetical order - which in this case is 'small'. If we wish to change this, so that large is the event of interest, then we can code the target as 0 and 1 (1 being our event of interest) so that 1 is modelled:

AdultUCI_new$income <- ifelse(AdultUCI_new$income == "large", 1, 0) #can change event of interest to samll here

AdultUCI_new$income <- as.factor(AdultUCI_new$income)

prop.table(table(AdultUCI_new$income))


########################### Split the data into train/test sets##############################

seed <- 123 # set a seed for reproducibility

set.seed(seed)

# stratified sampling is is used to maintain the proportion of class labels in your training and test sets:
split <- sample.split(AdultUCI_new$income, SplitRatio = 0.7) # change SplitRatio for different splits, such as change to 0.8 for 80:20 split

train_income <- subset(AdultUCI_new, split == "TRUE")
test_income <- subset(AdultUCI_new, split == "FALSE")

prop.table(table(train_income$income)) # distribution of class labels maintained


######################## Convert train/test sets into H2O data frames ######################

# To use data in H2O functions/models, it needs to be an H2O data frame. The following converts the built-in R data frame iris into an H2O frame and stores it in the H2O memory space. Now, all future processing (cleaning, modeling, predictions) happens in H2O's memory space (inside the Java engine, not R’s memory). H2O is great at handling big datasets relative to RAM size due to its optimized data structures.

train_income_h2o <- as.h2o(train_income)
test_income_h2o  <- as.h2o(test_income)


############################# Data preprocessing #########################################

# Note that in this demonstration, we will be fitting a Naive Bayes classifier, decision tree and a logistic regression model. None of these algorithms require data preprocessing of the attributes (normalization and dummmy variable encoding). However, for other algorithms (SVM, KNN, NN and various ensemble methods), this is the stage at which it would be applied (to both the training and test sets).


######################### Specify name of target and predictors #########################

target <- "income"

# use setdiff function, such as setdiff(x,y) to return the elements in x that are not in y. This removes the target from the list to only leave the predictors:

predictors <- setdiff(names(train_income), target)  

############################## Fit Naive Bayes Classifier ##############################
# Build and train the Naive Bayes Classifier (https://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/naive-bayes.html):

income_nb <- h2o.naiveBayes(x = predictors,
                          y = target,
                          training_frame = train_income_h2o,
                          laplace = 0, # a smoothing parameter for categories with 
                          nfolds = 5, # for 5-fold CV
                          seed = seed) 

# check performance of model:
h2o.performance(income_nb)

# we will extract the predicted probabilities of class label = 1, append it to the original train and test sets, determine the predicted class for each based on the threshold and then create our confusion matrix and obtain the performance measures:

# Save predicted probabilities
preds_nb_train <- h2o.predict(income_nb, train_income_h2o)
preds_nb_test  <- h2o.predict(income_nb, test_income_h2o)

# Convert predictions to R data.frames to extract from H2O environment:
preds_nb_train <- as.data.frame(preds_nb_train)
preds_nb_test <- as.data.frame(preds_nb_test)

# Append column 3 (predicted probabilities for class lablel = 1) to original training and test sets:

train_nb_pred <- cbind(train_income,preds_nb_train[,3, drop = FALSE]) # drop = FALSE keeps the original name of this 3rd column (p1) in the resulting data frame
test_nb_pred <- cbind(test_income,preds_nb_test[,3, drop = FALSE])


# Create confusion matrix using a threshold:

threshold <- 0.5

# training
train_nb_pred$pred_class <- factor(ifelse(train_nb_pred$p1 > threshold, "1", "0"))

# predicted classes first then actual classes
confusionMatrix(train_nb_pred$pred_class, train_nb_pred$income, 
                positive = "1",mode = "everything")

# actual classes first then predicted probabilities
roc_nb_train <- roc(train_nb_pred$income, train_nb_pred$p1)
auc(roc_nb_train)
plot(roc_nb_train)

# test
test_nb_pred$pred_class <- factor(ifelse(test_nb_pred$p1 > threshold, "1", "0"))

# predicted classes first then actual classes
confusionMatrix(test_nb_pred$pred_class, test_nb_pred$income, 
                positive = "1",mode = "everything")

# actual classes first then predicted probabilities
roc_nb_test <- roc(test_nb_pred$income, test_nb_pred$p1)
auc(roc_nb_test)
plot(roc_nb_test)


#############################Fit a decision tree using h2o #############################

# we will start by tuning the hpyerparameter (max_depth which is the maximum depth of the final decision tree). 

# Set up the hyperparameter search space
hyper_params <- list(
  max_depth = seq(1, 20, 1)  # Try max_depth from 1 to 20
)

# Define search criteria:
search_criteria <- list(
  strategy = "Cartesian"  # Try "RandomDiscrete" for random search
)

# Run the grid search using a single decision tree, GBM (gradient boosting method) with ntrees = 1)
grid <- h2o.grid(
  algorithm = "gbm",
  grid_id = "dtree_grid", # this is just the ID we are giving the search grid
  x = predictors,
  y = target,
  training_frame = train_income_h2o,
  hyper_params = hyper_params,
  search_criteria = search_criteria,
  ntrees = 1,
  learn_rate = 1.0,       # Full weight per tree (since it's only one)
  sample_rate = 1.0,
  col_sample_rate = 1.0,
  stopping_rounds = 0,
  seed = seed
)

# View grid results
h2o.getGrid("dtree_grid", sort_by = "mse", decreasing = FALSE)

# Extract the best model ID
best_model_id <- grid@model_ids[[1]]

# Retrieve the best model
best_model <- h2o.getModel(best_model_id)

# Extract the best max_depth
tuned_max_depth <- best_model@allparameters$max_depth


# Build and train the model (H2O prevents overfitting at training time (pre-pruning)):
income_dt <- h2o.decision_tree(y = target, 
                            training_frame = train_income_h2o, 
                            max_depth = tuned_max_depth,
                            seed=seed)

# Extract tree structure (for the first tree)
tree_structure <- h2o.tree(income_dt, tree_number = 0)

# Save predicted probabilities
preds_dt_train <- h2o.predict(income_dt, train_income_h2o)
preds_dt_test  <- h2o.predict(income_dt, test_income_h2o)

# Convert predictions to R data.frames to extract from H2O environment:
preds_dt_train <- as.data.frame(preds_dt_train)
preds_dt_test <- as.data.frame(preds_dt_test)

# Append column 3 (predicted probabilities for class lablel = 1) to original training and test sets:

train_dt_pred <- cbind(train_income,preds_dt_train[,3, drop = FALSE]) # drop = FALSE keeps the original name of this 3rd column (p1) in the resulting data frame
test_dt_pred <- cbind(test_income,preds_dt_test[,3, drop = FALSE])


# Create confusion matrix using a threshold:

threshold <- 0.5

# training
train_dt_pred$pred_class <- factor(ifelse(train_dt_pred$p1 > threshold, "1", "0"))

# predicted classes first then actual classes
confusionMatrix(train_dt_pred$pred_class, train_dt_pred$income, 
                positive = "1",mode = "everything")

# actual classes first then predicted probabilities
roc_dt_train <- roc(train_dt_pred$income, train_dt_pred$p1)
auc(roc_dt_train)
plot(roc_dt_train)

# test
test_dt_pred$pred_class <- factor(ifelse(test_dt_pred$p1 > threshold, "1", "0"))

# predicted classes first then actual classes
confusionMatrix(test_dt_pred$pred_class, test_dt_pred$income, 
                positive = "1",mode = "everything")

# actual classes first then predicted probabilities
roc_dt_test <- roc(test_dt_pred$income, test_dt_pred$p1)
auc(roc_dt_test)
plot(roc_dt_test)

##################################### Fit DT using Rpart #################################

# we will use another package that fits a DT that allows for a visualization:

library(rpart)

set.seed(seed)

DT_rpart <- rpart(income ~.,  data = train_income,  
            method  = "class", # for classification
            xval=10 # 10 fold CV
) # default attribute selection measure is Gini Index

DT_rpart # run this to see information about the fitted tree

# Let's obtain the complexity parameter (cp). The complexity parameter is used to control the size of the decision tree and to select the optimal tree size. 
# The CP values indicate how much the overall error rate decreases with each split. A large CP indicates that a split resulted in a significant decrease in error, while a smaller CP suggests a less impactful split. This parameter determines a threshold under which the split of a node is not worth the complexity.
# If the cost of adding another variable to the decision tree from the current node is 
# above the value of cp, then tree building should not continue.

# The rel error is the total error of the model divided by the error of the initial model (a model with just the root node, predicting the most frequent class). It's a measure of the error relative to the simplest possible model.

# The xerror is the cross-validation error of the model. It is computed during the tree-building process if cross-validation is enabled (e.g., using the xval argument in rpart()). This error is estimated by applying the decision tree to each of the cross-validation folds used during tree construction. It provides a measure of how well the tree is likely to perform on unseen data, hence an estimate of the model's generalization error. Typically, it helps identify if the model is overfitting. If xerror starts to increase as the complexity of the model increases (more splits in the tree), it may suggest that simpler models are preferable.

# The xstd is the standard error of the cross-validation error (xerror). This value provides an indication of the variability of the cross-validation error estimate. A high standard error suggests that the cross-validation error might not be a reliable estimate of the model's error on new data, possibly due to the model being unstable across different subsets of the training data or due to a small number of cross-validation folds.

# rpart() automatically computes the optimal tree size (considering complexity cost) using these metrics. Specifically, xerror and xstd are used to determine the smallest tree that is within one standard error of the minimum cross-validation error (xerror + xstd). This criterion helps to balance model accuracy with complexity, aiming to avoid overfitting while maintaining sufficient explanatory power.

printcp(DT_rpart) 
plotcp(DT_rpart)


### Extract predicted probabilities:
# Note: this provides two columns - the predicted probabilities for "0" in column 1 and "1" in column 2.

pred_prob_DT_train <- predict(DT_rpart, newdata = train_income,type = "prob")
train_DT_rpart <- cbind(train_income,pred_prob_DT_train[,2,drop = FALSE]) # We only want the probs in column 2 (for "1")


pred_prob_DT_test <- predict(DT_rpart, newdata = test_income,type = "prob")
test_DT_rpart <- cbind(test_income,pred_prob_DT_test[,2,drop = FALSE]) # We only want the probs in column 2 (for "1")


### Then select a cut-off (or find optimal cut-off):

threshold_DT <- 0.5 # specify the cut-off here (this can change to find the optimal)

# training (note the predicted probabilities are now in a column named "1")
train_DT_rpart$pred_class <- factor(ifelse(train_DT_rpart$`1` > threshold_DT, "1", "0"))

# predicted classes first then actual classes
confusionMatrix(train_DT_rpart$pred_class, train_DT_rpart$income, 
                positive = "1",mode = "everything")

# actual classes first then predicted probabilities
roc_DT_train_rpart <- roc(train_DT_rpart$income, train_DT_rpart$`1` )
auc(roc_DT_train_rpart)
plot(roc_DT_train_rpart)

# test
test_DT_rpart$pred_class <- factor(ifelse(test_DT_rpart$`1`  > threshold, "1", "0"))

# predicted classes first then actual classes
confusionMatrix(test_DT_rpart$pred_class, test_DT_rpart$income, 
                positive = "1",mode = "everything")

# actual classes first then predicted probabilities
roc_DT_test_rpart <- roc(test_DT_rpart$income, test_DT_rpart$`1`)
auc(roc_DT_test_rpart)
plot(roc_DT_test_rpart)

# visualize the DT:

library(rpart.plot)

dev.new(width=15, height=20) # This just allows the plot to be shown in a separate window (useful for small screens)

rpart.plot(DT_rpart)
rpart.plot(DT_rpart, yesno=1,type=2,fallen.leaves = FALSE) # add additional options to change the appearance.
# see http://www.milbo.org/rpart-plot/prp.pdf for more options to customize the plot

######################### Fit a logistic regression model ################################

# A logistic regression is in the class of a generalized linear model (GLM), various GLMs can be fitted in h2o for different types of responses (continuous, binary, count, multiple categories - multi-class classification)

# Fit the logistic regression model
income_LR <- h2o.glm(
  x = predictors,
  y = target,
  training_frame = train_income_h2o,
  family = "binomial",       # logistic regression
  lambda = 0,                # no regularization (like classical GLM)
  compute_p_values = TRUE    # optional: get p-values
)

# extract p-values for inference:
income_LR@model[["coefficients_table"]]

# Save predicted probabilities
preds_LR_train <- h2o.predict(income_LR, train_income_h2o)
preds_LR_test  <- h2o.predict(income_LR, test_income_h2o)

# Convert predictions to R data.frames to extract from H2O environment:
preds_LR_train <- as.data.frame(preds_LR_train)
preds_LR_test <- as.data.frame(preds_LR_test)

# Append column 3 (predicted probabilities for class lablel = 1) to original training and test sets:

train_LR_pred <- cbind(train_income,preds_LR_train[,3, drop = FALSE]) # drop = FALSE keeps the original name of this 3rd column (p1) in the resulting data frame
test_LR_pred <- cbind(test_income,preds_LR_test[,3, drop = FALSE])


# Create confusion matrix using a threshold:

threshold <- 0.5

# training
train_LR_pred$pred_class <- factor(ifelse(train_LR_pred$p1 > threshold, "1", "0"))

# predicted classes first then actual classes
confusionMatrix(train_LR_pred$pred_class, train_LR_pred$income, 
                positive = "1",mode = "everything")

# actual classes first then predicted probabilities
roc_LR_train <- roc(train_LR_pred$income, train_LR_pred$p1)
auc(roc_nb_train)
plot(roc_nb_train)

# test
test_LR_pred$pred_class <- factor(ifelse(test_LR_pred$p1 > threshold, "1", "0"))

# predicted classes first then actual classes
confusionMatrix(test_LR_pred$pred_class, test_LR_pred$income, 
                positive = "1",mode = "everything")

# actual classes first then predicted probabilities
roc_LR_test <- roc(test_LR_pred$income, test_LR_pred$p1)
auc(roc_nb_test)
plot(roc_nb_test)


#################### Combine ROC curves of test set for all models ############


# Plot (see https://r-charts.com/colors/ for more colours)
plot(roc_nb_test, col = "#458B74", lwd = 2, main = "ROC Curve Comparison of test set for NB, DT and LR")
lines(roc_DT_test_rpart, col = "#CD3333", lwd = 2)
lines(roc_LR_test, col = "#009ACD", lwd = 2)

# Add legend
legend("bottomright",
       legend = c("Naive Bayes", "Decision tree", "Logistic regression"),
       col = c("#458B74", "#CD3333", "#009ACD"),
       lwd = 2)



############## Shut down H2O cluster so it doesn't use up any more resources ############


h2o.shutdown(prompt = FALSE)




