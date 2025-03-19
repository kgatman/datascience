##################################################################
#################Data Exploration and Analysis####################
##################################################################
setwd("C:\\Users\\Makhate\\deepdives\\datascience\\docs\\pgdip\\605\\CO2\\info")
getwd()
CO2data <- read.csv("CO2EmissionsData.csv")

str(CO2data)
summary(CO2data)

#####Cleaning#######
clean_CO2data <- na.omit(CO2data)
####
# the clean dataframe now has 7354 entries/observations
# 31 Obs have been ommited
###

####Checking Categorical Variables#######
print(sapply(clean_CO2data,class))
########################################
#### Reducing Many Factors ####
clean_CO2data$Vehicle.Class[clean_CO2data$Vehicle.Class %in% c('SUV - SMALL', 'SUV - STANDARD', 'MINIVAN')] <- 'SUV'
clean_CO2data$Vehicle.Class[clean_CO2data$Vehicle.Class %in% c('MID-SIZE', 'TWO-SEATER', 'FULL-SIZE', 'STATION WAGON - SMALL', 'STATION WAGON - MID-SIZE')] <- 'Sedan'
clean_CO2data$Vehicle.Class[clean_CO2data$Vehicle.Class %in% c('VAN - CARGO', 'VAN - PASSENGER', 'PICKUP TRUCK - STANDARD', 'SPECIAL PURPOSE VEHICLE', 'PICKUP TRUCK - SMALL')] <- 'Bakkie'
clean_CO2data$Vehicle.Class[clean_CO2data$Vehicle.Class %in% c('COMPACT', 'MINICOMPACT', 'SUBCOMPACT')] <- 'Town_Runners'

####Preparing Stats
summary_clean_CO2data <-as.data.frame(summary(clean_CO2data))

##############Deduplicating##################
library(dplyr)
deduplicated_CO2<-clean_CO2data %>% distinct()
View(deduplicated_CO2)

##############Stats##################
stats_on_deduplicated_data <- as.data.frame(summary(deduplicated_CO2))
install.packages('writexl')
library('writexl')
write_xlsx(stats_on_deduplicated_data,"stats_on_deduplicated_data.xlsx")
########################################


##################################################################
##########Data Visualization Before Fitting Models################
##################################################################
library(ggplot2)
##########Emissions By Engine Size############
ggplot(data=deduplicated_CO2, 
       aes(x = CO2.Emissions.g.km., y = Engine.Size.L., color=Engine.Size.L.)) +
  geom_point() +
  labs(title = "CO2 Emissions By Engine Size",
       x = "CO2 Emissions",
       y = "Engine Size")

##########Emissions By Fuel Type############
ggplot(data=deduplicated_CO2, 
       aes(x =Fuel.Type , y = CO2.Emissions.g.km., color=Fuel.Type)) +
  geom_boxplot() +
  labs(title = "CO2 Emissions By Fuel Type",
       x = "Fuel Type",
       y = "CO2 Emissions")

##########Emissions By Cylinders############
ggplot(data=deduplicated_CO2, 
       aes(x = Cylinders, y = CO2.Emissions.g.km., color=Cylinders)) +
  geom_point() +
  labs(title = "CO2 Emissions By Cylinders",
       x = "Cylinders",
       y = "CO2 Emissions")

##########Emissions By Vehicle TYpe############
ggplot(data=deduplicated_CO2, 
       aes(x = Vehicle.Class, y = CO2.Emissions.g.km., color=Vehicle.Class)) +
  geom_boxplot() +
  labs(title = "CO2 Emissions By Vehicle Type",
       x = "CO2 Emissions",
       y = "Vehicle Type")

##########Emissions By Fuel Consumption############
ggplot(data=deduplicated_CO2, 
       aes(x = CO2.Emissions.g.km., y = Fuel.Consumption.Comb..L.100.km., color=Fuel.Consumption.Comb..L.100.km.)) +
  geom_point() +
  labs(title = "CO2 Emissions By Fuel Consumption",
       x = "CO2 Emissions",
       y = "Consumption")




##################################################################
####### OLS | Ridge | LASSO | Elastic Net | Regression Trees #####
##################################################################

numeric_deduplicated_CO2 <- deduplicated_CO2

#Converting categorical variables into numericals
numeric_deduplicated_CO2$Make <- as.numeric(as.factor(deduplicated_CO2$Make))
numeric_deduplicated_CO2$Model <- as.numeric(as.factor(deduplicated_CO2$Model))
numeric_deduplicated_CO2$Model <- as.numeric(as.factor(deduplicated_CO2$Model))
numeric_deduplicated_CO2$Vehicle.Class <- as.numeric(as.factor(deduplicated_CO2$Vehicle.Class))
numeric_deduplicated_CO2$Engine.Size.L. <- as.numeric(as.factor(deduplicated_CO2$Engine.Size.L.))
numeric_deduplicated_CO2$Cylinders <- as.numeric(as.factor(deduplicated_CO2$Cylinders))
numeric_deduplicated_CO2$Transmission <- as.numeric(as.factor(deduplicated_CO2$Transmission))
numeric_deduplicated_CO2$Fuel.Type <- as.numeric(as.factor(deduplicated_CO2$Fuel.Type))
###
# in class, the package dummy was used to preprocess the dataset
#############Splitting the data ###################
library(caTools)
set.seed(123) #to make sure we'll be using the same training dataset
split=sample.split(numeric_deduplicated_CO2$CO2.Emissions.g.km.,SplitRatio = 0.8)

training_set = subset(numeric_deduplicated_CO2,split==TRUE)
test_set = subset(numeric_deduplicated_CO2,split==FALSE)

######Summary Stats##########
summary(training_set$CO2.Emissions.g.km.)
sd(training_set$CO2.Emissions.g.km.)

summary(test_set$CO2.Emissions.g.km.)
sd(test_set$CO2.Emissions.g.km.)

#################OLS##############

OLS = lm(CO2.Emissions.g.km.~.,data=training_set)
summary(OLS)

####Using the fitted model to make predictions######
OLS_predict_training = predict(OLS,newdata = training_set)
OLS_predict_test = predict(OLS,newdata = test_set)

######Performance Metrics#############
library(MLmetrics)
########Training Set
MAE(OLS_predict_training,training_set$CO2.Emissions.g.km.)
RMSE(OLS_predict_training,training_set$CO2.Emissions.g.km.)
R2_Score(OLS_predict_training,training_set$CO2.Emissions.g.km.)

########Test Set
MAE(OLS_predict_test,test_set$CO2.Emissions.g.km.)
RMSE(OLS_predict_test,test_set$CO2.Emissions.g.km.)
R2_Score(OLS_predict_test,test_set$CO2.Emissions.g.km.)

#################Ridge##################
###converting data frame into matrix
#Training
X_train <- as.matrix(select(training_set,-CO2.Emissions.g.km.))
Y_train <- training_set$CO2.Emissions.g.km.
#Test
X_test <- as.matrix(select(test_set,-CO2.Emissions.g.km.))
Y_test <- test_set$CO2.Emissions.g.km.

#lambda
install.packages("glmnet")
library(glmnet)
lambdas_to_play_with <- 10^seq(-3,5,length.out = 100)
set.seed(123)
ridge_cv <- cv.glmnet(X_train, Y_train, 
                      type.measure="mae",
                      alpha = 0, lambda = lambdas_to_play_with, 
                      standardize = TRUE) #defaults to nfolds = 10
plot(ridge_cv)
ridge_cv_mse <- cv.glmnet(X_train, Y_train, 
                      type.measure="mse",
                      alpha = 0, lambda = lambdas_to_play_with, 
                      standardize = TRUE) #defaults to nfolds = 10
plot(ridge_cv_mse)

#extract the best lamdda from the CV
lambda_cv <-ridge_cv$lambda.min
#Fit the final ridge reg model
ridge_reg <- glmnet(X_train,Y_train,
                    alpha=0, lambda = lambda_cv,
                    standardize = TRUE)

#Make predictions on training and test datasets
Y_ridge_pred_train <- predict(ridge_reg,X_train)
Y_ridge_pred_test <- predict(ridge_reg,X_test)

#Perfomance
MAE(Y_ridge_pred_test,Y_test)
MAE(Y_ridge_pred_train,Y_train)


###########LASSO
set.seed(123)
library(glmnet)
lasso_cv <- cv.glmnet(X_train, Y_train,
                      type.measure="mae",
                      alpha = 1, 
                      lambda = lambdas_to_play_with, 
                      standardize = TRUE) #defaults to nfolds = 10

plot(lasso_cv)

lambda_cv_lasso = lasso_cv$lambda.min

#Fitting the model
lasso_reg <- glmnet(X_train,
                    Y_train,
                    alpha = 1,
                    lambda = lambda_cv_lasso,
                    standardize = TRUE)
summary(lasso_reg)
coefficients(lasso_reg)

#Make predictions on training and test datasets
Y_lasso_pred_train <- predict(lasso_reg,X_train)
Y_lasso_pred_test <- predict(lasso_reg,X_test)

#Perfomance
MAE(Y_lasso_pred_train,Y_test)
MAE(Y_lasso_pred_test,Y_train)

#### Elastic Net
library(caret)
set.seed(123)

train_control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 1,
                              search = "random",
                              verboseIter = TRUE)

elastic_net <- train(CO2.Emissions.g.km. ~.,
                     data = training_set,
                     method = "glmnet",
                     preProcess = c("center","scale"),
                     tuneLength = 25,
                     trControl = train_control)

Y_elastic_pred_train=predict(elastic_net,training_set)
Y_elastic_pred_test=predict(elastic_net,test_set)

MAE(Y_elastic_pred_train,training_set$CO2.Emissions.g.km.)
MAE(Y_elastic_pred_test,test_set$CO2.Emissions.g.km.)

###Regression Tree
library(caTools)
set.seed(123) #to make sure we'll be using the same training dataset

split2 = sample.split(numeric_deduplicated_CO2$CO2.Emissions.g.km.,
                      SplitRatio = 0.8)

training_set2 = subset(numeric_deduplicated_CO2,
                       split2==TRUE)
test_set2 = subset(numeric_deduplicated_CO2,
                       split2==FALSE)

library(rpart)

set.seed(123)
RegTree <- rpart(CO2.Emissions.g.km. ~.,
                 data = training_set2,
                 method = "anova") #defaulting to xval=10 for a 10-fold

RegTree

library(rpart.plot)
rpart.plot(RegTree,
           yesno = 1,
           type = 2,
           fallen.leaves = FALSE)
install.packages("partykit")
library(partykit)
testingRegTree <- as.party(RegTree)
plot(testingRegTree)


install.packages("visNetwork")
install.packages("sparkline") ###the visNetwork library demanded that I install it
library(sparkline) #### so I had to try it out
library(visNetwork)
visTree(RegTree) ###beautiful tree this one🥰

##################################################################
###############   SVR | KNN | Neural Network #####################
##################################################################
library(caTools)
set.seed(123)
split_k = sample.split(numeric_deduplicated_CO2$CO2.Emissions.g.km.,
                       SplitRatio = 0.8)
training_set_k = subset(numeric_deduplicated_CO2,split_k == TRUE)
test_set_k = subset(numeric_deduplicated_CO2,split_k == FALSE)

library(caret)

control <- trainControl(method = "cv", number = 10)

set.seed(123)
svr_linear <- train(CO2.Emissions.g.km.~., 
                    data = training_set_k,
                    method = "svmLinear2", 
                    preProcess = c("center","scale"),
                    trControl = control)
svr_linear
plot(svr_linear)
summary(svr_linear)

set.seed(123)
svr_linear1 <- train(CO2.Emissions.g.km.~., 
                    data = training_set_k,
                    method = "svmLinear2", 
                    preProcess = c("center","scale"),
                    trControl = control,
                    epsilon = 0.5)
svr_linear1
plot(svr_linear1)
summary(svr_linear1)
##test
set.seed(123)
svr_linear1_test <- train(CO2.Emissions.g.km.~., 
                     data = test_set_k,
                     method = "svmLinear2", 
                     preProcess = c("center","scale"),
                     trControl = control,
                     epsilon = 0.5)
summary(svr_linear1_test)
svr_linear1_test

set.seed(123)
svr_linear2 <- train(CO2.Emissions.g.km.~., 
                     data = training_set_k,
                     method = "svmLinear2", 
                     preProcess = c("center","scale"),
                     trControl = control,
                     epsilon = 0.7)
svr_linear2
plot(svr_linear2)
summary(svr_linear2)

#from the most preferred model
varImp(svr_linear1)
####svr_linear2 was bad!!! I'm discarding!!!!🚮🚮🚮




################### KNN #################
set.seed(123)
knn <- train(CO2.Emissions.g.km.~.,
             data = training_set_k, 
             method = "knn", 
             preProcess=c("center", "scale"), 
             trControl = control)
knn
plot(knn)



pred__train_knn = predict(knn,newdata=training_set_k)
pred_test_knn = predict(knn,newdata=test_set_k)

#Performance on training set:

rbind("knn Train",c("MAE","RMSE"),c(round(MAE(pred__train_knn,training_set_k$CO2.Emissions.g.km.),3),  
                                    round(RMSE(pred__train_knn,training_set_k$CO2.Emissions.g.km.),3)))


## Performance on test set:

rbind("knn Test", c("MAE","RMSE"), c(round(MAE(pred_test_knn,test_set_k$CO2.Emissions.g.km.),3),
                                     round(RMSE(pred_test_knn,test_set_k$CO2.Emissions.g.km.),3)))


### Neural Networks
set.seed(123)
NN <- train(CO2.Emissions.g.km.~., 
            data=training_set_k, 
            method="nnet", 
            preProcess=c("center", "scale"), 
            trControl=control, 
            linout=TRUE,
            maxit=1000)
NN

plot(NN)
install.packages("NeuralNetTools")
library(NeuralNetTools)

plotnet(NN)

# Prediction of NN:

pred__train_NN1 = predict(NN,newdata=training_set_k)
pred_test_NN1 = predict(NN,newdata=test_set_k)

#Performance on training set:
library(MLmetrics)
rbind("NN Train",c("MAE","RMSE"),c(round(MAE(pred__train_NN1,training_set_k$CO2.Emissions.g.km.),3),  
                                   round(RMSE(pred__train_NN1,training_set_k$CO2.Emissions.g.km.),3)))


## Performance on test set:

rbind("NN Test", c("MAE","RMSE"), c(round(MAE(pred_test_NN1,test_set_k$CO2.Emissions.g.km.),3),
                                    round(RMSE(pred_test_NN1,test_set_k$CO2.Emissions.g.km.),3)))


# Variable importances for the NN:
library(caret)
varImp(NN)