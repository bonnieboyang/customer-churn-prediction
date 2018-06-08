library(dplyr)
library(ggplot2)
library(GGally)
library(caret)
library(pROC)


set.seed(50)

rm(list = ls())  # empty the environment

tcc = read.csv("Telco-Customer-Churn.csv", head = TRUE)


summary(tcc)

str(tcc)

# EDA and feature engineering
# change the type of seniorcitizen from int into factor
tcc$SeniorCitizen = as.factor(tcc$SeniorCitizen)

# use ggpairs to explore the relathionships among variables
# devide data into four subsets
demographic_variables = select(tcc, gender, SeniorCitizen, 
                               Partner, Dependents)
basicinfo_variables = select(tcc, Contract,PhoneService, 
                             MultipleLines, InternetService)
service_variables = select(tcc, OnlineSecurity, OnlineBackup, 
                           DeviceProtection, TechSupport, 
                           StreamingTV, StreamingMovies)
payment_variables = select(tcc, PaperlessBilling, PaymentMethod, 
                           MonthlyCharges, TotalCharges, tenure)

# plot the relationships among the demographic variables
demographic_ggpairs = ggpairs(demographic_variables, 
        #columns = c("gender", "SeniorCitizen", "Partner", "Dependents"), 
        upper = list(discrete = 'blank'),
        lower = list(discrete = wrap('facetbar', alpha = 0.7)),
        diag = list(discrete = "barDiag"),
        title = "Within demographic variables")

# add Churn info  to the diagonal plots, show the relationships between demographic variables and Churn
demographic_ggpairs[1,1] = demographic_ggpairs[1,1]+aes(fill = tcc$Churn)
demographic_ggpairs[2,2] = demographic_ggpairs[2,2]+aes(fill = tcc$Churn)
demographic_ggpairs[3,3] = demographic_ggpairs[3,3]+aes(fill = tcc$Churn)
demographic_ggpairs[4,4] = demographic_ggpairs[4,4]+aes(fill = tcc$Churn)
demographic_ggpairs


# plot the relationships among the basic info variables
basicinfo_ggpairs = ggpairs(basicinfo_variables,
                              upper = list(discrete = 'blank'),
                              lower = list(discrete = wrap('facetbar', alpha = 0.7)),
                              diag = list(discrete = "barDiag"),
                              title = "Within basic info variables")

# add Churn info  to the diagonal plots, show the relationships between basic info variables and Churn
basicinfo_ggpairs[1,1] = basicinfo_ggpairs[1,1]+aes(fill = tcc$Churn)
basicinfo_ggpairs[2,2] = basicinfo_ggpairs[2,2]+aes(fill = tcc$Churn)
basicinfo_ggpairs[3,3] = basicinfo_ggpairs[3,3]+aes(fill = tcc$Churn)
basicinfo_ggpairs[4,4] = basicinfo_ggpairs[4,4]+aes(fill = tcc$Churn)
basicinfo_ggpairs

# plot the relationships among the service variables
service_ggpairs = ggpairs(service_variables,
                            upper = list(discrete = 'blank'),
                            lower = list(discrete = wrap('facetbar', alpha = 0.7)),
                            diag = list(discrete = "barDiag"),
                            title = "Within service variables")

# add Churn info  to the diagonal plots, show the relationships between service variables and Churn
service_ggpairs[1,1] = service_ggpairs[1,1]+aes(fill = tcc$Churn)
service_ggpairs[2,2] = service_ggpairs[2,2]+aes(fill = tcc$Churn)
service_ggpairs[3,3] = service_ggpairs[3,3]+aes(fill = tcc$Churn)
service_ggpairs[4,4] = service_ggpairs[4,4]+aes(fill = tcc$Churn)
service_ggpairs[5,5] = service_ggpairs[5,5]+aes(fill = tcc$Churn)
service_ggpairs[6,6] = service_ggpairs[6,6]+aes(fill = tcc$Churn)
service_ggpairs

# plot the relationships among the payment variables
payment_ggpairs = ggpairs(payment_variables,
                            upper = list(discrete = 'blank'),
                            lower = list(combo = wrap('box', alpha = 0.7)),
                            diag = list(discrete = "barDiag"),
                            title = "Within payment variables")

# add Churn info  to the diagonal plots
# show the relationships between payment variables and Churn
payment_ggpairs[1,1] = payment_ggpairs[1,1]+aes(fill = tcc$Churn)
payment_ggpairs[2,2] = payment_ggpairs[2,2]+aes(fill = tcc$Churn)
payment_ggpairs[3,3] = payment_ggpairs[3,3]+aes(fill = tcc$Churn)
payment_ggpairs[4,4] = payment_ggpairs[4,4]+aes(fill = tcc$Churn)
payment_ggpairs[5,5] = payment_ggpairs[5,5]+aes(fill = tcc$Churn)
payment_ggpairs

# feature engineering
# further explore the relationship between MonthCharges, tenure and TotalCharges
tcc %>% ggplot(mapping = aes(x = MonthlyCharges*tenure, y = TotalCharges)) +
  geom_point()
# based on the above plot, remove total charges
tcc = select(tcc, -TotalCharges)

# further explore the relationship between MultipleLines and PhoneService
tcc %>% ggplot(mapping = aes(x = PhoneService, fill = MultipleLines)) +
  geom_bar()

# based the above plot, remove phoneservice
tcc = select(tcc, - PhoneService)

# And rename MultipleLines into MultiplePhoneLines, make its meaning clearer
colnames(tcc)[colnames(tcc) == "MultipleLines"] = "MultiplePhoneLines" 

# further explore the relationship between tenure and Churn
tcc %>% ggplot(mapping = aes(x = tenure, color = Churn)) +
  geom_density() +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80))

# based on the above density plot, create a new feature: CatTenure
tcc$CatTenure = cut(tcc$tenure, breaks = c(-1, 0, 10, 20, 30, 40, 50, 60, 70, 80))
tcc = select(tcc, -tenure)
tcc %>% ggplot(mapping = aes(x = CatTenure, fill = Churn)) +
  geom_bar()   # relationship between CatTenure and Churn

# remove the column customerID
tcc = select(na.omit(tcc), -customerID)

# preprocess the data
#binarize the categorical features: gender, SeniorCitizen, Partner, Dependents,
#MultiplePhioneLines, InternetService, OnlineSecurity, OnlineBackup, DeviceProtection,
#TechSupport,StreamTV,StreamingMovies,Contract,PaperLessBilling,PaymetMethod, CatTenure
gender = model.matrix(~ gender -1, data = tcc)
SeniorCitizen = model.matrix(~ SeniorCitizen -1, data = tcc)
Partner = model.matrix(~ Partner -1, data = tcc)
Dependents  = model.matrix(~ Dependents  -1, data = tcc)
MultiplePhoneLines  = model.matrix(~ MultiplePhoneLines  -1, data = tcc)
InternetService  = model.matrix(~ InternetService  -1, data = tcc)
OnlineSecurity  = model.matrix(~ OnlineSecurity  -1, data = tcc)
OnlineBackup  = model.matrix(~ OnlineBackup  -1, data = tcc)
DeviceProtection  = model.matrix(~ DeviceProtection  -1, data = tcc)
TechSupport  = model.matrix(~ TechSupport  -1, data = tcc)
StreamingTV  = model.matrix(~ StreamingTV  -1, data = tcc)
StreamingMovies  = model.matrix(~ StreamingMovies  -1, data = tcc)
Contract  = model.matrix(~ Contract  -1, data = tcc)
PaperlessBilling  = model.matrix(~ PaperlessBilling  -1, data = tcc)
PaymentMethod  = model.matrix(~ PaymentMethod  -1, data = tcc)
CatTenure  = model.matrix(~ CatTenure  -1, data = tcc)

tcc = cbind(tcc, gender, SeniorCitizen, Partner, Dependents,MultiplePhoneLines, 
            InternetService, OnlineSecurity, OnlineBackup, 
            DeviceProtection, TechSupport, StreamingTV, StreamingMovies, 
            Contract, PaperlessBilling, PaymentMethod, CatTenure)

tcc = select(tcc, -gender, -SeniorCitizen, -Partner, -Dependents, -MultiplePhoneLines, -InternetService, -OnlineSecurity, 
            -OnlineBackup, -DeviceProtection, -TechSupport, -StreamingTV, -StreamingMovies, -Contract, -PaperlessBilling, 
            -PaymentMethod, -CatTenure)


# splitting data
in_train = createDataPartition( y = tcc$Churn, p = 0.8, list = FALSE)
tcc_train = tcc[in_train, ]
tcc_test = tcc[-in_train, ]

str(tcc_train)
str(tcc_test)

# centering and scaling training data for analysis
Preprocess_steps = preProcess(select(tcc_train, MonthlyCharges), 
                              method = c('center', 'scale'))

tcc_train_proc = predict(Preprocess_steps, newdata = tcc_train)
tcc_test_proc = predict(Preprocess_steps, newdata = tcc_test)
head(tcc_train_proc)
head(tcc_test_proc)

# checking for zero-variance features in training data, then remove them
nzv_train <- nearZeroVar(tcc_train_proc, saveMetrics = TRUE) 
is_nzv_train = row.names(nzv_train[nzv_train$nzv == TRUE, ])
is_nzv_train

tcc_train_proc = tcc_train_proc[ , !(colnames(tcc_train_proc) %in% is_nzv_train)]
str(tcc_train_proc)

# identifying correlated predictors in training set, then remove them ????????
#cor_train = cor(tcc_train_proc[, c('tenure', 'MonthlyCharges')])
#highly_cor = findCorrelation(cor_train, cutoff = .75)
#tcc_train_proc = tcc_train_proc[ , -highly_cor]

# classification with logistic regression
# build logistic model
logistic_model = train(Churn ~ ., 
                       data = tcc_train_proc, 
                       method ='glm',
                       family = binomial,
                       tuneLength = 20,
                       trControl = trainControl(method = 'cv', number = 10))

summary(logistic_model)
logistic_model$finalModel

# plot decision boundries
#decisionplot(logistic_model, tcc_train_proc, class = 'Churn')

# plot the variable importance plot
plot(varImp(logistic_model), main = 'varImp plot for logistic_model')

# test predictions for logistic model
logistic_predictions = predict(logistic_model, 
                               newdata = tcc_test_proc)
confusionMatrix(logistic_predictions, 
                tcc_test_proc$Churn)

# plot ROC curve
logistic_ROC = roc(as.ordered(tcc_test_proc$Churn), as.ordered(logistic_predictions))
plot_logistic_ROC = plot.roc(logistic_ROC,  
         xlab = "Specificity (False Positive Rate) - logistic model",
         ylab = "Sensitivity (True Positive Rate)",
         print.auc = TRUE,
         auc.polygon = TRUE
         )


# build Boosted Logistic regression model
boosted_model = train(Churn ~ .,
                      data = tcc_train_proc,
                      method = 'LogitBoost',
                      family = binomial,
                      tuneGrid = expand.grid(nIter = 1:20),
                      trControl = trainControl(method = 'cv', number = 10))

summary(boosted_model)
boosted_model$finalModel
# plot the variable importance plot
plot(varImp(boosted_model), main = 'varImp plot for boosted_model')

# test predictions for boosted model
boosted_predictions = predict(boosted_model, 
                              newdata = tcc_test_proc)
confusionMatrix(boosted_predictions, 
                tcc_test_proc$Churn)

# plot ROC curve
boosted_ROC = roc(as.ordered(tcc_test_proc$Churn), as.ordered(boosted_predictions))
plot_boosted_ROC = plot.roc(boosted_ROC, 
         xlab = "Specificity (False Positive Rate) - boosted model",
         ylab = "Sensitivity (True Positive Rate)",
         print.auc = TRUE,
         auc.polygon = TRUE)


# build lasso model
trainX =select(tcc_train_proc, -Churn)
trainY = tcc_train_proc$Churn

lasso_model = train(trainX,
                    trainY,
                    method ='glmnet',
                    tuneGrid = expand.grid(
                      alpha = seq(.05, 1, length = 20),
                      lambda = c((1:5)/10)),
                    trControl = trainControl(method = 'cv', number = 10))

summary(lasso_model)
lasso_model$finalModel
# plot the variable importance plot
plot(varImp(lasso_model), main = 'varImp plot for lasso_model')


# test predictions for lasso model
lasso_predictions = predict(lasso_model, 
                            newdata = tcc_test_proc)
confusionMatrix(lasso_predictions, 
                tcc_test_proc$Churn)

# plot ROC curve
lasso_ROC = roc(as.ordered(tcc_test_proc$Churn), as.ordered(lasso_predictions))
plot_lasso_ROC = plot.roc(lasso_ROC,  
         xlab = "Specificity (False Positive Rate) - lasso model",
         ylab = "Sensitivity (True Positive Rate)",
         print.auc = TRUE,
         auc.polygon = TRUE)


# build knn model
knn_model = train(Churn ~ ., 
                       data = tcc_train_proc, 
                       method ='knn',
                       #family = binomial,
                       tuneLength = 20,
                       trControl = trainControl(method = 'cv', number = 10))

summary(knn_model)
knn_model$finalModel
plot(knn_model)
# plot the variable importance plot
plot(varImp(knn_model), main = 'varImp plot for knn_model')

# test predictions for knn model
knn_predictions = predict(knn_model, 
                               newdata = tcc_test_proc)
confusionMatrix(knn_predictions, 
                tcc_test_proc$Churn)

# plot ROC curve
knn_ROC = roc(as.ordered(tcc_test_proc$Churn), as.ordered(knn_predictions))
plot_knn_ROC = plot.roc(knn_ROC,  
         xlab = "Specificity (False Positive Rate) - knn model",
         ylab = "Sensitivity (True Positive Rate)",
         print.auc = TRUE,
         auc.polygon = TRUE)

###################below is for decision trees
set.seed(50) 

tcc = read.csv("Telco-Customer-Churn.csv", head = TRUE)


summary(tcc)

str(tcc)

# EDA and feature engineering
#remove total charges
tcc = select(tcc, -TotalCharges)

#remove phoneservice
tcc = select(tcc, - PhoneService)

# And rename MultipleLines into MultiplePhoneLines, make its meaning clearer
colnames(tcc)[colnames(tcc) == "MultipleLines"] = "MultiplePhoneLines" 


# change the type of seniorcitizen from int into factor
tcc$SeniorCitizen = as.factor(tcc$SeniorCitizen)

# create a new feature: CatTenure
#tcc$CatTenure = cut(tcc$tenure, breaks = c(-1, 0, 10, 20, 30, 40, 50, 60, 70, 80))
#tcc = select(tcc, -tenure)

# remove the column customerID
tcc = select(na.omit(tcc), -customerID)

# preprocess the data
# splitting data
in_train = createDataPartition( y = tcc$Churn, p = 0.8, list = FALSE)
tcc_train = tcc[in_train, ]
tcc_test = tcc[-in_train, ]

str(tcc_train)
str(tcc_test)

# keep all the features in train set except for the target variable
train_set = select(tcc_train, -Churn) 

# grow one tree
tree_model = train(y = tcc_train$Churn,
                   x = train_set,
                   method = 'rpart',
                   tuneLength = 20,
                   trControl = trainControl(method = 'cv', number = 10))

tree_model$finalModel

# plot the tree
library(rpart.plot)
#rpart.plot(tree_model$finalModel)
prp(tree_model$finalModel, extra = 106, box.palette = "auto") # extra=106 class model with a binary response

# plot the fancy tree
#library(rattle)
#fancyRpartPlot(tree_model$finalModel)

# plot the variable importance plot
plot(varImp(tree_model), main = 'varImp plot for tree_model')

# test predictions for tree model
tree_predictions = predict(tree_model, newdata = tcc_test)
confusionMatrix(tree_predictions, tcc_test$Churn)

# plot ROC curve
tree_ROC = roc(as.ordered(tcc_test$Churn), as.ordered(tree_predictions))
plot_tree_ROC = plot.roc(tree_ROC, 
         xlab = "Specificity (False Positive Rate)- tree model",
         ylab = "Sensitivity (True Positive Rate)",
         #print.auc = TRUE,
         auc.polygon = TRUE,
         add = TRUE)

# build bagged CART
bagged_model = train(y = tcc_train$Churn,
                     x = train_set,
                     method = 'treebag',
                     tuneLength = 20,
                     trControl = trainControl(method = 'cv', number = 10))

bagged_model$finalModel

# plot the variable importance plot
plot(varImp(bagged_model), main = 'varImp plot for bagged model')

# test predictions for bagged CART model
bagged_predictions = predict(bagged_model, newdata = tcc_test)
confusionMatrix(bagged_predictions, tcc_test$Churn)

# plot ROC curve
bagged_ROC = roc(as.ordered(tcc_test$Churn), as.ordered(bagged_predictions))
plot_bagged_ROC = plot.roc(bagged_ROC, 
         xlab = "Specificity (False Positive Rate) - bagged model",
         ylab = "Sensitivity (True Positive Rate)",
         print.auc = TRUE,
         auc.polygon = TRUE,
         add = TRUE,
         max.auc.polygon.col = 'red')

#compare the models
results = resamples(
  list(logistic_model = logistic_model,
       boosted_model = boosted_model,
       lasso_model = lasso_model,
       knn_model = knn_model,
       tree_model = tree_model,
       bagged_model = bagged_model))

dotplot(results)

# compare the ROC curves for all the six models
ggroc(list(logistic_model = logistic_ROC,
           boosted_model = boosted_ROC,
           lasso_model = lasso_ROC,
           knn_model = knn_ROC,
           tree_model = tree_ROC,
           bagged_model = bagged_ROC),
      alpha = 0.5)

#compare the ROC curves for logistic_model and tree model
plot.roc(logistic_ROC,  
         xlab = "Specificity (False Positive Rate) - logistic model vs tree model",
         ylab = "Sensitivity (True Positive Rate)",
         #print.auc = TRUE,
         auc.polygon = TRUE
)

plot.roc(tree_ROC, 
         xlab = "Specificity (False Positive Rate)- tree model",
         ylab = "Sensitivity (True Positive Rate)",
         #print.auc = TRUE,
         #auc.polygon = TRUE,
         add = TRUE)

#library(gridExtra)
#grid.arrange(grobTree(plot_logistic_ROC), grobTree(plot_boosted_ROC)),
#                          plot_lasso_ROC,
#                          plot_knn_ROC,
#                          plot_tree_ROC,
#                          plot_bagged_ROC)
#             )

summary(results)
# conclusion: tree model is the best model

# Kappa is always less than or equal to 1. A value of 1 implies perfect agreement 
# and values less than 1 imply less than perfect agreement.
