#------------------------------------------------------------------------------------#
#                                                                                    #
# Project: Diabetes Detection System for HarvardX: Data Science: Capstone course     #                                 
# Author: Xiaobin Hu                                                                 #
# Date: October 31, 2019                                                             #
#                                                                                    #
#------------------------------------------------------------------------------------#

# Load all necessary libraries.
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(ggpubr)
library(grid)
library(gridExtra)
library(sjmisc)
library(mlr)
library(earth)
library(xgboost)
library(mboost)
library(corrplot)

#------------------------------------------------------------------------------------#
#                                                                                    #
#                             Data Preparation                                       #                                 
#                                                                                    #
#------------------------------------------------------------------------------------#

# Data Aquistion
# Download Pima Indian Diabetes dataset from Internet
dl <- tempfile()
url <- "https://gist.githubusercontent.com/ktisha/c21e73a1bd1700294ef790c56c8aec1f/raw/819b69b5736821ccee93d05b51de0510bea00294/pima-indians-diabetes.csv"
download.file(url, dl)
pima_diabetes_dat <- data.frame(readr::read_csv(dl, skip = 9, col_names = FALSE))

# Remove uncessary object
rm(dl)

setwd("C:\\PROJECTS\\R_Programs\\capstone-project-2")
# Check the data
# Assign names to the columns
colnames(pima_diabetes_dat) <- c("num_Of_pregnance", 
                                 "plasma_glucose_concentration", 
                                 "diastolic_blood_pressure",
                                 "tricep_skin_fold_thickness",
                                 "serum_insulin",
                                 "bmi",
                                 "diabetes_pedigree_function",
                                 "age",
                                 "diabetes")

# Change diabetes variable to factor
pima_diabetes_dat$diabetes <- factor(pima_diabetes_dat$diabetes)

# Understand Data
dim(pima_diabetes_dat)
head(pima_diabetes_dat)       

p_nog_1 <- pima_diabetes_dat %>%
  ggplot(aes(x = factor(1), y = num_Of_pregnance)) +
  geom_boxplot(width = 0.4, fill = "#FFDB6D", color = "#C4961A") +
  geom_jitter(width = 0.1, size = 1, color = "#00AFBB") +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  labs(title = "Number of Pregrance",
    x = NULL,
    y = NULL) +
  theme(plot.title = element_text(hjust = 0.5))

p_pgc_1 <- pima_diabetes_dat %>%
  ggplot(aes(x = factor(1), y = plasma_glucose_concentration)) +
  geom_boxplot(width = 0.4, fill = "#FFDB6D", color = "#C4961A") +
  geom_jitter(width = 0.1, size = 1, color = "#00AFBB") +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  labs(title = "Plasma Glucose Concentration",
       x = NULL,
       y = NULL) +
  theme(plot.title = element_text(hjust = 0.5))

p_dbp_1 <- pima_diabetes_dat %>%
  ggplot(aes(x = factor(1), y = diastolic_blood_pressure)) +
  geom_boxplot(width = 0.4, fill = "#FFDB6D", color = "#C4961A") +
  geom_jitter(width = 0.1, size = 1, color = "#00AFBB") +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  labs(title = "Diastolic Blood Pressure",
       x = NULL,
       y = NULL) +
  theme(plot.title = element_text(hjust = 0.5))

p_tsft_1 <- pima_diabetes_dat %>%
  ggplot(aes(x = factor(1), y = tricep_skin_fold_thickness)) +
  geom_boxplot(width = 0.4, fill = "#FFDB6D", color = "#C4961A") +
  geom_jitter(width = 0.1, size = 1, color = "#00AFBB") +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  labs(title = "Tricep Skin Fold Thickness",
       x = NULL,
       y = NULL) +
  theme(plot.title = element_text(hjust = 0.5))

p_si_1 <- pima_diabetes_dat %>%
  ggplot(aes(x = factor(1), y = serum_insulin)) +
  geom_boxplot(width = 0.4, fill = "#FFDB6D", color = "#C4961A") +
  geom_jitter(width = 0.1, size = 1, color = "#00AFBB") +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  labs(title = "Serum Insulin",
       x = NULL,
       y = NULL) +
  theme(plot.title = element_text(hjust = 0.5))

p_bmi_1 <- pima_diabetes_dat %>%
  ggplot(aes(x = factor(1), y = bmi)) +
  geom_boxplot(width = 0.4, fill = "#FFDB6D", color = "#C4961A") +
  geom_jitter(width = 0.1, size = 1, color = "#00AFBB") +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  labs(title = "BMI",
       x = NULL,
       y = NULL) +
  theme(plot.title = element_text(hjust = 0.5))


p_dpf_1 <- pima_diabetes_dat %>%
  ggplot(aes(x = factor(1), y = diabetes_pedigree_function)) +
  geom_boxplot(width = 0.4, fill = "#FFDB6D", color = "#C4961A") +
  geom_jitter(width = 0.1, size = 1, color = "#00AFBB") +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  labs(title = "Diabetes Pedigree Function",
       x = NULL,
       y = NULL) +
  theme(plot.title = element_text(hjust = 0.5))

p_age_1 <- pima_diabetes_dat %>%
  ggplot(aes(x = factor(1), y = age)) +
  geom_boxplot(width = 0.4, fill = "#FFDB6D", color = "#C4961A") +
  geom_jitter(width = 0.1, size = 1, color = "#00AFBB") +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) + 
  labs(title = "Age",
       x = NULL,
       y = NULL) +
  theme(plot.title = element_text(hjust = 0.5))

grid.arrange(p_nog_1, p_pgc_1, p_dbp_1, p_tsft_1, p_si_1, p_bmi_1,p_dpf_1, p_age_1,
             nrow = 4,
             top = "Feteature Distribution",
             left = "")

# Data Validation and Transformation

# Check if there is any duplicated observations
dupes <- duplicated(pima_diabetes_dat)
table(dupes)
sum(dupes == TRUE)
# There is no duplicate observation in the dataset.

# Check if there is any zero or near-zero variance features.
feature_variance <- caret::nearZeroVar(pima_diabetes_dat, saveMetrics = TRUE)
feature_variance %>%
  as_tibble()
sum(feature_variance$zeroVar == TRUE | feature_variance$nzv == TRUE)
# There is no featuere with zero or near-zero variance

# Find correlations between features
df_corr <- cor(pima_diabetes_dat[!colnames(pima_diabetes_dat) %in% c("diabetes")], method = "spearman")
high_corr <- caret::findCorrelation(df_corr, cutoff = 0.9)
length(high_corr)

# Display the feature correlations in a plot
corrplot(df_corr, method = "number")

# Find if there is any linear combinations.
linear_combos <- caret::findLinearCombos(pima_diabetes_dat[!colnames(pima_diabetes_dat) %in% c("diabetes")])
length(linear_combos$linearCombos)
# There is no linear combination found.

# Basice descriptive statistic information
pima_diabetes_dat %>%
  sjmisc::descr() %>%
  as_tibble()
# There is no NA values found for all features, this is very good.
# But some features have zero values, which should be treated as missing values.

# How many observations with plasma glucose_contration = 0?
# They are missing values.
# I will use mean and median of this feature for imputation.
sum(pima_diabetes_dat$plasma_glucose_concentration == 0)

# There are 5 obvervation which don't have plasm glucose concentration values.
# Use mean of plasma glucose concentration for imputation
pima_diabetes_dat <- pima_diabetes_dat%>%
  mutate(plasma_glucose_concentration_clean = ifelse(plasma_glucose_concentration == 0, 
                                                      mean(plasma_glucose_concentration), 
                                                      plasma_glucose_concentration))

# How many observations with diastolic blood pressure = 0
sum(pima_diabetes_dat$diastolic_blood_pressure == 0)

# There are 35 observetions with diastolic blood pressure value missing.

# Use mean of diastolic blood pressure for imputation
pima_diabetes_dat <- pima_diabetes_dat%>%
  mutate(diastolic_blood_pressure_clean = ifelse(diastolic_blood_pressure == 0, 
                                                  mean(diastolic_blood_pressure), 
                                                  diastolic_blood_pressure))

# How many observations with tricep skin fold thickness value missing?
sum(pima_diabetes_dat$tricep_skin_fold_thickness == 0)
percentage_nv <- round((sum(pima_diabetes_dat$tricep_skin_fold_thickness == 0) / nrow(pima_diabetes_dat)) * 100,
                       2)

percentage_nv
# There are 227 observations which have no tricep skin fold thickness value.
# I will drop this feature from the dataset, because imputation does not make sense
#   due to the large volume of missing values.

# How many observation which have zero values for serum insulin?
sum(pima_diabetes_dat$serum_insulin == 0)

# There are 374 observations, almost half, don't have serum insulin. 
# I will not use this feature for machine learing.

# How many observeations which have zero values for bmi?
sum(pima_diabetes_dat$bmi == 0)

# There are 11 observations with bmi equal to zero.
# Use mean of diastolic blood pressure for imputation
pima_diabetes_dat <- pima_diabetes_dat%>%
  mutate(bmi_clean = ifelse(bmi == 0, mean(bmi), bmi))

# Check how many diabetes confirmed cases in the dataset
pima_diabetes_dat %>%
  dplyr::group_by(diabetes) %>%
  dplyr::summarise(num_rows = n())

# What is the percentage of confirmed diabetes in the dataset
mean(pima_diabetes_dat$diabetes == "1") * 100

# I will drop the following features from the dataset, 
#   before I feed the dataset to machine learning algorythms
# 

diabetes_dat_filtd <- pima_diabetes_dat %>%
  select(-c("tricep_skin_fold_thickness",
            "serum_insulin"))


head(diabetes_dat_filtd)

# Create a sudirectory to save the data file
if(!dir.exists(file.path(getwd(), "data"))){
  dir.create(file.path(getwd(), "data"), recursive = TRUE)
}

# Save edx dataset to RDada file, so we can load data from local file later.
saveRDS(pima_diabetes_dat, "data/pima_diabetes_dat.rds")
saveRDS(diabetes_dat_filtd, "data/diabetes_dat_filtd.rds")

# Understand the Data

# Plot the distributions of the features

fill <- 'skyblue3'
color <- 'grey'

# Plot distribution of number of pregrance
bw <- 1
plot_nog <- diabetes_dat_filtd %>%
  ggplot(aes(num_Of_pregnance)) +
  geom_histogram(
    aes(fill = I(fill),
        color = I(color)),
    binwidth = bw,
    show.legend = FALSE)  +
  labs( title = "Distribution of number of pregrance")

bw <- 10
plot_pgc <- diabetes_dat_filtd %>%
  ggplot(aes(plasma_glucose_concentration_clean)) +
  geom_histogram(
    aes(fill = I(fill),
        color = I(color)),
    binwidth = bw,
    show.legend = FALSE)  +
  labs( title = "Distribution of plasma glucose concentration")

# plot_pgc

bw <- 5
plot_dbp <- diabetes_dat_filtd %>%
  ggplot(aes(diastolic_blood_pressure_clean)) +
  geom_histogram(
    aes(fill = I(fill),
        color = I(color)),
    binwidth = bw,
    show.legend = FALSE)  +
  labs( title = "Distribution of diastolic blood pressure") +
  theme_linedraw()

# plot_dbp

bw <- 2
plot_bmi <- diabetes_dat_filtd %>%
  ggplot(aes(bmi_clean)) +
  geom_histogram(
    aes(fill = I(fill),
        color = I(color)),
    binwidth = bw,
    show.legend = FALSE)  +
  labs( title = "Distribution of bmi")

# plot_bmi

bw <- 0.2
plot_dpf <- diabetes_dat_filtd %>%
  ggplot(aes(diabetes_pedigree_function)) +
  geom_histogram(
    aes(fill = I(fill),
        color = I(color)),
    binwidth = bw,
    show.legend = FALSE)  +
  labs( title = "Distribution of diabetes pedigree function")

# plot_dpf
bw <- 3
plot_age <- diabetes_dat_filtd %>%
  ggplot(aes(age)) +
  geom_histogram(
    aes(fill = I(fill),
        color = I(color)),
    binwidth = bw,
    show.legend = FALSE)  +
  labs( title = "Distribution of age")

# plot_age

grid.arrange(plot_nog, plot_pgc, plot_dbp, plot_bmi,plot_dpf, plot_age,
             nrow = 3,
             top = "Feteature Distribution",
             left = ""
)

# Split the dataset into 2 parts: 70% of the data will be used for training.
# The rest of the data will be used for testing

set.seed(2007, sample.kind = "Rounding")
train_index <- createDataPartition(diabetes_dat_filtd$diabetes, times = 1, p = 0.8, list = FALSE)
train_set <- diabetes_dat_filtd[train_index, ]
test_set <- diabetes_dat_filtd[-train_index, ]

# Check the percentage of data in training dataset
nrow(train_set) / nrow(pima_diabetes_dat)

# The percentage of diabetes confirmed should be close in both traing and testing dataset
pct_train <- round((nrow(train_set) / nrow(diabetes_dat_filtd)) * 100, 2)
original_diabetes_ratio <- round(mean(diabetes_dat_filtd$diabetes == 1) * 100, 2)
traing_diabetes_ratio <- round(mean(train_set$diabetes == 1) * 100, 2)
test_diabetes_ratio <- round(mean(test_set$diabetes == 1) * 100, 2)
original_diabetes_ratio
traing_diabetes_ratio
test_diabetes_ratio

# Create a sudirectory to save the data file
if(!dir.exists(file.path(getwd(), "data"))){
  dir.create(file.path(getwd(), "data"), recursive = TRUE)
}

# Save training dataset to RDada file, so we can load data from local file later.
saveRDS(train_set, "data/diabetes_train_set.rds")

# Save testing dataset to RData file
saveRDS(test_set, "data/diabetes_test_set.rds")


# When you resume your work, you can uncomment the following code 
#   to repopulate datasets by loading data from local rdata files.
# train_set <- readr::read_rds("data/diabetes_train_set.rds")
# test_set <- readr::read_rds("data/diabetes_test_set.rds")

#------------------------------------------------------------------------------------#
#                                                                                    #
#                             Model Building and Results                             #                                 
#                                                                                    #
#------------------------------------------------------------------------------------#

#--------------------------------------------------------#
# Find the best candidate machine learning algorythms.   #
#------------------------------------------------------- #

# Apply all of these models using train with all the default parameters. 
set.seed(1, sample.kind = "Rounding")

models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

# Run the following code to train the various models:

fits <- lapply(models, function(model){
  print(model)
  caret::train(diabetes ~ ., method = model, data = train_set)
})

# Assing model names as the column names
names(fits) <- models

# View(fits)

# Use all models to predict y
diabetes_hat_all <- sapply(fits, function(fit){
  diabetes_hat <- predict(fit, test_set)
})

# Convert diabetes_hat_all from matrix to data frame
diabetes_hat_all_df <- data.frame(diabetes_hat_all)

accuracy_all <- sapply(diabetes_hat_all_df, function(y_hat){
  diabetes_hat <- factor(y_hat)
  confusionMatrix(diabetes_hat, reference = test_set$diabetes)$overall["Accuracy"]
})


View(accuracy_all)

# The accuracy of naive_bayes, gamLoess, qda, adaboost, and rf are all above 0.75. 
# The best one is rf method equal to 0.7777778.

y_hat_all_df


#--------------------------------------------------------#
# Use Ensemble to Build Stacked Models                   #
#------------------------------------------------------- #
# I will use differnt combination of base models to build stacked models,
#   to check stacked models can provide better accuracy.

# Try ensemble approach by book - experiment 1
# listLearners("classif", check.packages = TRUE, properties = "missings")[c("class","package")]
diabetes_task <- mlr::makeClassifTask(data = train_set, target = "diabetes")
base <- c("classif.randomForest", "classif.naiveBayes")

learns <- lapply(base, makeLearner)
learns <- lapply(learns, setPredictType, "prob")
sl <- 
  mlr::makeStackedLearner(
    base.learners = learns,
    super.learner = "classif.gbm",
    predict.type = "prob",
    method = "stack.cv"
  )

# Now train our stacked model
stacked_fit <- mlr::train(sl, diabetes_task)

# We establsih the predicted probabilities for the test data:
pred_stacked <- predict(stacked_fit, newdata = test_set)
mlr::calculateConfusionMatrix(pred_stacked)
mlr::performance(pred_stacked, measures = list(acc, logloss))

# Then acuuracy is 0.7712418, no better than single random forest model
mlr::performance(pred_stacked, measures = list(acc, logloss))["acc"]


# Creating an ensemble - experiment 2

base <- c("classif.randomForest", "classif.lda")
learns <- lapply(base, makeLearner)
learns <- lapply(learns, setPredictType, "prob")
sl <- 
  mlr::makeStackedLearner(
    base.learners = learns,
    super.learner = "classif.glmnet",
    predict.type = "prob",
    method = "stack.cv"
  )

# Now train our stacked model
stacked_fit <- mlr::train(sl, diabetes_task)

# We establsih the predicted probabilities for the test data:
pred_stacked <- predict(stacked_fit, newdata = test_set)
mlr::calculateConfusionMatrix(pred_stacked)
mlr::performance(pred_stacked, measures = list(acc, logloss))

# Then acuuracy is 0.7647059, no better than single random forest model
mlr::performance(pred_stacked, measures = list(acc, logloss))["acc"]

# Creating an ensemble - experiment 3

base <- c("classif.gbm", "classif.randomForest")

learns <- lapply(base, makeLearner)
learns <- lapply(learns, setPredictType, "prob")
sl <- 
  mlr::makeStackedLearner(
    base.learners = learns,
    super.learner = "classif.lda",
    predict.type = "prob",
    method = "stack.cv"
  )

# Now train our stacked model
stacked_fit <- mlr::train(sl, diabetes_task)

# We establsih the predicted probabilities for the test data:
pred_stacked <- predict(stacked_fit, newdata = test_set)
mlr::calculateConfusionMatrix(pred_stacked)
mlr::performance(pred_stacked, measures = list(acc, logloss))


#--------------------------------------------------------#
# Tune Hyper Parameters for Random Forest Model          #
#------------------------------------------------------- #



gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

fitControl <- trainControl(method = "repeatedcv"
                           ,
                           number = 3,
                           repeats = 5)

set.seed(42, sample.kind = "Rounding")

gbm_model_diabetes_grid <- caret::train(diabetes ~ .,
                               data = train_set,
                               method = "gbm",
                               trControl = fitControl,
                               verbose = FALSE,
                               tuneGrid = gbmGrid)

gbm_model_diabetes_grid


plot(gbm_model_diabetes_grid)

y_hat_by_glm_cv <- predict(gbm_model_diabetes_grid, test_set)
confusionMatrix(y_hat_by_glm_cv, reference = test_set$diabetes)$overall["Accuracy"]

# Manual Search
control <- trainControl(method="repeatedcv", number=5, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(sqrt(ncol(train_set))))
modellist <- list()
acc_List <- list()
metric <- "Accuracy"  

for (ntree in c(1:100)) {
  set.seed(7, sample.kind = "Rounding")
  fit <- caret::train(diabetes ~ ., data=train_set, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, ntree=ntree)
  y_hat <- predict(fit, test_set)
  acc <- confusionMatrix(y_hat, reference = test_set$diabetes)$overall["Accuracy"]
  key <- toString(ntree)
  modellist[[key]] <- fit
  print(paste("key = ", key, ", acc = ", acc))
  acc_List <- c(acc_List, acc)
}

temp_acc_df <- as_vector(acc_List)

temp_acc_df

acc_df <- data.frame("ntree" = c(1:100),
                     "accuracy" = temp_acc_df)

acc_df %>%
  ggplot(aes(x = ntree, y = accuracy)) +
  geom_line(color = "dodgerblue4", size = 1 ) +
  theme_linedraw()

best_acc <- max(acc_df$accuracy)

acc_df[which.max(acc_df$accuracy),"ntree"]


#--------------------------------------------------------#
#                       Then  End                        #
#------------------------------------------------------- #
