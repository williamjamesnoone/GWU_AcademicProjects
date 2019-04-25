#Machine Learning I
#Final Project
#Author: Will Nooone


# Load Libraries ----------------------------------------------------------

library(xlsx)
library(glmnet)
library(ISLR)
library(plotmo)
library(gradDescent)
library(caret)
library(dplyr)
library(zoo)


# Load Raw Data ---------------------------------------------------------------

#load data as .xlsx
setwd("C:/Users/wnoone/Google Drive/MS Data Science/DATS 6202 Machine Learning I/Final Project/Code/Data")
file <- ("PROJECT_XLSX.xlsx")
data_raw <- read.xlsx(file, 1)  

#check data structure and type declarations
str(data_raw)
summary(data_raw)
View(data_raw)


# Subset Data -----------------------------------

#create vector of column names for subsetting
colnames(data_raw)
col_names <- c('DATE','TICKER','FIRM','SECTOR','PX_LAST','HISTORICAL_MARKET_CAP','SALES_REV_TURN','NET_INCOME',
               'BS_CUR_ASSET_REPORT','BS_TOT_NON_CUR_ASSET','BS_CUR_LIAB','NON_CUR_LIAB','TOTAL_EQUITY',
               'ESG_DISCLOSURE_SCORE','ENVIRON_DISCLOSURE_SCORE','DIRECT_CO2_EMISSIONS', 'INADIRECT_CO2_EMISSIONS', 
               'TOTAL_CO2_EMISSIONS','CO2_INATENASITY','TOTAL_GHG_EMISSIONS','NOX_EMISSIONS','CARBON_MONOXIDE_EMISSIONS', 
               'SO2_EMISSIONS', 'ENERGY_CONSUMPTION','TOTAL_WATER_USE' ,'HAZARDOUS_WASTE','TOTAL_WASTE',
               'PAPER_CONSUMPTION','NUM_ENVIRON_FINES','ENVIRON_FINES_AMT','SOCIAL_DISCLOSURE_SCORE', 'NUMBER_EMPLOYEES_CSR', 
               'EMPLOYEE_TURNOVER_PCT','PCT_EMPLOYEES_UNIONIZED','PCT_WOMEN_EMPLOYEES','PCT_WOMEN_MGT',
               'PCT_MINORITY_EMPLOYEES','PCT_MINORITY_MGT','COMMUNITY_SPENDING','GOVNCE_DISCLOSURE_SCORE','BOARD_SIZE', 
               'INDEPENDENT_DIRECTORS','PCT_INDEPENDENT_DIRECTORS','BOARD_DURATION','BOARD_MEETINGS_PER_YR',
               'BOARD_MEETING_ATTENDANCE_PCT','POLITICAL_DONATIONS')

#create unscaled and scaled data subsets
data_unimputed <- data.frame(data_raw[col_names])
summary(data_unimputed)


# Data Imputation ---------------------------------------------------------

#impute NA over all zeros
data_unimputed[data_unimputed == 0] <- NA
summary(data_unimputed)
str(data_unimputed)
View(data_unimputed)

#create function to impute median for NA values
median_impute=function(x){
  x[is.na(x)] <- median(x, na.rm=TRUE) 
  x 
}

data_imputed <- data_unimputed %>% group_by(TICKER) %>% mutate_if(is.numeric, median_impute)
View(data_imputed)

#re-impute Zero for remaining NA
data_imputed[is.na(data_imputed)] <- 0
View(data_imputed)
str(data_imputed)
summary(data_imputed)

#scale imputed data 
data_scaled  <- data_imputed %>% group_by(TICKER) %>% mutate_if(is.numeric, scale)
str(data_scaled)
summary(data_scaled)



# Create Feature Set -----------------------------

feature_names <- c('SALES_REV_TURN','NET_INCOME',
               'BS_CUR_ASSET_REPORT','BS_TOT_NON_CUR_ASSET','BS_CUR_LIAB','NON_CUR_LIAB','TOTAL_EQUITY',
               'ESG_DISCLOSURE_SCORE','ENVIRON_DISCLOSURE_SCORE','DIRECT_CO2_EMISSIONS', 'INADIRECT_CO2_EMISSIONS', 
               'TOTAL_CO2_EMISSIONS','CO2_INATENASITY','TOTAL_GHG_EMISSIONS','NOX_EMISSIONS','CARBON_MONOXIDE_EMISSIONS', 
               'SO2_EMISSIONS', 'ENERGY_CONSUMPTION','TOTAL_WATER_USE' ,'HAZARDOUS_WASTE','TOTAL_WASTE',
               'PAPER_CONSUMPTION','NUM_ENVIRON_FINES','ENVIRON_FINES_AMT','SOCIAL_DISCLOSURE_SCORE', 'NUMBER_EMPLOYEES_CSR', 
               'EMPLOYEE_TURNOVER_PCT','PCT_EMPLOYEES_UNIONIZED','PCT_WOMEN_EMPLOYEES','PCT_WOMEN_MGT',
               'PCT_MINORITY_EMPLOYEES','PCT_MINORITY_MGT','COMMUNITY_SPENDING','GOVNCE_DISCLOSURE_SCORE','BOARD_SIZE', 
               'INDEPENDENT_DIRECTORS','PCT_INDEPENDENT_DIRECTORS','BOARD_DURATION','BOARD_MEETINGS_PER_YR',
               'BOARD_MEETING_ATTENDANCE_PCT','POLITICAL_DONATIONS')

# Standard OLS Regression -------------------------------------------------

ols_bloom <- lm(PX_LAST~
                   SALES_REV_TURN +
                   NET_INCOME +
                   BS_CUR_ASSET_REPORT + 
                   BS_TOT_NON_CUR_ASSET + 
                   BS_CUR_LIAB + 
                   NON_CUR_LIAB + 
                   TOTAL_EQUITY +
                   ESG_DISCLOSURE_SCORE + 
                   ENVIRON_DISCLOSURE_SCORE + 
                   DIRECT_CO2_EMISSIONS + 
                   INADIRECT_CO2_EMISSIONS + 
                   TOTAL_CO2_EMISSIONS + 
                   CO2_INATENASITY + 
                   TOTAL_GHG_EMISSIONS + 
                   NOX_EMISSIONS + 
                   CARBON_MONOXIDE_EMISSIONS +
                   SO2_EMISSIONS + 
                   ENERGY_CONSUMPTION + 
                   TOTAL_WATER_USE + 
                   HAZARDOUS_WASTE + 
                   TOTAL_WASTE +
                   PAPER_CONSUMPTION + 
                   NUM_ENVIRON_FINES + 
                   ENVIRON_FINES_AMT + 
                   SOCIAL_DISCLOSURE_SCORE + 
                   NUMBER_EMPLOYEES_CSR +  
                   EMPLOYEE_TURNOVER_PCT + 
                   PCT_EMPLOYEES_UNIONIZED + 
                   PCT_WOMEN_EMPLOYEES + 
                   PCT_WOMEN_MGT +
                   PCT_MINORITY_EMPLOYEES + 
                   PCT_MINORITY_MGT + 
                   COMMUNITY_SPENDING + 
                   GOVNCE_DISCLOSURE_SCORE + 
                   BOARD_SIZE +
                   INDEPENDENT_DIRECTORS + 
                   PCT_INDEPENDENT_DIRECTORS + 
                   BOARD_DURATION + 
                   BOARD_MEETINGS_PER_YR +
                   BOARD_MEETING_ATTENDANCE_PCT + 
                   POLITICAL_DONATIONS
                 , data = data_imputed)
summary(ols_imputed) 

ols_bloom_coef <- data.frame(ols_bloom$coefficients)
View(ols_bloom_coef)

ols_bloom_pred <- predict(ols_bloom, data_imputed)
head(ols_bloom_pred)
head(data_imputed$PX_LAST)


# LASSO Regression --------------------------------------------------------

#Create Index to Train/Test Split
n_obs = dim(data_scaled)[1]
n_obs
prop_split = 0.66
train_index = sample(1:n_obs, round(n_obs * prop_split))

#Here we are omitting the target variable
predictors <- data_imputed[feature_names]
head(predictors)

#Here we are creating the target variable 
target <- data_imputed$PX_LAST
head(target)

#Lasso also Regression Requires a Matrix vice a Dataframe, just like ridge
predictors <- model.matrix(SALES_REV_TURN~., predictors)
str(predictors)
head(predictors)
pred_tr = predictors[train_index,]
pred_te = predictors[-train_index,]
target_tr = target[train_index]
target_te = target[-train_index]

set.seed(101)
#here we are going to train our lambda then embed it into our lasso model 
cv.bloom.lasso <- cv.glmnet(pred_tr, target_tr, 
                            family="gaussian", 
                            alpha=1, nlambda=100, 
                            lambda.min.ratio=.0001)

coef(cv.bloom.lasso, s=cv.bloom.lasso$lambda.1se)

#if we embed it back into another lasso we get the same result
cv.white.lasso.1 <- glmnet(pred_tr, target_tr, alpha=1,lambda = cv.bloom.lasso$lambda.1se)

#same coefs

View(coef(cv.bloom.lasso.1))

plot(cv.bloom.lasso, xvar = 'lambda')


# Performance of Models ---------------------------------------------------

#Compare models using RMSE
#predict with OLS, GD and Ridge and see what happens  

y_hat_ols <- predict(ols_bloom, data_imputed)
RMSE_OLS <- sqrt(mean((data_imputed$PX_LAST-y_hat_ols)^2))
RMSE_OLS


# y_hat_gd <- pred_GD_whitev1
# RSME_gd <- sqrt(mean((gd_white_te-y_hat_gd)^2))
# RSME_gd
# 
# y_hat_ridge <- predict(cv.white.ridge, pred_te)
# RMSE_Ridge <- sqrt(mean((target_te-y_hat_ridge)^2))
# RMSE_Ridge 

y_hat_lasso <- predict(cv.bloom.lasso, pred_te)
RMSE_Lasso <- sqrt(mean((target_te-y_hat_lasso)^2)) 
RMSE_Lasso
