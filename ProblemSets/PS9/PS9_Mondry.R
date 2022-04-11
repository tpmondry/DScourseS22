library(tidyverse)
library(tidymodels)
library(magrittr)
library(glmnet)

#read in data
housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names = FALSE)
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

set.seed(123456)

#test-train split
housing_split <- initial_split(housing, prop = 0.8)
housing_train <- training(housing_split)
housing_test  <- testing(housing_split)

############################
### data prep
############################

# create recipe
housing_recipe <- recipe(medv ~ ., data = housing) %>%
  # convert outcome variable to logs
  step_log(all_outcomes()) %>%
  # convert 0/1 chas to a factor
  step_bin2factor(chas) %>%
  # create interaction term between crime and nox
  step_interact(terms = ~ crim:zn:indus:rm:age:rad:tax:
                      ptratio:b:lstat:dis:nox) %>%
  # create square terms of some continuous variables
  step_poly(crim,zn,indus,rm,age,rad,tax,ptratio,b,
               lstat,dis,nox,degree=6)
# Run the recipe
housing_prep <- housing_recipe %>% prep(housing_train, retain = TRUE)
housing_train_prepped <- housing_prep %>% juice
housing_test_prepped <- housing_prep %>% bake(new_data = housing_test)
# create x and y training and test data
housing_train_x <- housing_train_prepped %>% select(-medv)
housing_test_x <- housing_test_prepped %>% select(-medv)
housing_train_y <- housing_train_prepped %>% select(medv)
housing_test_y <- housing_test_prepped %>% select(medv)

#############################
### LASSO
#############################

### tuning

tune_spec <- linear_reg(
  penalty = tune(), # tuning parameter
  mixture = 1       # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

# define a grid
lambda_grid <- grid_regular(penalty(), levels = 50)

# 6-fold CV
rec_folds <- vfold_cv(housing_train_prepped, v = 6)

# Workflow
rec_wf <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec)

# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

# finding best result
top_rmse  <- show_best(rec_res, metric = "rmse")

# getting optimal penalty param
lasso_lambda <- top_rmse$penalty[1]

### estimating
lasso_spec <- linear_reg(penalty=lasso_lambda,
                         mixture=1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

lasso_fit <- lasso_spec %>%
  fit(medv ~ ., data=housing_train_prepped)

# RMSE in sample
lasso_RMSE_insample <- lasso_fit %>% predict(housing_train_prepped) %>%
  mutate(truth = housing_train_prepped$medv) %>%
  rmse(truth,`.pred`) %>%
  `[`('.estimate') %>% as.numeric

# RMSE out of sample
lasso_RMSE_outofsample <- lasso_fit %>% predict(housing_test_prepped) %>%
  mutate(truth = housing_test_prepped$medv) %>%
  rmse(truth,`.pred`) %>%
  `[`('.estimate') %>% as.numeric

#############################
### ridge
#############################

### tuning

tune_spec <- linear_reg(
  penalty = tune(), # tuning parameter
  mixture = 0       # 1 = lasso, 0 = ridge
) %>% 
  set_engine("glmnet") %>%
  set_mode("regression")

# define a grid
lambda_grid <- grid_regular(penalty(), levels = 50)

# 6-fold CV
rec_folds <- vfold_cv(housing_train_prepped, v = 6)

# Workflow
rec_wf <- workflow() %>%
  add_formula(log(medv) ~ .) %>%
  add_model(tune_spec)

# Tuning results
rec_res <- rec_wf %>%
  tune_grid(
    resamples = rec_folds,
    grid = lambda_grid
  )

# finding best result
top_rmse  <- show_best(rec_res, metric = "rmse")

# getting optimal penalty param
ridge_lambda <- top_rmse$penalty[1]

### estimating
ridge_spec <- linear_reg(penalty=ridge_lambda,
                         mixture=1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

ridge_fit <- ridge_spec %>%
  fit(medv ~ ., data=housing_train_prepped)

# RMSE in sample
ridge_RMSE_insample <- ridge_fit %>% predict(housing_train_prepped) %>%
  mutate(truth = housing_train_prepped$medv) %>%
  rmse(truth,`.pred`) %>%
  `[`('.estimate') %>% as.numeric

# RMSE out of sample
ridge_RMSE_outofsample <- ridge_fit %>% predict(housing_test_prepped) %>%
  mutate(truth = housing_test_prepped$medv) %>%
  rmse(truth,`.pred`) %>%
  `[`('.estimate') %>% as.numeric

####################
### generate results table
####################

# start building table
results <- data.frame(model = c('LASSO', 'ridge'),
                      lambda = c(lasso_lambda, ridge_lambda) %>% round(5),
                      `In-sample RMSE` = c(lasso_RMSE_insample, ridge_RMSE_insample) %>% round(5),
                      `Out-of-sample RMSE` = c(lasso_RMSE_outofsample, ridge_RMSE_outofsample) %>% round(5))
                      
results %>% knitr::kable(format = 'latex')
