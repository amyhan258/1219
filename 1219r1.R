library(tidymodels)
print("hello")

compute_log_ratio <- function(mpg, wt, log_base = exp(1)) {
  log(mpg/wt, base = log_base)
}
map(head(mtcars$mpg, 3), sqrt)

mtcars

data(ames)
dim(ames)
ggplot(ames, aes(x = Sale_Price)) + 
  geom_histogram(bins = 50, col= "white") #geom_histogram(aes(y = ..count..))

ames <- ames %>% mutate(Sale_Price = log10(Sale_Price))
ames

set.seed(501)
ames_split <- initial_split(ames, prop = 0.80)
ames_split
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

dim(ames_train)
dim(ames_test)

set.seed(502)


set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

dim(ames_train)

# validation: This led to models that overfit, meaning that they performed 
# very well on the training set but poorly on the test set. 
# To combat this issue, a small validation set of data were held back and 
# used to measure performance as the network was trained. 


# set seed every time we want to get a reproducible random result
set.seed(502)
# To put 60% into training, 20% in validation, and 20% in testing:
ames_val_split <- initial_validation_split(ames, prop = c(0.6, 0.2))
ames_val_split
ames_train <- training(ames_val_split)
ames_test <- testing(ames_val_split)
ames_val <- validation(ames_val_split)

#머신러닝의 regularization: outlier의 영향을 적게 받게 하기 위해서

#For tidymodels, the approach to specifying a model is intended to be more unified
# 1. model type, engine, mode

# 2. Once the details of the model have been specified, the model estimation 
# can be done with either the fit() function (to use a formula) or 
# the fit_xy() function (when your data are already pre-processed). 
# The parsnip package allows the user to be indifferent to the interface 
# of the underlying model; you can always use a formula even if the modeling package’s 
# function only has the x/y interface.


lm_model <- 
  linear_reg() %>% 
  set_engine("lm")

#2-1
lm_form_fit <- 
  lm_model %>% 
  fit(Sale_Price ~ Longitude + Latitude, data = ames_train)
lm_form_fit

#2-2
lm_xy_fit <- 
  lm_model %>% 
  fit_xy(
    x = ames_train %>% select(Longitude, Latitude),
    y = ames_train %>% pull(Sale_Price)
  )
lm_xy_fit

# 3: parsnip의 장점
# Not only does parsnip enable a consistent model interface for different packages, 
# it also provides consistency in the model arguments. 
# Random forest's Three commonly used arguments are 
# the number of trees in the ensemble, 
# the number of predictors to randomly sample with each split within a tree, 
# and the number of data points required to make a split. 

# +3. For example, to specify the amount of regularization to use in a glmnet model, 
# the Greek letter lambda is used. parsnip standardizes on the argument name penalty
# :. Main arguments vs Engine arguments -> set_engine()에서 해라.

#4. 결과
lm_form_fit %>% extract_fit_engine()
# or
tidy(lm_form_fit)

#5. 예측
ames_test_small <- ames_test %>% slice(1:5)
ames_test_small %>% 
  select(Sale_Price) %>% 
  bind_cols(predict(lm_form_fit, ames_test_small)) %>% 
  # Add 95% prediction intervals to the results:
  bind_cols(predict(lm_form_fit, ames_test_small, type = "pred_int")) 


# new 1. workflow
lm_wflow <- 
  workflow() %>% 
  add_model(lm_model)

lm_wflow <- 
  lm_wflow %>% 
  add_formula(Sale_Price ~ Longitude + Latitude)
lm_wflow

lm_fit <- fit(lm_wflow, ames_train)
lm_fit

predict(lm_fit, ames_test %>% slice(1:3)) # remind: predict는 fit 한거에 쓰기

# Both the model and preprocessor can be removed or updated:
lm_fit %>% update_formula(Sale_Price ~ Longitude)

# 2. formula 말고 variable도 줄 수 있음
lm_wflow <- 
  lm_wflow %>% 
  remove_formula() %>% 
  add_variables(outcome = Sale_Price, predictors = c(Longitude, Latitude))
fit(lm_wflow, ames_train)

#3 - multilevel model에 대하여
library(nlme)
library(lme4)
library(multilevelmod)
Orthodont
#lmer(distance ~ Sex + (age | Subject), data = Orthodont)


multilevel_spec <- linear_reg() %>% set_engine("lmer")

multilevel_workflow <- 
  workflow() %>% 
  # Pass the data along as-is: 
  add_variables(outcome = distance, predictors = c(Sex, age, Subject)) %>% 
  add_model(multilevel_spec, formula = distance ~ Sex + (age | Subject))
            
multilevel_fit <- fit(multilevel_workflow, data = Orthodont)
multilevel_fit



# recipe 쓰기
library(tidymodels) # Includes the recipes package
tidymodels_prefer()

simple_ames <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type,
         data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_dummy(all_nominal_predictors())
simple_ames

lm_wflow <- 
  lm_wflow %>% 
  remove_variables() %>% 
  add_recipe(simple_ames)
lm_wflow
lm_fit <- fit(lm_wflow, ames_train)

lm_fit %>% 
  tidy() %>% 
  slice(1:5)


# recipe까지 들어간 일차최종
library(tidymodels)
data(ames)
ames <- mutate(ames, Sale_Price = log10(Sale_Price))

set.seed(502)
ames_split <- initial_split(ames, prop = 0.80, strata = Sale_Price)
ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

ames_rec <- 
  recipe(Sale_Price ~ Neighborhood + Gr_Liv_Area + Year_Built + Bldg_Type + 
           Latitude + Longitude, data = ames_train) %>%
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_other(Neighborhood, threshold = 0.01) %>% # step_other() pool infrequently occurring values into an "other" category.
  step_dummy(all_nominal_predictors()) %>% 
  step_interact( ~ Gr_Liv_Area:starts_with("Bldg_Type_") ) %>% 
  step_ns(Latitude, Longitude, deg_free = 20)

lm_model <- linear_reg() %>% set_engine("lm")

lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(ames_rec)

lm_fit <- fit(lm_wflow, ames_train)
tidy(lm_fit)


ames_test_res <- predict(lm_fit, new_data = ames_test %>% select(-Sale_Price))
ames_test_res
ames_test_res <- bind_cols(ames_test_res, ames_test %>% select(Sale_Price))
ames_test_res
ggplot(ames_test_res, aes(x = Sale_Price, y = .pred)) + 
  # y=x
  geom_abline(lty = 2) + 
  geom_point(alpha = 0.5) + 
  labs(y = "Predicted Sale Price (log10)", x = "Sale Price (log10)") +
  coord_obs_pred()

ames_metrics <- metric_set(rmse, rsq, mae)
ames_metrics(ames_test_res, truth = Sale_Price, estimate = .pred)

data(two_class_example)
#tibble(two_class_example)
two_class_curve <- roc_curve(two_class_example, truth, Class1)
#two_class_curve
autoplot(two_class_curve)
roc_auc(two_class_example, truth, Class1)

#먼가 9단원까지 끝남
