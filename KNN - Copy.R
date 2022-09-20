#########################################################
# Course: Machine Learning for BI 1 
# Lecture 1: Fundamentals, modelling, KNN alg.
# Copyright: Ana Alina Tudoran
# Aarhus University, Fall 2022
#########################################################

# KNN regression - application

    # Modeling process packages required here
    library(rsample)   # for resampling procedures
    library(caret)     # for resampling and model training # a meta-engine

    # Ames housing data
    ames <- AmesHousing::make_ames() # data is available here
    dim(ames)
    str(ames$Sale_Price)

    # Stratified sampling 
    set.seed(123)
    split <- initial_split(ames, prop = 0.7, 
                           strata = "Sale_Price")
    ames_train  <- training(split)
    ames_test   <- testing(split)

    # Specify re-sampling strategy: k-fold cross-validation
    cv <- trainControl(
      method = "repeatedcv", 
      number = 10, 
      repeats = 1
    )
    
    # Create a grid of values for the hyperparameter k
    hyper_grid <- expand.grid(k = seq(2, 25, by = 1))

    # Tune a knn model using grid search
    knn_fit <- train(
      Sale_Price ~ ., 
      data = ames_train, 
      method = "knn", 
      trControl = cv, 
      tuneGrid = hyper_grid,
      metric = "RMSE"
    )
    knn_fit

# Looking at our results we see that the best model for this run (seed 123)
# was associated with k= 7, which resulted in a cross-validated RMSE of 43607.26
# Here RMSE was used to select the optimal model using the smallest value. 
# Exercise: use other metric for model selection, display the
# the result and discuss the implications.  


