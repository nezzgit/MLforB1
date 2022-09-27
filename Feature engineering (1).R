#########################################################
# Course: Machine Learning for BI 1 
# Lecture 2: Feature engineering application
# Copyright: Ana Alina Tudoran
# Aarhus University, Fall 2022
#########################################################

# Helper packages
library(dplyr)    # for data manipulation
library(ggplot2)  # for awesome graphics
library(visdat)   # for additional visualizations
# Feature engineering packages
library(caret)    # for various ML tasks
library(recipes)  # for feature engineering tasks


# 3.1. Data "ames" has property sales information
#   Dependant Variable: Sale_Price (i.e., $195,000, $215,000)
#   Objective: use property attributes to predict the sale price of a home - Create algo to predict house price.
#   Details: ?AmesHousing::ames_raw
#   Install.packages("AmesHousing", lib="/Library/Frameworks/R.framework/Versions/4.0/Resources/library")

# Get data
  ames <- AmesHousing::make_ames()

# Stratified sampling with the rsample package
  set.seed(123)
  library(rsample)
  split <- initial_split(ames, prop = 0.7, strata = "Sale_Price")
  ames_train  <- training(split)
  ames_test   <- testing(split)

# 3.2 Target engineering For heavily skewed data we will try many transformations, often we will not be able to make best changes
    attach(mtcars)
    par(mfrow=c(1,3))
    hist(ames_train$Sale_Price, breaks = 20, col = "red", border = "red", ylim = c(0, 800))
   
    # log if all value are positive. Applies log
    transformed_response <- log(ames_train$Sale_Price)
    
    # Box-Cox transformation (lambda=0 is equivalet to log(x)). Lambda parametres goes from -5 ot 5. Will test which value of Lambda transforms the data 
    # and leads to the closest possible bell curve distribution. 
    # If lambda is zeor it will apply log
    library(forecast)
    transformed_response_BC <- forecast::BoxCox(ames_train$Sale_Price, lambda="auto") # lambda auto
    
    hist(transformed_response, breaks = 20, col = "lightgreen", border = "lightgreen",ylim = c(0, 800) )
    hist(transformed_response_BC, breaks = 20, col = "lightblue", border = "lightblue", ylim = c(0, 800))
    
    
    #From left to right plots: Non transformed, log transform, boxcox transform
    
    # log transformation using a blueprint (recipe)
    # As an example, we create ames_recipe using caret as: 
    # software applies log to all dependant variables
      ames_recipe <- recipe(Sale_Price ~ ., data = ames_train) %>%
        step_log(all_outcomes())
      ames_recipe # This will not return the actual log transformed values but, rather, a blueprint to be applied later.
    

# 3.3 Dealing with missing data
    # If we use the original ames_raw data (via AmesHousing::ames_raw), 
    # there are actually 13,997 missing values 
    sum(is.na(AmesHousing::ames_raw))
    
    # simple plot
    library(DataExplorer)
    plot_missing(AmesHousing::ames_raw)
   
    # advanced plot
    library(ggplot2) 
    AmesHousing::ames_raw %>%
      is.na() %>%
      reshape2::melt() %>%
      ggplot(aes(Var2, Var1, fill=value)) + 
      geom_raster() + 
      coord_flip() +
      scale_y_continuous(NULL, expand = c(0, 0)) +
      scale_fill_grey(name = "", 
                      labels = c("Present", 
                                 "Missing")) +
      xlab("Observation") +
      theme(axis.text.y  = element_text(size = 4))
    
    # advanced plot
    library(visdat)
    vis_miss(AmesHousing::ames_raw, cluster = TRUE)
    #The variables and NA patterns have been clustered by rows (i.e., cluster = TRUE).
    
    # Options to manage mising data 
        # 1. Create a new dataset without missing values using na.omit() function 
        #    ames_without_missing <- na.omit (AmesHousing::ames_raw)
        # Note: If missing data has a particular label code in the original file (such as, unknown, 999, etc.), 
        # first we can recode that label class to missing value using:
        # data[data =="unknown"] <- NA 
        # data[data == 999] <- NA 
    
        # 2. Imputation
        # Imputing the mode (one variable)
        library (Hmisc) 
        ames_impute <- AmesHousing::ames_raw # make a copy 
        sum(is.na(ames_impute$Alley)) # 2732 missing
        ames_impute$Alley <- as.factor(ames_impute$Alley) # required to be factor
        ames_impute$Alley <- impute (ames_impute$Alley) # by default, the imputed value is the mode
        sum(is.na(ames_impute$Alley)) # 0 mising

        # Imputation using our blueprint is better 
        # The following would build onto our ames_recipe and 
        # impute all missing values for the Gr_Liv_Area variable
        # or for all variables: 
              # with the median value:
              ames_recipe %>%
                step_medianimpute(Gr_Liv_Area) # for mode imputation use step_modeimpute()
              # with KNN imputation for all predictors
              ames_recipe %>%
                step_knnimpute(all_predictors(), neighbors = 6)
              # with bagged decision trees imputation
              ames_recipe %>%
                step_bagimpute(all_predictors())
      
          
# 3.4 Feature filtering 
     # RULES OF THUMB TO DETECT zroVar and nzv
     # 1. The fraction of unique values over the sample size is low (say ≤10 %).
     # 2. The ratio of the frequency of the most prevalent value to the frequency 
     #  of the second most prevalent value is large (say ≥20)
    library(caret)
    caret::nearZeroVar(ames_train, saveMetrics = TRUE) %>% 
      tibble::rownames_to_column() %>% 
      filter(nzv)
    # NOTE: we can add step_zv() and step_nzv() to our ames_recipe to remove zero or near-zero variance features 
      ames_recipe %>%
        step_nzv(all_nominal()) %>%
    # Other feature filtering methods include using advanced modelling techniques like SVM, random forest etc. see ML2.
    
    
# 3.5 Numeric feature engineering 
    # Normalize all numeric columns using the blueprint
      ames_recipe %>%
        step_YeoJohnson(all_numeric())  
    # Standardize all numeric values using the blueprint
      ames_recipe %>%
        step_center(all_numeric(), -all_outcomes()) %>%
        step_scale(all_numeric(), -all_outcomes())
    
    
  # 3.6. Categorical feature engineering  
    # 1. Lumping (collapsing)
    # for some factors, some levels that have very few observations e.g. 
    count(ames_train, Neighborhood) %>% arrange(n)
    count(ames_train, Screen_Porch) %>% arrange(n)
    
    # to collapse all levels that are observed in less than 10% of the
    # training sample into an “other” category, use step_other()
    # Lump levels for two features example
    lumping <- ames_recipe %>%
      step_other(Neighborhood, threshold = 0.01, 
                 other = "other") %>%
      step_other(Screen_Porch, threshold = 0.1, 
                 other = ">0")
    
    # To see the effect, we apply the blueprint to training data (bake)
    apply_2_training <- prep(lumping, training = ames_train) %>%
    bake(ames_train)
    # see new distribution of Neighborhood
    count(apply_2_training, Neighborhood) %>% arrange(n)
    # New distribution of Screen_Porch
    count(apply_2_training, Screen_Porch) %>% arrange(n)
  
    
    # 2. Dummy encoding (step_dummy)
    # Lump levels for two features
    ames_recipe %>%
      step_dummy(all_nominal(), one_hot = FALSE)
    
    
    # 3. Label encoding (step_integer)
    # We should be careful with label encoding unordered categorical features 
    # because most models will treat them as ordered numeric features. 
    # If a categorical feature is naturally ordered then label encoding is a natural
    # choice
    count(ames_train, Overall_Qual)
    # Label encoded
    ames_recipe %>%
      step_integer(Overall_Qual) %>%
      prep(ames_train) %>%
      bake(ames_train) %>%
      count(Overall_Qual)
    
    
  # 3.7. Dimension reduction (step_pca)- this topic is covered in depth in a future lecture
    # In a few words, we want to reduce the dimension of our features 
    # with principal components analysis, retain the number of components 
    # required to explain, say, 95% of the variance and use these components 
    # as features in downstream modeling
    recipe(Sale_Price ~ ., data = ames_train) %>%
      step_center(all_numeric()) %>%
      step_scale(all_numeric()) %>%
      step_pca(all_numeric(), threshold = .90)
    
    
  # 3.8. Now let´s see a full-implementation of several tasks at once
    # The tasks are: 
      # Remove near-zero variance features that are categorical (aka nominal).
      # Ordinal encode our quality-based features (which are inherently ordinal).
      # Center and scale (i.e., standardize) all numeric features.
      # Perform dimension reduction by applying PCA to all numeric features.
      
          #  create blueprint
            ames_recipe <- recipe(Sale_Price ~ ., data = ames_train) %>%
              step_nzv(all_nominal())  %>%
              step_integer(matches("Qual|Cond|QC|Qu")) %>%
              step_center(all_numeric(), -all_outcomes()) %>%
              step_scale(all_numeric(), -all_outcomes()) %>%
              step_pca(all_numeric(), -all_outcomes())
            ames_recipe
          # prepare: estimate feature engineering parameters based on training data
            prepare <- prep(ames_recipe, training = ames_train)
            prepare
          # bake: apply the recipe to new data (e.g., the training data or future test data) with bake()
            baked_train <- bake(prepare, new_data = ames_train)
            baked_test <- bake(prepare, new_data = ames_test)
            baked_train
            baked_test
    
    
    # If we want to develop our blueprint and then, within each resample iteration, 
    # apply prep() and bake() to our resample training and validation data, then caret package 
    # simplifies this process. We only need to specify the blueprint 
    # and caret will automatically prepare and bake within each resample.
        set.seed(123)
        # 1. Create the blueprint
          ames_recipe <- recipe(Sale_Price ~ ., data = ames_train) %>%
          step_nzv(all_nominal()) %>%
          step_integer(matches("Qual|Cond|QC|Qu")) %>%
          step_center(all_numeric(), -all_outcomes()) %>%
          step_scale(all_numeric(), -all_outcomes()) %>%
          step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE)
        
        # Next, apply the same resampling method and hyperparameter search grid as we did
        # in previous lecture.The only difference is when we train our re-sample models 
        # with train(), we supply our blueprint (ames_recipe) as the first argument 
        # and then caret library takes care of the rest.
        
        # Specify re-sampling plan
        cv <- trainControl(
          method = "repeatedcv", 
          number = 10, 
          repeats = 2)
        
        # Construct grid of hyperparameter values
        hyper_grid <- expand.grid(k = seq(1, 10, by = 1))
        
        # Tune a knn model using grid search
        knn_fit2 <- train(
          ames_recipe,      # notice here!
          data = ames_train, 
          method = "knn", 
          trControl = cv, 
          tuneGrid = hyper_grid,
          metric = "RMSE"
        )
        
        knn_fit2
        # Looking at our results we see that, for this run, the best KNN-model 
        # was associated with k=7 with a cross-validated RMSEA of 33318.22
        # Notice in the KNN.R file (without feature engineering),
        # we obtain the best model with k=7, and cross-validated RMSE of 43607.26.
        # Hence feature engineering reduced significantly the error. 
        
        # visual display of RMSEA vs. k
        ggplot(knn_fit2)
           
 # Apply recipe to new data (test)
        predictions <- predict(knn_fit2, ames_test) #Since you are giving the recipe (inside of model), 
        # we do not give predict() baked data; we use predict() and the original test set.
        predictions 
        # we can calculate the test RMSEA 
        RMSEA = sqrt(sum((ames_test$Sale_Price - predictions)^2/877))
        RMSEA
        #or simply
        library(Metrics)
        rmse(ames_test$Sale_Price, predictions)
     
        
        