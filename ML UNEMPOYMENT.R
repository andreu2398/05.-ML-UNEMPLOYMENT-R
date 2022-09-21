# LIBRARIES AND DATA
{

#LIBRARIES

library(rio)
library(dplyr)
library(tidyr)
library(caret)
library(kernlab)
library(randomForest)

#DATA

df <- import("./unemployment.csv") %>%
    rename("year" = 1 , "month" = 2 , "unemployment" = 3) %>%
    mutate(unemployment = as.numeric(gsub("\\.", "", unemployment, perl=TRUE))) %>%
    head(259)

aa <- df %>% select(3) %>%
    head(nrow(df)-1) %>%
    add_row(unemployment = NA, .before = 0)

df <- df %>% mutate(unemployment_prev1m = aa$unemployment)

aa <- df %>% select(3) %>%
    head(nrow(df)-2) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0)

df <- df %>% mutate(unemployment_prev2m = aa$unemployment)

aa <- df %>% select(3) %>%
    head(nrow(df)-3) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0)

df <- df %>% mutate(unemployment_prev3m = aa$unemployment)

aa <- df %>% select(3) %>%
    head(nrow(df)-4) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0)

df <- df %>% mutate(unemployment_prev4m = aa$unemployment)

aa <- df %>% select(3) %>%
    head(nrow(df)-5) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0)

df <- df %>% mutate(unemployment_prev5m = aa$unemployment)

aa <- df %>% select(3) %>%
    head(nrow(df)-6) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0)

df <- df %>% mutate(unemployment_prev6m = aa$unemployment)

aa <- df %>% select(3) %>%
    head(nrow(df)-7) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0)

df <- df %>% mutate(unemployment_prev7m = aa$unemployment)

aa <- df %>% select(3) %>%
    head(nrow(df)-8) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0)

df <- df %>% mutate(unemployment_prev8m = aa$unemployment)

aa <- df %>% select(3) %>%
    head(nrow(df)-9) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0)

df <- df %>% mutate(unemployment_prev9m = aa$unemployment)

aa <- df %>% select(3) %>%
    head(nrow(df)-10) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0)

df <- df %>% mutate(unemployment_prev10m = aa$unemployment)

aa <- df %>% select(3) %>%
    head(nrow(df)-11) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0)

df <- df %>% mutate(unemployment_prev11m = aa$unemployment)

aa <- df %>% select(3) %>%
    head(nrow(df)-12) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0) %>%
    add_row(unemployment = NA, .before = 0)

df <- df %>% mutate(unemployment_prevy = aa$unemployment)


df <- df %>% mutate(var1m = unemployment - unemployment_prev1m ,
                    var2m = unemployment - unemployment_prev2m ,
                    var3m = unemployment - unemployment_prev3m ,
                    var4m = unemployment - unemployment_prev4m ,
                    var5m = unemployment - unemployment_prev5m ,
                    var6m = unemployment - unemployment_prev6m ,
                    var7m = unemployment - unemployment_prev7m ,
                    var8m = unemployment - unemployment_prev8m ,
                    var9m = unemployment - unemployment_prev9m ,
                    var10m = unemployment - unemployment_prev10m ,
                    var11m = unemployment - unemployment_prev11m ,
                    vary = unemployment - unemployment_prevy)

df <- df %>% mutate(var1mper = (unemployment - unemployment_prev1m)*100/unemployment ,
                    var2mper = (unemployment - unemployment_prev2m)*100/unemployment ,
                    var3mper = (unemployment - unemployment_prev3m)*100/unemployment ,
                    var4mper = (unemployment - unemployment_prev4m)*100/unemployment ,
                    var5mper = (unemployment - unemployment_prev5m)*100/unemployment ,
                    var6mper = (unemployment - unemployment_prev6m)*100/unemployment ,
                    var7mper = (unemployment - unemployment_prev7m)*100/unemployment ,
                    var8mper = (unemployment - unemployment_prev8m)*100/unemployment ,
                    var9mper = (unemployment - unemployment_prev9m)*100/unemployment ,
                    var10mper = (unemployment - unemployment_prev10m)*100/unemployment ,
                    var11mper = (unemployment - unemployment_prev11m)*100/unemployment ,
                    varyper = (unemployment - unemployment_prevy)*100/unemployment)

df <- df %>% tail(nrow(df)-12) %>%
    select(-year)

df_var <- df %>% select(month , var1m , var2m , var3m , var4m , var5m , var6m , var7m , var8m , var9m , var10m , var11m , vary , var1mper , var2mper , var3mper , var4mper , var5mper , var6mper , var7mper , var8mper , var9mper , var10mper , var11mper , varyper)

df_absolut <- df %>% select(month , unemployment , unemployment_prev1m , unemployment_prev2m , unemployment_prev3m , unemployment_prev4m , unemployment_prev5m , unemployment_prev6m , unemployment_prev7m , unemployment_prev8m , unemployment_prev9m , unemployment_prev10m , unemployment_prev11m , unemployment_prevy)

}

# CREATING DATASET TO TRAIN AND THEN TEST THE MODELS
{
#create a list of 80% of the rows in the original dataset we can use for training
validation_index <- createDataPartition(df_var$month , p = 0.8 , list = FALSE)

#select 20% of the data for validation
validation <- df_var[-validation_index,]

#use the remaining 80% of data to training and testing the models
dataset <- df_var[validation_index,]
}

# RUN SOME ALGORITHMS

{
# Run algorithms using 10-fold cross validation
control <- trainControl(method = "cv" , number = 10)
metric <- "Accuracy"

# Build Models
# a) linear algorithms
set.seed(7)
fit.lda <- train(month~. , data = dataset , method = "lda" , metric = metric , trControl = control)

# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(month~. , data = dataset , method = "rpart" , metric = metric , trControl = control)
# kNN
set.seed(7)
fit.knn <- train(month~. , data = dataset , method = "knn" , metric = metric , trControl = control)

# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(month~. , data = dataset , method = "svmRadial" , metric = metric , trControl = control)
# Random Forest
set.seed(7)
fit.rf <- train(month~. , data = dataset , method = "rf" , metric = metric , trControl = control)
}

# COMPARING THE MODELS
{
# Summarize accuracy of models
results <- resamples(list(lda = fit.lda , cart = fit.cart , knn=fit.knn , svn = fit.svm , rf = fit.rf))
summary(results)

# Compare accuracy of models
dotplot(results)

# Summarize the Best Model (in this case it is "lda")
print(fit.lda)
}

# MAKE PREDICTIONS

predictions <- predict(fit.lda , validation)

confusionMatrix(predictions , as.factor(validation$month))
