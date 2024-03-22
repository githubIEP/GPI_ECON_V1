##### ----- Preparing Data for Machine Learning Model

### --- Libraries and Variables

library(tidyverse)
library(reticulate)

# Variables to remove
ML_KEEP = c("geocode","year","event_type","date","deaths_total","hostages_total","injured_total",
            "is_claimed","perpetrator_name","suicide","terrorists_killed","summary","latitute","longitude",
            "weapons","targets","the_west","conflict","intensity","ideology","admin_ID")

ML_REMOVE =c("summary","Unknown")

# Groups counted as unknown
ML_UNKNOWN = c("Unknown", "Jihadist (undetermined)")

# Date Variables
ML_DATE = c("date")

### --- Read in the Data

# Load datasert, add marker for Unknown groups
ml.df <- rio::import("02_data/processed/clean_TT.rds") %>%
  select(all_of(ML_KEEP)) %>%
  mutate(Unknown = if_else(Group %in% ML_UNKNOWN, 1, 0))

# Convert Date Variables to Numeric
for (date_var in ML_DATE) {
  ml.df[[date_var]] <- as.numeric(ml.df[[date_var]])
}

### --- Extracting Group Names using ChatGPT API

#source()

### --- Split Data into Test/Training and Final, export for Python ML

ml_unknown.df <- ml.df %>%
  dplyr::filter(Unknown == 1) %>%
  select(-all_of(ML_REMOVE))

ml_known.df <- ml.df %>%
  dplyr::filter(Unknown == 0) %>%
  select(-all_of(ML_REMOVE))

write.csv(ml_known.df,"02_data/processed/ml_testtraining.csv")
write.csv(ml_unknown.df,"02_data/processed/ml_final.csv")






## One-hot encoding categorical variables
for (cat_var in ML_CATEGORICAL) {
  ml_known.df[[cat_var]] <- as.integer(factor(ml_known.df[[cat_var]]))
}



ml_known.df <- na.omit(ml_known.df)
target_column <- "Group"
ml_known.df[[target_column]] <- ml_known.df[[target_column]] - 1
CLASS_COUNT = n_distinct(ml_known.df$Group) + 1

### --- Cross-Validation and Hyperparameter Tuning

set.seed(123)
folds <- createFolds(ml_known.df$Group, k = 5)
control <- trainControl(method = "cv", number = 5, index = folds, returnResamp = "all")

grid <- expand.grid(
  nrounds = c(50, 100, 150),
  max_depth = c(3, 6, 9),
  eta = c(0.01, 0.1, 0.3),
  gamma = c(0, 0.5, 1),
  colsample_bytree = c(0.5, 0.7, 1),
  min_child_weight = c(1, 3, 5),
  subsample = c(0.5, 0.7, 1)
)

xgb_train <- train(
  Group ~ ., 
  data = ml_known.df,
  method = "xgbTree",
  trControl = control,
  tuneGrid = grid
)

### --- Exporting and Visualizing Performance Metrics

# Export performance metrics
results <- xgb_train$results
performance_table <- xgb_train$resample
write.csv(results, "04_outputs/tables/model_performance_results.csv")
write.csv(performance_table, "04_outputs/tables/model_performance_table.csv")

# Performance Chart
performance_plot <- ggplot(performance_table, aes(x = Resample, y = Accuracy)) +
  geom_line() +
  ggtitle("Model Performance Across Folds") +
  xlab("Fold") +
  ylab("Accuracy")
print(performance_plot)
ggsave("04_outputs/charts/ml_performance_plot.png", performance_plot)

### --- Feature Importance

importance_matrix <- xgb.importance(feature_names = colnames(data_matrix), model = xgb_train$finalModel)
feature_importance_plot <- xgb.plot.importance(importance_matrix, top_n = 10)
print(feature_importance_plot)
ggsave("04_outputs/charts/ml_feature_importance_plot.png", feature_importance_plot)