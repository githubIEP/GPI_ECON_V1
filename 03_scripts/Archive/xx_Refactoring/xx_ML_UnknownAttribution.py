##### ----- Machine Learning in Python for Terrorist Group Attribution



### --- Libraries

import pandas as pd 
import xgboost as xgb 
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.metrics import accuracy_score
from sklearn.preprocessing import LabelEncoder
from imblearn.over_sampling import RandomOverSampler
import matplotlib.pyplot as plt
import os

### --- Variables

# Categorical Variables
ML_CATEGORICAL = ["iep_geocode", "the_west", "event_type", "suicide", "targets_1", "weapons_1", "intensity", "conflict", "iep_region", "ID"]

# Hyperparameters
hyperparameters = {
    'n_estimators': [50, 100, 150],
    'max_depth': [3, 6, 9],
    'learning_rate': [0.01, 0.1, 0.3],
    'gamma': [0, 0.5, 1],
    'colsample_bytree': [0.5, 0.7, 1],
    'min_child_weight': [1, 3, 5]
}

### --- Functions

# Load and process datasets
def pf_LoadClean(filepath):
    """Load and preprocess the dataset."""
    # Load data
    df = pd.read_csv(filepath)

    # Remove groups with 5 or fewer instances
    group_counts = df['Group'].value_counts()
    df = df[df['Group'].isin(group_counts[group_counts > 5].index)]

    # Encode categorical variables
    df = pd.get_dummies(df, columns=ML_CATEGORICAL)

    # Label Encoding for the target variable
    label_encoder = LabelEncoder()
    df['Group'] = label_encoder.fit_transform(df['Group'])

    return df, label_encoder

# Train the model
def pf_TrainModel(df, target_variable, hyperparameters):
    X = df.drop(target_variable, axis=1)
    y = df[target_variable]

    # Split data into train and test sets
    X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=123)

    # Upsampling
    ros = RandomOverSampler(random_state=123)
    X_train_resampled, y_train_resampled = ros.fit_resample(X_train, y_train)

    # Initialize XGBoost classifier
    model = xgb.XGBClassifier()

    # Grid search for hyperparameter tuning
    clf = GridSearchCV(model, hyperparameters, scoring='accuracy', cv=5)
    clf.fit(X_train_resampled, y_train_resampled)

    return clf, X_test, y_test

# Fit Model to Test Set and Evaluation
def pf_EvaluateModel(clf, X_test, y_test, group_encoder):
    predictions = clf.predict(X_test)

    # Convert numeric predictions back to original labels
    original_labels = group_encoder.inverse_transform(predictions)

    accuracy = accuracy_score(y_test, predictions)
    print(f"Model Accuracy: {accuracy:.2f}")

    return accuracy, original_labels

# Plot of feature importance
def pf_Plot_FeatureImportance(clf):
    """Plot feature importance of the model."""
    xgb.plot_importance(clf.best_estimator_)
    plt.show()

# Load and preprocess data
df_evaluate, group_encoder = pf_LoadClean("02_data/processed/ml_testtraining.csv")

# Train the model
clf, X_test, y_test = pf_TrainModel(df_evaluate, 'Group', hyperparameters)

# Evaluate the model and get original labels of predictions
accuracy, original_labels = pf_EvaluateModel(clf, X_test, y_test, group_encoder)

# Print original labels of predictions
print("Original Labels of Predictions:", original_labels)

# Plot feature importance
pf_Plot_FeatureImportance(clf)


