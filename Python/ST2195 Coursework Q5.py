# -*- coding: utf-8 -*-
"""
Created on Sun Mar 13 17:47:54 2022

@author: Joshua
"""

### **Use the available variables to construct a model that predicts delays**

# Import modules

import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.metrics import plot_roc_curve
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.linear_model import LogisticRegression
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.ensemble import RandomForestClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn.pipeline import Pipeline
from sklearn.impute import SimpleImputer
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.compose import ColumnTransformer

pd.set_option("display.max_columns", None)

sns.set(rc={"figure.dpi":300, 'savefig.dpi':300})
sns.set_context('notebook')
sns.set_style("ticks")

os.chdir("C:/Users/Joseph/Downloads/Joshua/SIM Year 2/Programing for Data Science/r/coursework/data")

# Load in the data

flight_data = pd.read_csv("flight_data.csv") 

# Majority of the data preparation has been done during the earlier stages of the analysis

#### Overview of Delays       
                                                  
flight_data.groupby("depdel15")["arrdel15"].value_counts()

#### **Arrival Delay for Unique Carrier**

carrier_del = flight_data.groupby("UniqueCarrier")["arrdel15"]\
    .agg(["mean", "count"])\
    .sort_values("mean", ascending = False)\
        .reset_index()

plt.figure(figsize=(10, 4))
c = sns.barplot(data = carrier_del, 
                x = "UniqueCarrier",
                y = "mean",
                dodge = False,
                hue = "count",
                palette = "YlOrRd_r")
c.set(title = "Percentage of Arrival Delay by Carrier",
      xlabel = "Carrier",
      ylabel = "Percentage of Arrival Delay")
plt.legend(bbox_to_anchor = (1.02, 1), 
           loc = 'upper left',
           borderaxespad = 0,
           title = "Counts");

#### **Correlation Analysis**

# Before proceeding to the modeling stage:

# Check for correlation between the numerical variables of the model data

corr_data = flight_data.drop(
    columns = ["UniqueCarrier", "TailNum", "Origin", "Dest", "flight_date", "season"])

corr_matrix = corr_data.corr()

corr_matrix

# Create mask for lower triangle
mask = np.triu(np.ones_like(corr_matrix))

f, ax = plt.subplots(figsize = (10, 10))
cmap = sns.diverging_palette(220, 10, as_cmap = True)
sns.heatmap(corr_matrix, mask = mask, cmap = cmap, vmax = 0.3, center = 0,
            square = True, linewidths = 0.5, cbar_kws = {"shrink": 0.4},
            annot = True, annot_kws={"size":8}, fmt = ".2f");

#### **Data Modeling**

# "features": The list of variables that might be meaningful factors in determining delays

target = flight_data.loc[flight_data.Year == 2000, "arrdel15"].copy()
features = ["season", "UniqueCarrier", "DepTime", "depdel15",
            "ActualElapsedTime", "Distance", "TaxiOut"]  

model_data = flight_data.loc[flight_data.Year == 2000, features].copy()

model_data.info()

# Pipeliens: Pre-processing

numerical_features = ["DepTime", "ActualElapsedTime", "Distance", "TaxiOut"]

# Applying SimpleImputer and StandardScaler into a pipeline
numerical_transformer = Pipeline(steps = [
    ('imputer', SimpleImputer()),
    ('scaler', StandardScaler())])

categorical_features = ["season", "UniqueCarrier", "depdel15"]

# Applying SimpleImputer and StandardScaler into another pipeline
categorical_transformer = Pipeline(steps = [
    ('imputer', SimpleImputer()),
    ('onehot', OneHotEncoder(handle_unknown = 'ignore'))])

data_transformer = ColumnTransformer(
    transformers = [
        ('numerical', numerical_transformer, numerical_features),
        ('categorical', categorical_transformer, categorical_features)]) 

# Get the training and test sets

# Set random state for reproducible results

X_train, X_test, y_train, y_test = train_test_split(model_data, target, 
                                                    test_size = 0.5, 
                                                    random_state = 123)

# Set up parameters

param_grid = {
    'data_transformer__numerical__imputer__strategy': ['mean', 'median'],
    'data_transformer__categorical__imputer__strategy': ['constant','most_frequent']
}

#### **Logistic Regression**
pipe_lr = Pipeline(steps=[('data_transformer', data_transformer),
                      ('pipe_lr', LogisticRegression(max_iter = 10000, penalty = 'none'))])
grid_lr = GridSearchCV(pipe_lr, param_grid = param_grid)
grid_lr.fit(X_train, y_train);

#### **Gradient Boosting**

pipe_gdb = Pipeline(steps=[('data_transformer', data_transformer),
       ('pipe_gdb',GradientBoostingClassifier(random_state = 2))])

grid_gdb = GridSearchCV(pipe_gdb, param_grid = param_grid)
grid_gdb.fit(X_train, y_train);

#### **Random Forest**

pipe_rf = Pipeline(steps=[('data_transformer', data_transformer),
                           ('pipe_rf', RandomForestClassifier(random_state = 0))])
grid_rf = GridSearchCV(pipe_rf, param_grid = param_grid)
grid_rf.fit(X_train, y_train);

#### **Classification Tree**

pipe_tree = Pipeline(steps=[('data_transformer', data_transformer),
                           ('pipe_tree', DecisionTreeClassifier(random_state = 0))])
grid_tree = GridSearchCV(pipe_tree, param_grid = param_grid)
grid_tree.fit(X_train, y_train);

### **Compare the performance of the classification models by the ROC curve**

plt.figure(figsize=(8, 4))
ax = plt.gca()

plot_roc_curve(grid_lr, X_test, y_test, ax = ax, name = 'Logistic Regression')
plot_roc_curve(grid_gdb, X_test, y_test, ax = ax, name = 'Gradient Boosting')
plot_roc_curve(grid_rf, X_test, y_test, ax = ax, name = 'Random forests')
plot_roc_curve(grid_tree, X_test, y_test, ax = ax, name = 'Classification trees')
plt.plot([0, 1], [0, 1], color = 'black', lw = 1, linestyle = '--')
plt.show()

