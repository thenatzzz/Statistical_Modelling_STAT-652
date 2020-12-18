import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from sklearn import datasets, linear_model
from sklearn.metrics import mean_squared_error, r2_score
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import accuracy_score
import random
from sklearn import svm, ensemble
from sklearn.neural_network import MLPRegressor
from sklearn.inspection import permutation_importance
import xgboost as xgb
from sklearn.model_selection import cross_validate
from sklearn.model_selection import cross_val_score

random.seed(2928893)


df = pd.read_csv('Data2020.csv')
print(df.shape, ' original')
df_X = df.iloc[:,1:]
df_Y= df.iloc[:,0]

print(df_X.shape, df_Y.shape)

X_train, X_test, y_train, y_test = train_test_split(df_X, df_Y, test_size=0.25)
print(X_train.shape, X_test.shape, y_train.shape, y_test.shape)

# fit the model
# model = RandomForestRegressor(random_state=1)
# model = linear_model.LinearRegression()
# model.fit(X_train, y_train)
# y_pred = model.predict(X_test)

# print('Mean squared error: %.2f'
      # % mean_squared_error(y_test, y_pred))
#
params_gbt = {'n_estimators': 500,
          'max_depth': 4,
          'min_samples_split': 5,
          'learning_rate': 0.01,
          'loss': 'ls'}
classifiers = [
    xgb.XGBRegressor(objective ='reg:squarederror', colsample_bytree = 0.3, learning_rate = 0.1,
                max_depth = 5, alpha = 10, n_estimators = 10),
    ensemble.GradientBoostingRegressor(**params_gbt),
    RandomForestRegressor(random_state=2928893),
    svm.SVR(),
    linear_model.BayesianRidge(),
    linear_model.LassoLars(),
    linear_model.ARDRegression(),
    linear_model.PassiveAggressiveRegressor(),
    linear_model.TheilSenRegressor(),
    linear_model.LinearRegression(),
    MLPRegressor(solver='lbfgs', alpha=1e-5,
                    hidden_layer_sizes=(5, 2), random_state=1)]
for item in classifiers:
    print(item)
    model = item
    model.fit(X_train, y_train)
    y_pred = model.predict(X_test)

    # print('Mean squared error: %.2f'
          # % mean_squared_error(y_test, y_pred))
    rmse = np.sqrt(mean_squared_error(y_test, y_pred))
    print("RMSE : % f" %(rmse))

    # scores = cross_val_score(model,df_X, df_Y, cv=5)
    # print("Accuracy: %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))

    print('-'*50,'\n')

''' XGBoost '''
data_dmatrix = xgb.DMatrix(data=df_X,label=df_Y)

params = {"objective":"reg:squarederror",'colsample_bytree': 0.3,'learning_rate': 0.1,
                'max_depth': 5, 'alpha': 10}

cv_results = xgb.cv(dtrain=data_dmatrix, params=params, nfold=3,
                    num_boost_round=50,early_stopping_rounds=10,metrics="rmse", as_pandas=True, seed=123)
print(cv_results.head())
print(cv_results.tail(5))
# print((cv_results["test-rmse-mean"]).tail(1)*(cv_results["test-rmse-mean"]).tail(1))






# from sklearn.feature_selection import VarianceThreshold
# sel = VarianceThreshold(threshold=(.8 * (1 - .8)))
# x=sel.fit_transform(X_train)
# print(x.shape)


from sklearn.model_selection import StratifiedKFold
from sklearn.feature_selection import RFE

rfecv = RFE(estimator=RandomForestRegressor(random_state=2928893), n_features_to_select=1, step=1)
rfecv.fit(X_train, y_train)

print(" features : {}".format(rfecv.ranking_))




# for y,y_p in zip(y_test,y_pred):
    # print(y,y_p)
# import statsmodels.api as sm
#
# mod = sm.OLS(y_train, X_train)
# res = mod.fit()
# print(res.summary())
