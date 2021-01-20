#%%
import numpy as np
import pandas as pd
from scipy.stats import uniform, randint
from sklearn.model_selection import KFold, RandomizedSearchCV, train_test_split
from sklearn.metrics import r2_score, mean_absolute_error
import xgboost as xgb

df = pd.read_csv('scores.csv')
col_y = 'favorite_count'
suffixes = ['a', 'h']
prefixes = ['xg', 'g', 'estimated_follower_count']
cols_x = []
for suffix in suffixes:
    for prefix in prefixes:
        cols_x.append(prefix + '_' + suffix)
cols_x.append('estimated_follower_count')

suffixes = ['x', 'y']
prefixes = ['hour', 'wday']
for suffix in suffixes:
    for prefix in prefixes:
        cols_x.append(prefix + '_' + suffix)

cols_x
#%%
df.columns
#%%
[print(col, col in df.columns) for col in cols_x]
# cols_x

#%%
df.columns.str


#%%
def select_y(df):
    return df.loc[:, col_y].values


def select_x(df):
    # return (df.loc[:, cols_x]).reset_index(drop=True)
    return (df.loc[:, ['hour_x', 'hour_y']])


x, y = select_x(df), select_y(df)

x_trn, x_tst, y_trn, y_tst = train_test_split(x, y, random_state=42)

#%%
xgb_model = xgb.XGBRegressor()
#%%
len(uniform(0.7, 0.3))

#%%
search = RandomizedSearchCV(
    xgb_model,
    param_distributions=params,
    random_state=42,
    n_iter=200,
    cv=3,
    verbose=1,
    n_jobs=1,
    return_train_score=True
)

search.fit(X, y)

#%%
rf_fit = RandomForestRegressor(random_state=42)