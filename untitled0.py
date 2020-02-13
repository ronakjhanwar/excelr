import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
train=pd.read_csv("train.csv")
test=pd.read_csv("test.csv")
test.describe()
train.head()
sum(train.isnull().sum().values)
train1=train.iloc[:, 1:80]
total=train1.append(test.iloc[:,1:79],sort=False)
total.head()
total.isnull().sum()
total.info()
plt.figure(figsize=(5,5))
sns.distplot(train.SalePrice, color="tomato")
plt.title("Target distribution in train")
plt.ylabel("Density");
import os 
dcurrentDirectory = os.getcwd()
os.chdir(r"C:\Users\Ronakjhanwar\.spyder-py3")
plt.figure(figsize=(5,5))
sns.distplot(np.log(train.SalePrice), color="tomato")
plt.title("Log Target distribution in train")
plt.ylabel("Density");

nan_percentage = total.isnull().sum().sort_values(ascending=False) / total.shape[0]
missing_val = nan_percentage[nan_percentage > 0]
plt.figure(figsize=(15,5))
sns.barplot(x=missing_val.index.values, y=missing_val.values * 100, palette="Reds_r");
plt.title("Percentage of missing values in train & test");
plt.ylabel("%");
plt.xticks(rotation=90);

to_drop = missing_val[missing_val > 0.5].index.values
total = total.drop(to_drop, axis=1)
total.shape
to_drop
sum(total.isnull().sum().values)
total.info()

total['BsmtCond']=total['BsmtCond'].fillna(total['BsmtCond'].mode()[0])
total['BsmtExposure']=total['BsmtExposure'].fillna(total['BsmtExposure'].mode()[0])
total['BsmtFinType1']=total['BsmtFinType1'].fillna(total['BsmtFinType1'].mode()[0])
total['BsmtFinType2']=total['BsmtFinType2'].fillna(total['BsmtFinType2'].mode()[0])
total['BsmtQual']=total['BsmtQual'].fillna(total['BsmtQual'].mode()[0])
total['Electrical']=total['Electrical'].fillna(total['Electrical'].mode()[0])
total['Exterior1st']=total['Exterior1st'].fillna(total['Exterior1st'].mode()[0])
total['Exterior2nd']=total['Exterior2nd'].fillna(total['Exterior2nd'].mode()[0])
total['FireplaceQu']=total['FireplaceQu'].fillna(total['FireplaceQu'].mode()[0])
total['Functional']=total['Functional'].fillna(total['Functional'].mode()[0])
total['GarageCond']=total['GarageCond'].fillna(total['GarageCond'].mode()[0])
total['GarageFinish']=total['GarageFinish'].fillna(total['GarageFinish'].mode()[0])
total['GarageQual']=total['GarageQual'].fillna(total['GarageQual'].mode()[0])
total['GarageType']=total['GarageType'].fillna(total['GarageType'].mode()[0])
total['KitchenQual']=total['KitchenQual'].fillna(total['KitchenQual'].mode()[0])
total['MSZoning']=total['MSZoning'].fillna(total['MSZoning'].mode()[0])
total['MasVnrType']=total['MasVnrType'].fillna(total['MasVnrType'].mode()[0])
total['SaleCondition']=total['SaleCondition'].fillna(total['SaleCondition'].mode()[0])
total['SaleType']=total['SaleType'].fillna(total['SaleType'].mode()[0])
total['Utilities']=total['Utilities'].fillna(total['Utilities'].mode()[0])


plt.hist(total.GarageArea)
total.GarageArea.mean()
total.loc[:,"TotalBsmtSF"].value_counts()
total = total.drop(['GarageYrBlt'], axis=1)

total['BsmtFinSF1']=total['BsmtFinSF1'].fillna(total['BsmtFinSF1'].mean())
total['BsmtFinSF2']=total['BsmtFinSF2'].fillna(total['BsmtFinSF2'].median())
total['BsmtFullBath']=total['BsmtFullBath'].fillna(total['BsmtFullBath'].median())
total['BsmtHalfBath']=total['BsmtHalfBath'].fillna(total['BsmtHalfBath'].median())
total['BsmtUnfSF']=total['BsmtUnfSF'].fillna(total['BsmtUnfSF'].median())
total['GarageCars']=total['GarageCars'].fillna(total['GarageCars'].median())
total['LotFrontage']=total['LotFrontage'].fillna(total['LotFrontage'].mean())
total['MasVnrArea']=total['MasVnrArea'].fillna(total['MasVnrArea'].median())
total['TotalBsmtSF']=total['TotalBsmtSF'].fillna(total['TotalBsmtSF'].mean())
total['GarageArea']=total['GarageArea'].fillna(total['GarageArea'].mean())

num_candidates = list(total.dtypes[total.dtypes!="object"].index.values)
num_candidates
cat_candidates = total.dtypes[total.dtypes=="object"].index.values
cat_candidates
frequencies = []
for col in cat_candidates:
    overall_freq = total.loc[:, col].value_counts().max() / total.shape[0]
    frequencies.append([col, overall_freq])

frequencies = np.array(frequencies)
freq_df = pd.DataFrame(index=frequencies[:,0], data=frequencies[:,1], columns=["frequency"])
sorted_freq = freq_df.frequency.sort_values(ascending=False)

plt.figure(figsize=(15,5))
sns.barplot(x=sorted_freq.index[0:30], y=sorted_freq[0:30].astype(np.float), palette="Blues_r")
plt.xticks(rotation=90);
total['Utilities'].value_counts()

w=pd.get_dummies(total)
train2=w.iloc[0:1460,:]
test2=w.iloc[1460:,:]
train2['SalePrice']=train.SalePrice
train.get('SalePrice')


# Fitting Random Forest Regression to the dataset 
# import the regressor 
from sklearn.ensemble import RandomForestRegressor 
  
 # create regressor object 
regressor = RandomForestRegressor(n_estimators = 100, random_state = 0) 
x=train2.iloc[:,:274] 
y=train2.iloc[:,274:] 
# fit the regressor with x and y data 
regressor.fit(x, y)  

predictions = regressor.predict(test2)

pred=pd.DataFrame(predictions)
sub_df=pd.read_csv("sample.csv")
dataset=pd.concat([sub_df['Id'],pred],axis=1)
dataset.columns=['ID','SalePrice']
dataset.to_csv('sample_submission.csv',index=False)



