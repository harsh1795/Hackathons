import pandas
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.feature_selection import SelectFromModel
from sklearn.feature_extraction import DictVectorizer 
URL = 'p.csv'
df_training = pandas.read_csv(URL)
Y_train = df_training['Lung_Cancer'].values
del df_training['Lung_Cancer']
del df_training['Patient_ID']
df_train = df_training[:19]
df_test = df_training[19:]
#ENCODING
X_train = df_train.to_dict('records')
X_test = df_test.to_dict('records')
X_tr = []
X_te = []
X_tr.extend(X_train)
X_te.extend(X_test)
X_total = X_tr + X_te
#One Hot Encoding 
enc = DictVectorizer(sparse = True)
X_encoded_total =enc.fit_transform(X_total)
X_encoded_train =X_encoded_total[:len(X_tr)]
X_encoded_test =X_encoded_total[len(X_tr):]
estimator = GradientBoostingClassifier() #Using a basic GBM for feature reduction
estimator.fit(X_encoded_train.toarray(),Y_train[:19])
Predictions = estimator.predict(X_encoded_test.toarray())
print Predictions