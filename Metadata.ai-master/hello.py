from flask import Flask
from flask_cors import CORS, cross_origin
import MySQLdb
import pandas
from pandas.io.sql import frame_query
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.feature_selection import SelectFromModel
from sklearn.feature_extraction import DictVectorizer 
from sklearn.externals import joblib

app = Flask(__name__)
CORS(app)

@app.route("/")
def BasicNight():
		#BasicFuntion Night 
        db = MySQLdb.connect("localhost","root","","ge_hackathon" )
        cursor = db.cursor()
        sql = "SELECT * FROM answers WHERE finished='1'"
        data = frame_query(sql, db)
        #BasicFuntion Night 

        df_training = data
        df_train = df_training[df_training.columns[2:9]]

        Y_train = df_train['L11P'].values
        del df_train['L11P']
        #ENCODING
        X_train = df_train.to_dict('records')
        X_tr = []
        X_tr.extend(X_train)
        #One Hot Enpredcoding 
        enc = DictVectorizer(sparse = True)
        X_encoded_train =enc.fit_transform(X_tr)
        estimator = GradientBoostingClassifier() 
        estimator.fit(X_encoded_train.toarray(),Y_train)
        joblib.dump(estimator, 'Estbasic.pkl') 
        return 'success'

@app.route("/L1")
def Basicpred():
    URL = 'DUMB.csv'
    DUMB = pandas.read_csv(URL)
    df_DUMB = DUMB[DUMB.columns[1:7]]

    db = MySQLdb.connect("localhost","root","","ge_hackathon" )
    cursor = db.cursor()
    sql = "SELECT * FROM answers WHERE finished='0'"
    data = frame_query(sql, db)

    df_test = data[data.columns[2:8]]
    frames = [ df_DUMB,df_test]
    df_WH = pandas.concat(frames)

    #ENCODING
    X_test = df_WH.to_dict('records')
    X_te = []
    X_te.extend(X_test)


    encoder = DictVectorizer(sparse = True)
    X_encoded_test = encoder.fit_transform(X_te)
    clf2 = joblib.load('Estbasic.pkl') 
    Predictions = clf2.predict(X_encoded_test.toarray()[-1])
    return str(int(Predictions))

if __name__ == "__main__":
    app.run()