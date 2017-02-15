from flask import Flask
from flask_cors import CORS, cross_origin
import MySQLdb
import pandas
from pandas.io.sql import frame_query
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.feature_selection import SelectFromModel
from sklearn.feature_extraction import DictVectorizer 
from sklearn.externals import joblib
import numpy as np

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
        df_train = df_train[pandas.notnull(df_train['L11P'])]
        Y_train = df_train['L11P'].values
        del df_train['L11P']
        #ENCODING
        X_train = df_train.to_dict('records')
        X_tr = []
        X_tr.extend(X_train)
        #One Hot Encoding 
        enc = DictVectorizer(sparse = True)
        X_encoded_train =enc.fit_transform(X_tr)
        estimator = GradientBoostingClassifier() 
        estimator.fit(X_encoded_train.toarray(),Y_train)
        joblib.dump(estimator, 'Estbasic.pkl') 

        db = MySQLdb.connect("localhost","root","","ge_hackathon" )
        cursor = db.cursor()
        sql = "SELECT * FROM answers WHERE finished='1'"
        data = frame_query(sql, db)
        df_training = data
        df_train = df_training[list(data.columns[2:8])+ list(data.columns[10:16])]
        df_train = df_train[pandas.notnull(df_train['L21P'])]
        Y_train = df_train['L21P'].values
        del df_train['L21P']
        #ENCODING
        X_train = df_train.to_dict('records')
        X_tr = []
        X_tr.extend(X_train)
        #One Hot Encoding 
        enc = DictVectorizer(sparse = True)
        X_encoded_train =enc.fit_transform(X_tr)
        estimator = GradientBoostingClassifier() 
        estimator.fit(X_encoded_train.toarray(),Y_train)
        joblib.dump(estimator, 'EstL21.pkl')

        db = MySQLdb.connect("localhost","root","","ge_hackathon" )
        cursor = db.cursor()
        sql = "SELECT * FROM answers WHERE finished='1'"
        data = frame_query(sql, db)
        df_training = data
        df_train = df_training[list(data.columns[2:8])+ list(data.columns[17:22])]
        df_train = df_train[pandas.notnull(df_train['L22P'])]
        Y_train = df_train['L22P'].values
        del df_train['L22P']
        #ENCODING
        X_train = df_train.to_dict('records')
        X_tr = []
        X_tr.extend(X_train)
        #One Hot Encoding 
        enc = DictVectorizer(sparse = True)
        X_encoded_train =enc.fit_transform(X_tr)
        estimator = GradientBoostingClassifier() 
        estimator.fit(X_encoded_train.toarray(),Y_train)
        joblib.dump(estimator, 'EstL22.pkl') 
        
        db = MySQLdb.connect("localhost","root","","ge_hackathon" )
        cursor = db.cursor()
        sql = "SELECT * FROM answers WHERE finished='1'"
        data = frame_query(sql, db)
        df_training = data
        df_train = df_training[list(data.columns[2:8])+ list(data.columns[23:28])]
        df_train = df_train[pandas.notnull(df_train['L23P'])]
        Y_train = df_train['L23P'].values
        del df_train['L23P']
        #ENCODING
        X_train = df_train.to_dict('records')
        X_tr = []
        X_tr.extend(X_train)
        #One Hot Encoding 
        enc = DictVectorizer(sparse = True)
        X_encoded_train =enc.fit_transform(X_tr)
        estimator = GradientBoostingClassifier() 
        estimator.fit(X_encoded_train.toarray(),Y_train)
        joblib.dump(estimator, 'EstL23.pkl') 
        
        db = MySQLdb.connect("localhost","root","","ge_hackathon" )
        cursor = db.cursor()
        sql = "SELECT * FROM answers WHERE finished='1'"
        data = frame_query(sql, db)
        df_training = data
        df_train = df_training[list(data.columns[2:8])+ list(data.columns[29:34])]
        df_train = df_train[pandas.notnull(df_train['L24P'])]
        Y_train = df_train['L24P'].values
        del df_train['L24P']
        #ENCODING
        X_train = df_train.to_dict('records')
        X_tr = []
        X_tr.extend(X_train)
        #One Hot Encoding 
        enc = DictVectorizer(sparse = True)
        X_encoded_train =enc.fit_transform(X_tr)
        estimator = GradientBoostingClassifier() 
        estimator.fit(X_encoded_train.toarray(),Y_train)
        joblib.dump(estimator, 'EstL24.pkl') 
        
        URL = 'DUMB.csv'
        DUMB = pandas.read_csv(URL)
        db = MySQLdb.connect("localhost","root","","ge_hackathon" )
        cursor = db.cursor()
        sql = "SELECT * FROM answers WHERE finished='1'"
        data = frame_query(sql, db)
        Questions = list(data.columns[2:8])
        df_training = data
        df_train = df_training[Questions + list(data.columns[35:39])]
        df_train = df_train[pandas.notnull(df_train['L31P'])]
        Y_train = df_train['L31P'].values
        del df_train['L31P']
        #ENCODING
        X_train = df_train.to_dict('records')
        X_tr = []
        X_tr.extend(X_train)
        #One Hot Encoding 
        enc = DictVectorizer(sparse = True)
        X_encoded_train =enc.fit_transform(X_tr)
        estimator = GradientBoostingClassifier() 
        estimator.fit(X_encoded_train.toarray(),Y_train)
        joblib.dump(estimator, 'EstL31.pkl') 

        URL = 'DUMB.csv'
        DUMB = pandas.read_csv(URL)        
        db = MySQLdb.connect("localhost","root","","ge_hackathon" )
        cursor = db.cursor()
        sql = "SELECT * FROM answers WHERE finished='1'"
        data = frame_query(sql, db)
        Questions = list(data.columns[2:8])
        df_training = data
        df_train = df_training[Questions + list(data.columns[40:44])]
        df_train = df_train[pandas.notnull(df_train['L32P'])]
        Y_train = df_train['L32P'].values
        del df_train['L32P']
        #ENCODING
        X_train = df_train.to_dict('records')
        X_tr = []
        X_tr.extend(X_train)
        #One Hot Encoding 
        enc = DictVectorizer(sparse = True)
        X_encoded_train =enc.fit_transform(X_tr)
        estimator = GradientBoostingClassifier() 
        estimator.fit(X_encoded_train.toarray(),Y_train)
        joblib.dump(estimator, 'EstL32.pkl') 
        
        URL = 'DUMB.csv'
        DUMB = pandas.read_csv(URL)        
        db = MySQLdb.connect("localhost","root","","ge_hackathon" )
        cursor = db.cursor()
        sql = "SELECT * FROM answers WHERE finished='1'"
        data = frame_query(sql, db)
        Questions = list(data.columns[2:8])
        df_training = data
        df_train = df_training[Questions + list(data.columns[45:49])]
        df_train = df_train[pandas.notnull(df_train['L33P'])]
        Y_train = df_train['L33P'].values
        del df_train['L33P']
 
        #ENCODING
        X_train = df_train.to_dict('records')
        X_tr = []
        X_tr.extend(X_train)
        #One Hot Encoding 
        enc = DictVectorizer(sparse = True)
        X_encoded_train =enc.fit_transform(X_tr)
        estimator = GradientBoostingClassifier() 
        estimator.fit(X_encoded_train.toarray(),Y_train)
        joblib.dump(estimator, 'EstL33.pkl') 
       
        URL = 'DUMB.csv'
        DUMB = pandas.read_csv(URL)
        db = MySQLdb.connect("localhost","root","","ge_hackathon" )
        cursor = db.cursor()
        sql = "SELECT * FROM answers WHERE finished='1'"
        data = frame_query(sql, db)
        Questions = list(data.columns[2:8])
        df_training = data
        df_train = df_training[Questions + list(data.columns[35:39])]
        df_train = df_train[pandas.notnull(df_train['L31P'])]
        Y_train = df_train['L31P'].values
        del df_train['L31P']
        #ENCODING
        X_train = df_train.to_dict('records')
        X_tr = []
        X_tr.extend(X_train)
        #One Hot Encoding 
        enc = DictVectorizer(sparse = True)
        X_encoded_train =enc.fit_transform(X_tr)
        estimator = GradientBoostingClassifier() 
        estimator.fit(X_encoded_train.toarray(),Y_train)
        joblib.dump(estimator, 'EstL31.pkl') 
        
        URL = 'DUMB.csv'
        DUMB = pandas.read_csv(URL)
        db = MySQLdb.connect("localhost","root","","ge_hackathon" )
        cursor = db.cursor()
        sql = "SELECT * FROM answers WHERE finished='1'"
        data = frame_query(sql, db)
        Questions = list(data.columns[2:8])
        df_training = data
        df_train = df_training[Questions + list(data.columns[40:44])]
        df_train = df_train[pandas.notnull(df_train['L32P'])]
        Y_train = df_train['L32P'].values
        del df_train['L32P']
        #ENCODING
        X_train = df_train.to_dict('records')
        X_tr = []
        X_tr.extend(X_train)
        #One Hot Encoding 
        enc = DictVectorizer(sparse = True)
        X_encoded_train =enc.fit_transform(X_tr)
        estimator = GradientBoostingClassifier() 
        estimator.fit(X_encoded_train.toarray(),Y_train)
        joblib.dump(estimator, 'EstL32.pkl') 

        URL = 'DUMB.csv'
        DUMB = pandas.read_csv(URL)
        #L21 Night
        db = MySQLdb.connect("localhost","root","","ge_hackathon" )
        cursor = db.cursor()
        sql = "SELECT * FROM answers WHERE finished='1'"
        data = frame_query(sql, db)
        Questions = list(data.columns[2:8])
        df_training = data
        df_train = df_training[Questions + list(data.columns[45:49])]
        df_train = df_train[pandas.notnull(df_train['L33P'])]
        Y_train = df_train['L33P'].values
        del df_train['L33P']
 
        #ENCODING
        X_train = df_train.to_dict('records')
        X_tr = []
        X_tr.extend(X_train)
        #One Hot Encoding 
        enc = DictVectorizer(sparse = True)
        X_encoded_train =enc.fit_transform(X_tr)
        estimator = GradientBoostingClassifier() 
        estimator.fit(X_encoded_train.toarray(),Y_train)
        joblib.dump(estimator, 'EstL33.pkl') 
        return 'success'

@app.route("/L11")
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
    Predictions_prob = clf2.predict_proba(X_encoded_test.toarray()[-1])
    return  str(int(Predictions)) + '|' +str(float(Predictions_prob.max())) 


@app.route("/N21")
def L21Night():
        #L21 Night
        db = MySQLdb.connect("localhost","root","","ge_hackathon" )
        cursor = db.cursor()
        sql = "SELECT * FROM answers WHERE finished='1'"
        data = frame_query(sql, db)
        df_training = data
        df_train = df_training[list(data.columns[2:8])+ list(data.columns[10:16])]
        Y_train = df_train['L21P'].values
        del df_train['L21P']
        #ENCODING
        X_train = df_train.to_dict('records')
        X_tr = []
        X_tr.extend(X_train)
        #One Hot Encoding 
        enc = DictVectorizer(sparse = True)
        X_encoded_train =enc.fit_transform(X_tr)
        estimator = GradientBoostingClassifier() 
        estimator.fit(X_encoded_train.toarray(),Y_train)
        joblib.dump(estimator, 'EstL21.pkl') 
        return 'success'

@app.route("/L21")
def L21pred():
    URL = 'DUMB.csv'
    DUMB = pandas.read_csv(URL)
    df_DUMB = DUMB[DUMB.columns[1:7]+ DUMB.columns[9:14]]

    db = MySQLdb.connect("localhost","root","","ge_hackathon" )
    cursor = db.cursor()
    sql = "SELECT * FROM answers WHERE finished='0'"
    data = frame_query(sql, db)

    df_test = data[list(data.columns[2:8])+ list(data.columns[10:15])]
    frames = [ df_DUMB,df_test]
    df_WH = pandas.concat(frames)

    #ENCODING
    X_test = df_WH.to_dict('records')
    X_te = []
    X_te.extend(X_test)


    encoder = DictVectorizer(sparse = True)
    X_encoded_test = encoder.fit_transform(X_te)
    clf2 = joblib.load('EstL21.pkl') 
    Predictions = clf2.predict(X_encoded_test.toarray()[-1])
    Predictions_prob = clf2.predict_proba(X_encoded_test.toarray()[-1])
    return  str(int(Predictions)) + '|' +str(float(Predictions_prob.max())) 

@app.route("/N22")
def L22Night():
        #L21 Night
        db = MySQLdb.connect("localhost","root","","ge_hackathon" )
        cursor = db.cursor()
        sql = "SELECT * FROM answers WHERE finished='1'"
        data = frame_query(sql, db)
        df_training = data
        df_train = df_training[list(data.columns[2:8])+ list(data.columns[17:22])]
        Y_train = df_train['L22P'].values
        del df_train['L22P']
        #ENCODING
        X_train = df_train.to_dict('records')
        X_tr = []
        X_tr.extend(X_train)
        #One Hot Encoding 
        enc = DictVectorizer(sparse = True)
        X_encoded_train =enc.fit_transform(X_tr)
        estimator = GradientBoostingClassifier() 
        estimator.fit(X_encoded_train.toarray(),Y_train)
        joblib.dump(estimator, 'EstL22.pkl') 
        return 'success'

@app.route("/L22")
def L22pred():
    URL = 'DUMB.csv'
    DUMB = pandas.read_csv(URL)
    df_DUMB = DUMB[DUMB.columns[1:7]+ DUMB.columns[16:20]] 

    db = MySQLdb.connect("localhost","root","","ge_hackathon" )
    cursor = db.cursor()
    sql = "SELECT * FROM answers WHERE finished='0'"
    data = frame_query(sql, db)

    df_test = data[list(data.columns[2:8])+ list(data.columns[17:21])]
    frames = [ df_DUMB,df_test]
    df_WH = pandas.concat(frames)

    #ENCODING
    X_test = df_WH.to_dict('records')
    X_te = []
    X_te.extend(X_test)


    encoder = DictVectorizer(sparse = True)
    X_encoded_test = encoder.fit_transform(X_te)
    clf2 = joblib.load('EstL22.pkl') 
    Predictions = clf2.predict(X_encoded_test.toarray()[-1])
    Predictions_prob = clf2.predict_proba(X_encoded_test.toarray()[-1])
    return  str(int(Predictions)) + '|' +str(float(Predictions_prob.max())) 

@app.route("/N23")
def L23Night():
    #L21 Night
    db = MySQLdb.connect("localhost","root","","ge_hackathon" )
    cursor = db.cursor()
    sql = "SELECT * FROM answers WHERE finished='1'"
    data = frame_query(sql, db)
    df_training = data
    df_train = df_training[list(data.columns[2:8])+ list(data.columns[23:28])]
    Y_train = df_train['L23P'].values
    del df_train['L23P']
    #ENCODING
    X_train = df_train.to_dict('records')
    X_tr = []
    X_tr.extend(X_train)
    #One Hot Encoding 
    enc = DictVectorizer(sparse = True)
    X_encoded_train =enc.fit_transform(X_tr)
    estimator = GradientBoostingClassifier() 
    estimator.fit(X_encoded_train.toarray(),Y_train)
    joblib.dump(estimator, 'EstL23.pkl') 
    return 'success'

@app.route("/L23")
def L23pred():
    URL = 'DUMB.csv'
    DUMB = pandas.read_csv(URL)
    df_DUMB = DUMB[DUMB.columns[1:7]+ DUMB.columns[22:26]] 

    db = MySQLdb.connect("localhost","root","","ge_hackathon" )
    cursor = db.cursor()
    sql = "SELECT * FROM answers WHERE finished='0'"
    data = frame_query(sql, db)
    df_test = data[list(data.columns[2:8])+ list(data.columns[23:27])]
    frames = [ df_DUMB,df_test]
    df_WH = pandas.concat(frames)
    #ENCODING
    X_test = df_WH.to_dict('records')
    X_te = []
    X_te.extend(X_test)
    encoder = DictVectorizer(sparse = True)
    X_encoded_test = encoder.fit_transform(X_te)
    clf2 = joblib.load('EstL23.pkl') 
    Predictions = clf2.predict(X_encoded_test.toarray()[-1])
    Predictions_prob = clf2.predict_proba(X_encoded_test.toarray()[-1])
    return  str(int(Predictions)) + '|' +str(float(Predictions_prob.max())) 

@app.route("/N24")
def L24Night():
        #L21 Night
        db = MySQLdb.connect("localhost","root","","ge_hackathon" )
        cursor = db.cursor()
        sql = "SELECT * FROM answers WHERE finished='1'"
        data = frame_query(sql, db)
        df_training = data
        df_train = df_training[list(data.columns[2:8])+ list(data.columns[29:34])]
        Y_train = df_train['L24P'].values
        del df_train['L24P']
        #ENCODING
        X_train = df_train.to_dict('records')
        X_tr = []
        X_tr.extend(X_train)
        #One Hot Encoding 
        enc = DictVectorizer(sparse = True)
        X_encoded_train =enc.fit_transform(X_tr)
        estimator = GradientBoostingClassifier() 
        estimator.fit(X_encoded_train.toarray(),Y_train)
        joblib.dump(estimator, 'EstL24.pkl') 
        return 'success'

@app.route("/L24")
def L24pred():
    URL = 'DUMB.csv'
    DUMB = pandas.read_csv(URL)
    df_DUMB = DUMB[DUMB.columns[1:7]+ DUMB.columns[28:32]] 

    db = MySQLdb.connect("localhost","root","","ge_hackathon" )
    cursor = db.cursor()
    sql = "SELECT * FROM answers WHERE finished='0'"
    data = frame_query(sql, db)
    df_test = data[list(data.columns[2:8])+ list(data.columns[29:33])]
    frames = [ df_DUMB,df_test]
    df_WH = pandas.concat(frames)

    #ENCODING
    X_test = df_WH.to_dict('records')
    X_te = []
    X_te.extend(X_test)
    encoder = DictVectorizer(sparse = True)
    X_encoded_test = encoder.fit_transform(X_te)
    clf2 = joblib.load('EstL24.pkl') 
    Predictions = clf2.predict(X_encoded_test.toarray()[-1])
    Predictions_prob = clf2.predict_proba(X_encoded_test.toarray()[-1])
    return  str(int(Predictions)) + '|' +str(float(Predictions_prob.max())) 

@app.route("/N31")
def L31Night():
        #L21 Night
        URL = 'DUMB.csv'
        DUMB = pandas.read_csv(URL)
        db = MySQLdb.connect("localhost","root","","ge_hackathon" )
        cursor = db.cursor()
        sql = "SELECT * FROM answers WHERE finished='1'"
        data = frame_query(sql, db)
        N=str(int(data['L11A'].values[0]))
        if N == '21':
            Questions = list(data.columns[2:8])+ list(data.columns[10:15])
            Dumbster = DUMB[DUMB.columns[1:7]+ DUMB.columns[9:14]]
        elif N=='22':
            Questions = list(data.columns[2:8])+ list(data.columns[17:21])
            Dumbster = DUMB.columns[1:7]+ DUMB.columns[16:20]
        elif N=='23':
            Questions =list(data.columns[2:8])+ list(data.columns[23:27])
            Dumbster = DUMB.columns[1:7]+ DUMB.columns[22:26]
        else:
            Questions = list(data.columns[2:8])+ list(data.columns[29:33])
            Dumbster = DUMB.columns[1:7]+ DUMB.columns[28:32]
        df_training = data
        df_train = df_training[Questions + list(data.columns[35:39])]
        Y_train = df_train['L31P'].values
        del df_train['L31P']
        #ENCODING
        X_train = df_train.to_dict('records')
        X_tr = []
        X_tr.extend(X_train)
        #One Hot Encoding 
        enc = DictVectorizer(sparse = True)
        X_encoded_train =enc.fit_transform(X_tr)
        estimator = GradientBoostingClassifier() 
        estimator.fit(X_encoded_train.toarray(),Y_train)
        joblib.dump(estimator, 'EstL31.pkl') 
        return 'success'

@app.route("/L31")
def L31pred():
    URL = 'DUMB.csv'
    DUMB = pandas.read_csv(URL)
    db = MySQLdb.connect("localhost","root","","ge_hackathon" )
    cursor = db.cursor()
    sql = "SELECT * FROM answers WHERE finished='0'"
    data = frame_query(sql, db)
    Questions = list(data.columns[2:8])

    URL = 'DUMB.csv'
    DUMB = pandas.read_csv(URL)
    Dumbster = DUMB.columns[1:7]
    df_DUMB = DUMB[Dumbster + DUMB.columns[34:37] ] 
    df_test = data[ Questions + list(data.columns[35:38])]
    frames = [ df_DUMB,df_test]
    df_WH = pandas.concat(frames)
    #ENCODING
    X_test = df_WH.to_dict('records')
    X_te = []
    X_te.extend(X_test)
    encoder = DictVectorizer(sparse = True)
    X_encoded_test = encoder.fit_transform(X_te)
    clf2 = joblib.load('EstL31.pkl') 
    Predictions = clf2.predict(X_encoded_test.toarray()[-1])
    Predictions_prob = clf2.predict_proba(X_encoded_test.toarray()[-1])
    return  str(int(Predictions)) 

@app.route("/N32")
def L32Night():
    URL = 'DUMB.csv'
    DUMB = pandas.read_csv(URL)
    db = MySQLdb.connect("localhost","root","","ge_hackathon" )
    cursor = db.cursor()
    sql = "SELECT * FROM answers WHERE finished='1'"
    data = frame_query(sql, db)
    N=str(int(data['L11A'].values[0]))
    if N == '21':
        Questions = list(data.columns[2:8])+ list(data.columns[10:15])
        Dumbster = DUMB[DUMB.columns[1:7]+ DUMB.columns[9:14]]
    elif N=='22':
        Questions = list(data.columns[2:8])+ list(data.columns[17:21])
        Dumbster = DUMB.columns[1:7]+ DUMB.columns[16:20]
    elif N=='23':
        Questions =list(data.columns[2:8])+ list(data.columns[23:27])
        Dumbster = DUMB.columns[1:7]+ DUMB.columns[22:26]
    else:
        Questions = list(data.columns[2:8])+ list(data.columns[29:33])
        Dumbster = DUMB.columns[1:7]+ DUMB.columns[28:32]
    df_training = data
    df_train = df_training[Questions + list(data.columns[40:44])]
    Y_train = df_train['L32P'].values
    del df_train['L32P']
    #ENCODING
    X_train = df_train.to_dict('records')
    X_tr = []
    X_tr.extend(X_train)
    #One Hot Encoding 
    enc = DictVectorizer(sparse = True)
    X_encoded_train =enc.fit_transform(X_tr)
    estimator = GradientBoostingClassifier() 
    estimator.fit(X_encoded_train.toarray(),Y_train)
    joblib.dump(estimator, 'EstL32.pkl') 
    return 'success'

@app.route("/L32")
def L32pred():
    URL = 'DUMB.csv'
    DUMB = pandas.read_csv(URL)
    db = MySQLdb.connect("localhost","root","","ge_hackathon" )
    cursor = db.cursor()
    sql = "SELECT * FROM answers WHERE finished='0'"
    data = frame_query(sql, db)
    URL = 'DUMB.csv'
    DUMB = pandas.read_csv(URL)
    Questions = list(data.columns[2:8])
    Dumbster = DUMB.columns[1:7]
    df_DUMB = DUMB[Dumbster + DUMB.columns[39:42] ] 
    df_test = data[ Questions + list(data.columns[40:43])]
    frames = [ df_DUMB,df_test]
    df_WH = pandas.concat(frames)
    #ENCODING
    X_test = df_WH.to_dict('records')
    X_te = []
    X_te.extend(X_test)
    encoder = DictVectorizer(sparse = True)
    X_encoded_test = encoder.fit_transform(X_te)
    clf2 = joblib.load('EstL32.pkl') 
    Predictions = clf2.predict(X_encoded_test.toarray()[-1])
    Predictions_prob = clf2.predict_proba(X_encoded_test.toarray()[-1])
    return  str(int(Predictions)) 

@app.route("/N33")
def L33Night():
        URL = 'DUMB.csv'
        DUMB = pandas.read_csv(URL)
        #L21 Night
        db = MySQLdb.connect("localhost","root","","ge_hackathon" )
        cursor = db.cursor()
        sql = "SELECT * FROM answers WHERE finished='1'"
        data = frame_query(sql, db)
        N=str(int(data['L11A'].values[0]))
        if N == '21':
            Questions = list(data.columns[2:8])+ list(data.columns[10:15])
            Dumbster = DUMB[DUMB.columns[1:7]+ DUMB.columns[9:14]]
        elif N=='22':
            Questions = list(data.columns[2:8])+ list(data.columns[17:21])
            Dumbster = DUMB.columns[1:7]+ DUMB.columns[16:20]
        elif N=='23':
            Questions =list(data.columns[2:8])+ list(data.columns[23:27])
            Dumbster = DUMB.columns[1:7]+ DUMB.columns[22:26]
        else:
            Questions = list(data.columns[2:8])+ list(data.columns[29:33])
            Dumbster = DUMB.columns[1:7]+ DUMB.columns[28:32]
        df_training = data
        df_train = df_training[Questions + list(data.columns[45:49])]
        Y_train = df_train['L33P'].values
        del df_train['L33P']
 
        #ENCODING
        X_train = df_train.to_dict('records')
        X_tr = []
        X_tr.extend(X_train)
        #One Hot Encoding 
        enc = DictVectorizer(sparse = True)
        X_encoded_train =enc.fit_transform(X_tr)
        estimator = GradientBoostingClassifier() 
        estimator.fit(X_encoded_train.toarray(),Y_train)
        joblib.dump(estimator, 'EstL33.pkl') 
        return 'success'

@app.route("/L33")
def L33pred():
    URL = 'DUMB.csv'
    DUMB = pandas.read_csv(URL)
    db = MySQLdb.connect("localhost","root","","ge_hackathon" )
    cursor = db.cursor()
    sql = "SELECT * FROM answers WHERE finished='0'"
    data = frame_query(sql, db)
    URL = 'DUMB.csv'
    DUMB = pandas.read_csv(URL)
    Dumbster = DUMB.columns[1:7]
    df_DUMB = DUMB[Dumbster + DUMB.columns[44:47]]    
    Questions = list(data.columns[2:8])
    Dumbster = DUMB.columns[1:7]
    df_test = data[ Questions + list(data.columns[45:48])]
    frames = [ df_DUMB,df_test]
    df_WH = pandas.concat(frames)

    #ENCODING
    X_test = df_WH.to_dict('records')
    X_te = []
    X_te.extend(X_test)

    encoder = DictVectorizer(sparse = True)
    X_encoded_test = encoder.fit_transform(X_te)
    clf2 = joblib.load('EstL33.pkl') 
    Predictions = clf2.predict(X_encoded_test.toarray()[-1])
    Predictions_prob = clf2.predict_proba(X_encoded_test.toarray()[-1])
    return  str(int(Predictions)) 

if __name__ == "__main__":
    app.run()