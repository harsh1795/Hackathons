#!/usr/bin/python

import MySQLdb
from pandas.io.sql import frame_query
# Open database connection
db = MySQLdb.connect("localhost","root","","ge_hackathon" )

# prepare a cursor object using cursor() method
cursor = db.cursor()

# Prepare SQL query to INSERT a record into the database.
sql = "SELECT * FROM answers WHERE finished='0'"
data = frame_query(sql, db)
# disconnect from server
db.close()