import sys

import psycopg2

from numpy import arange
from math import *

def f(x):
    return (10*x, floor(x*5), sin(x))

if len(sys.argv) < 3:
    print("USAGE: populate.py TABLE NUM_ENTRIES")
    sys.exit(1)

try:
    table = sys.argv[1]
    entries = int(sys.argv[2])
    conn = psycopg2.connect("postgresql://")
    cur = conn.cursor()
    cur.execute("DROP TABLE IF EXISTS "+table+";")
    cur.execute("CREATE TABLE "+table+"(x real,y real,z real);")
    sql = "INSERT INTO "+table+" VALUES " + str(f(0))
    for x in arange(1.0/entries,1.0,1.0/entries):
        sql += ", " + str(f(x))
    cur.execute(sql + ";")
    conn.commit()
except (Exception, psycopg2.Error) as error :
    print ("Error while connecting to PostgreSQL", error)
finally:
    if(conn):
        cur.close()
        conn.close()
        print("Table "+table+" now has " + str(entries) + " entries.")
