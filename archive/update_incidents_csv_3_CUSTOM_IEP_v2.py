from datetime import date
from make_request import make_call_csv
from settings import DRAGONFLY_HOST
import pandas as pd
import datetime


#START_YEAR = 2007
#END_YEAR = 2022+1

###
# Get the date for yesterday
# START_DATE = (datetime.datetime.today() - datetime.timedelta(1)).strftime('%Y-%m-%d')
# #START_DATE = "2022-08-01" # can also set a desired start date
# # Get the date for today
# TODAY  = datetime.datetime.today().strftime('%Y-%m-%d')
# 
# print(START_DATE)
# print(TODAY)
###



# # short name of file
#DF = pd.read_csv("./responses/incidents_from_date_2022-01-01__to_date_2022-12-31.csv")
DFNAME = "C:/Users/aetchell/Desktop/R_Projects/terrorism-tracker-2022-v3/responses/incidents_from_date_2022-01-01__to_date_2022-12-31.csv"
# print(DFNAME)
DF = pd.read_csv(DFNAME)

# print(DF["start_date"].head())
# print(DF["start_date"].max())
# TODAY = datetime.datetime.today().strftime('%Y-%m-%d')
# print(TODAY)

# Do not want to convert, but this gives ther correct information 
#DF["start_date"] = pd.to_datetime( DF["start_date"], errors = 'coerce', dayfirst = True)
# 
DATES = pd.to_datetime( DF["start_date"], errors = 'coerce', dayfirst = True)
START_DATE = DATES.max().strftime('%d/%m/%Y')
print(START_DATE)
START_DATE = pd.to_datetime(START_DATE, format='%d/%m/%Y').strftime('%Y-%m-%d')
print(START_DATE)
TODAY = datetime.datetime.today().strftime('%Y-%m-%d')
print(TODAY)

#START_DATE = max(DF["start_date"])#pd.to_datetime(DF["start_date"], format='%Y-%m-%d').dt.strftime('%Y-%m-%d').max
#DF["start_date"] = pd.to_datetime(DF["start_date"], format='%Y-%m-%d')
#DF["start_date"] = datetime.datetime.strptime(DF["start_date"], "%d/%m/%Y").strftime('%Y-%m-%d')
#DF["start_date"] = pd.to_datetime(DF['start_date'], format='%d/%m/%Y')

# DF["start_date"] = pd.to_datetime(DF["start_date"], format='%m/d/%y').dt.strftime('%Y-%m-%d')
#
# #START_DATE = max(DF["start_date"])
# print(max(DF["start_date"]))


# # gets the maximum start time
# #START_TIME = max(DF["start_date"])
# START_DATE = DF.loc[1,"start_date"] # get tehe first start date (mnax doesnt wuite work for some reason... need to figure out why)
# #START_TIME = DF["start_date"].max()
# START_DATE = datetime.datetime.strptime(START_DATE, "%d/%m/%Y").strftime("%Y-%m-%d")
# print(START_DATE)
# TODAY = datetime.datetime.today().strftime('%Y-%m-%d')
# print(TODAY)
#

def update_incidents_csv_v3X():

    #MY_YEAR = i
    from_date = str(START_DATE) #str(MY_YEAR)+'-01-01'
    to_date = str(TODAY)#str(MY_YEAR)+'-12-31'


    url = f'{DRAGONFLY_HOST}/core/v1/incidents?limit=2'
    params = {
      'from_date': from_date,
      'to_date': to_date
    }
    return make_call_csv(endpoint_name='incidents', params=params, url=url)

# Get the updated incidents
update_incidents_csv_v3X()

# Open the file and append to the original
FNAME = "C:/Users/aetchell/Desktop/R_Projects/terrorism-tracker-2022-v3/responses/incidents_from_date_" + str(START_DATE) + "__to_date_" + str(TODAY) + ".csv"
print(FNAME)
FNAME = pd.read_csv(FNAME)

# Convert to date format and sort in descending order (i.e. newest first)
# current seems to be correct, but might need to fix from UTC
#FNAME["start_date"] = pd.to_datetime(FNAME["start_date"], format='%Y-%m-%d').dt.strftime('%d/%m/%Y') # ORIGNAL runs, but doesnt produce the right dates in the CSV
#FNAME["start_date"] = pd.to_datetime(FNAME["start_date"], format='%Y-%m-%d').dt.strftime('%Y-%m-%d') #ORIGINAL
#FNAME["start_date"] = pd.to_datetime(FNAME["start_date"], format='%Y-%m-%d').dt.strftime('%d/%m/%Y') # TESTING
#FNAME["start_date"] = pd.to_datetime(FNAME["start_date"], format='%Y-%m-%d').dt.strftime('%d/%m/%Y') #ORIGINAL
#FNAME["start_date"].dt.tz_localize(None)

# trying to remove the UTC fromt he start date when updating
#NAME["start_date"] = pd.to_datetime(FNAME["start_date"], format='%Y-%m-%d').dt.strftime('%Y-%m-%d') #ORIGINAL
#FNAME["start_date"]  = pd.to_datetime(FNAME["start_date"]).dt.date

FNAME = FNAME.sort_values(["start_date"], ascending=False)

# Append the old data to the new data
#DF = DF["start_date"] = pd.to_datetime(DF["start_date"], format='%d/%m/%Y').dt.strftime('%Y-%m-%d')
DF = FNAME.append(DF)
# drop dupliocates other than the lst one (remove any overlaps between the two datasets)
DF = DF.drop_duplicates(keep='last')
DF.to_csv(DFNAME, index=False)
