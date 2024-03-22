from datetime import date
from make_request import make_call_csv
from settings import DRAGONFLY_HOST


START_YEAR = 2007
END_YEAR = 2024


def get_incidents_csv_v3X(i):

    MY_YEAR = i
    from_date = str(MY_YEAR)+'-01-01'
    to_date = str(MY_YEAR)+'-12-31'
    
      
    url = f'{DRAGONFLY_HOST}/core/v1/incidents?limit=2'
    params = {
      'from_date': from_date,
      'to_date': to_date
    }
    print(url)
    return make_call_csv(endpoint_name='incidents', params=params, url=url)
  
  
# Get the incidents for each year separately. 
for i in range(START_YEAR, END_YEAR,1):
#for i in range(2022, 2024,1):
  #print(i)
  get_incidents_csv_v3X(i)
  
