from datetime import date

from make_request import make_call_csv
from settings import DRAGONFLY_HOST


def get_incidents_csv(from_date: str = '2022-02-20', to_date: str = f'{date.today():%Y-%m-%d}'):
    url = f'{DRAGONFLY_HOST}/core/v1/incidents?limit=2'
    params = {
        'from_date': from_date,
        'to_date': to_date
    }
    return make_call_csv(endpoint_name='incidents', params=params, url=url)


if __name__ == '__main__':
    get_incidents_csv()
