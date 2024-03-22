from datetime import date

from make_request import make_call_json
from settings import DRAGONFLY_HOST


def get_incidents_json(from_date: str = '2022-02-20', to_date: str = f'{date.today():%Y-%m-%d}', page: int = 1):
    url = f'{DRAGONFLY_HOST}/core/v1/incidents'
    params = {
        'from_date': from_date,
        'to_date': to_date,
        'page': page,
        'limit': 10,
        'sort': 'start_date',
        'order': 'desc'
    }
    return make_call_json(endpoint_name='incidents', params=params, url=url)


if __name__ == '__main__':
    get_incidents_json()
