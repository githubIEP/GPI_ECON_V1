from datetime import date

from make_request import make_call_json
from settings import DRAGONFLY_HOST


def get_location_risk_ratings_json(page: int = 1):
    url = f'{DRAGONFLY_HOST}/core/v1/risk_ratings'
    params = {
        'page': page,
        'limit': 10,
    }
    return make_call_json(endpoint_name='risk_ratings', params=params, url=url)


if __name__ == '__main__':
    get_location_risk_ratings_json()
