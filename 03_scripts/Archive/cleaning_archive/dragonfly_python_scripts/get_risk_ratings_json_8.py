from make_request import make_call_json
from settings import DRAGONFLY_HOST


def get_risk_ratings_json():
    url = f'{DRAGONFLY_HOST}/core/v1/risk_ratings'
    return make_call_json(endpoint_name='risk_ratings', params={'location_ids': [922, 879, 880]}, url=url)


if __name__ == '__main__':
    get_risk_ratings_json()
