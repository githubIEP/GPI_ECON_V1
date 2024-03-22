from make_request import make_call_json
from settings import DRAGONFLY_HOST


def get_countries_json():
    url = f'{DRAGONFLY_HOST}/core/v1/countries'
    return make_call_json(endpoint_name='countries', params={}, url=url)


if __name__ == '__main__':
    get_countries_json()
