from make_request import make_call_json
from settings import DRAGONFLY_HOST


def get_cities_geojson():
    url = f'{DRAGONFLY_HOST}/core/v1/cities'
    return make_call_json(endpoint_name='cities', params={'geojson': True}, url=url)


if __name__ == '__main__':
    get_cities_geojson()
