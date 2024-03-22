from make_request import make_call_json
from settings import DRAGONFLY_HOST


def get_regions_geojson():
    url = f'{DRAGONFLY_HOST}/core/v1/regions'
    return make_call_json(endpoint_name='regions', params={}, url=url)


if __name__ == '__main__':
    get_regions_geojson()
