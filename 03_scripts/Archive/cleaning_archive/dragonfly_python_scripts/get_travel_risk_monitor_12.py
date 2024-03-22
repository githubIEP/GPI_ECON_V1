from datetime import date
from make_request import make_call_json
from settings import DRAGONFLY_HOST


def get_travel_risk_monitor_json(page: int = 1):
    url = f'{DRAGONFLY_HOST}/core/v1//travel_risk_monitor_alerts'
    params = {
        'page': page,
        'limit': 10,
    }
    return make_call_json(endpoint_name='travel_risk_monitor', params=params, url=url)


if __name__ == '__main__':
    get_travel_risk_monitor_json()
