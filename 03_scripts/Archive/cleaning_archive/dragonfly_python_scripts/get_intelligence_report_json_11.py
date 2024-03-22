from datetime import date

from make_request import make_call_json
from settings import DRAGONFLY_HOST


def get_intelligence_reports_json(
        from_date: str = '2007-02-20',
        to_date: str = f'{date.today():%Y-%m-%d}',
        page: int = 1
):
    url = f'{DRAGONFLY_HOST}/core/v1/intelligence_reports'
    params = {
        'from_date': from_date,
        'to_date': to_date,
        'page': page,
        'limit': 10,
        'sort': 'reported_at',
        'order': 'desc'
    }
    return make_call_json(endpoint_name='intelligence_reports', params=params, url=url)


if __name__ == '__main__':
    get_intelligence_reports_json()
