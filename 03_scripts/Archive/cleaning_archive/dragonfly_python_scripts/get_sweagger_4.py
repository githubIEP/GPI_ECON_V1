from logger import logger

import requests

from request_headers import get_headers
from settings import DRAGONFLY_HOST


def get_incidents_csv():
    url = f'{DRAGONFLY_HOST}/core/v1/swagger.yaml'
    r = requests.get(url, headers=get_headers())
    logger.debug(f'Response status code {r.status_code}')
    if r.status_code > 399:
        logger.error(f'Got error back {r.text}')
    else:
        file_name = 'responses/swagger.yaml'
        with open(file_name, 'w') as swagger_file:
            swagger_file.write(r.text)
        logger.debug(f'File saved in {file_name}')


if __name__ == '__main__':
    get_incidents_csv()
