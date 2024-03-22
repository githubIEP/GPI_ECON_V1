import json
import os

import requests
from logger import logger
from request_headers import get_headers


def make_call_json(endpoint_name: str, url: str, params: dict = None, ):
    headers = get_headers()
    logger.info(f'Making call to {url} with params {params} with headers {headers}')
    r = requests.get(url, params=params, headers=headers)
    logger.info(f'Response status code {r.status_code}')
    if r.status_code > 399:
        logger.error(f'Got error back {r.text}')
    else:
        response_json = r.json()
        parsed_params = "__".join(f'{"".join(str(name))}_{"".join(str(value))}' for name, value in params.items())
        file_name = f'responses/{endpoint_name}_{parsed_params}.json'
        dirname = os.path.dirname(__file__)
        file_path = os.path.join(dirname, file_name)

        with open(file_path, 'w') as incidents_file:
            json.dump(response_json, incidents_file)
        logger.info(f'File saved in {file_name}')
        return file_path


def make_call_csv(endpoint_name: str, params: dict, url: str):
    headers = get_headers()
    headers.update({'Content-Type': 'text/csv'})
    parsed_params = "__".join(f'{"".join(str(name))}_{"".join(str(value))}' for name, value in params.items())
    file_name = f'responses/{endpoint_name}_{parsed_params}.csv'
    dirname = os.path.dirname(__file__)
    file_path = os.path.join(dirname, file_name)

    with requests.get(url, params=params, headers=headers, stream=True) as r:
        with open(file_path, "wb") as file:
            for chunk in r.iter_content(chunk_size=8192):
                file.write(chunk)
    print(f'File saved in {file_name}')
    return file_path
