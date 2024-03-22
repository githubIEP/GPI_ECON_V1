from datetime import datetime
from datetime import timedelta
from pathlib import Path

import jwt
from cryptography.hazmat.backends import default_backend
from cryptography.hazmat.primitives import serialization

from settings import COMPANY_EMAIL
from settings import DRAGONFLY_HOST
from settings import X_ENDPOINTS_HEADER
import os
PASSPHRASE = None


def get_token():
    token_dict = {
        'iat': datetime.utcnow(),
        'exp': datetime.utcnow() + timedelta(minutes=5),
        'iss': 'dragonfly.com',
        'aud': DRAGONFLY_HOST,
        'email': COMPANY_EMAIL
    }

    dirname = os.path.dirname(__file__)
    private_key_file = os.path.join(dirname, 'certs/private.key')
    private_key = serialization.load_pem_private_key(
        Path(private_key_file).read_bytes(), password=PASSPHRASE, backend=default_backend()
    )
    encoded = jwt.encode(token_dict, private_key, algorithm='RS256')
    return encoded


def get_headers():
    headers = {
        'Authorization': f'Bearer {get_token()}'
    }
    if X_ENDPOINTS_HEADER:
        headers.update({'X-Endpoint-API-UserInfo': X_ENDPOINTS_HEADER})
        headers.update({'X-Forwarded-Authorization': headers['Authorization']})
    return headers


if __name__ == '__main__':
    print(get_token())
