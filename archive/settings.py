"""Default configuration

Use env var to override
"""
from environs import Env
from pathlib import Path

env = Env()
env.read_env()

X_ENDPOINTS_HEADER = env.str('X_ENDPOINTS_HEADER')
DRAGONFLY_HOST = env.str('DRAGONFLY_HOST')
COMPANY_EMAIL = env.str('COMPANY_EMAIL')
COUNTRY = env.str('COUNTRY')
STATE_OR_PROVINCE_NAME = env.str('STATE_OR_PROVINCE_NAME')
ORGANIZATION_NAME = env.str('ORGANIZATION_NAME')
ORGANIZATION_UNIT_NAME = env.str('ORGANIZATION_UNIT_NAME')
CERT_VALID_FOR = 10 * 365 * 24 * 60 * 60
