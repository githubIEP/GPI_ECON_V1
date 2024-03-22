from OpenSSL import crypto

from settings import COMPANY_EMAIL
from settings import COUNTRY
from settings import STATE_OR_PROVINCE_NAME
from settings import ORGANIZATION_NAME
from settings import ORGANIZATION_UNIT_NAME
from settings import CERT_VALID_FOR


def generate_public_private_key_pair():
    k = crypto.PKey()
    k.generate_key(crypto.TYPE_RSA, 4096)
    cert = crypto.X509()
    cert.get_subject().C = COUNTRY
    cert.get_subject().ST = STATE_OR_PROVINCE_NAME
    cert.get_subject().L = 'localityName'
    cert.get_subject().O = ORGANIZATION_NAME
    cert.get_subject().OU = ORGANIZATION_UNIT_NAME
    cert.get_subject().CN = 'commonName'
    cert.get_subject().emailAddress = COMPANY_EMAIL
    cert.set_serial_number(0)
    cert.gmtime_adj_notBefore(0)
    cert.gmtime_adj_notAfter(CERT_VALID_FOR)
    cert.set_issuer(cert.get_subject())
    cert.set_pubkey(k)
    cert.sign(k, 'sha512')
    with open('certs/public.key', "wt") as f:
        f.write(crypto.dump_certificate(crypto.FILETYPE_PEM, cert).decode("utf-8"))
    with open('certs/private.key', "wt") as f:
        f.write(crypto.dump_privatekey(crypto.FILETYPE_PEM, k).decode("utf-8"))


generate_public_private_key_pair()
