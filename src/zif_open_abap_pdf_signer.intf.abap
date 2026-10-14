"! Creates the cryptographic signature embedded in a signed PDF.
"! zcl_open_abap_pdf_signer is a pure ABAP implementation, alternative
"! implementations can delegate to SSF, an HSM or a remote signing service.
INTERFACE zif_open_abap_pdf_signer PUBLIC.

  "! Sign data, returns a DER encoded detached CMS / PKCS#7 SignedData structure
  "! @parameter iv_data | Bytes covered by the PDF /ByteRange
  "! @parameter iv_signing_time | UTC time, format YYYYMMDDhhmmss
  "! @parameter rv_cms | DER encoded ContentInfo containing SignedData
  METHODS sign
    IMPORTING iv_data         TYPE xstring
              iv_signing_time TYPE string
    RETURNING VALUE(rv_cms)   TYPE xstring
    RAISING   zcx_open_abap_pdf.

ENDINTERFACE.
