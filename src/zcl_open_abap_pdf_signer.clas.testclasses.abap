CLASS ltcl_signer DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION MEDIUM.
  PRIVATE SECTION.
    METHODS base64_decode FOR TESTING RAISING cx_static_check.
    METHODS pem_decode FOR TESTING RAISING cx_static_check.
    METHODS sign FOR TESTING RAISING cx_static_check.
    METHODS no_key FOR TESTING RAISING cx_static_check.

    "! Test only RSA-2048 key, generated with openssl, never use for anything else
    CLASS-METHODS get_private_key
      RETURNING VALUE(rv_pem) TYPE string.

    "! Self-signed test certificate matching get_private_key
    CLASS-METHODS get_certificate
      RETURNING VALUE(rv_pem) TYPE string.
ENDCLASS.

CLASS ltcl_signer IMPLEMENTATION.

  METHOD base64_decode.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_open_abap_pdf_signer=>base64_decode( 'TWFu' )
      exp = '4D616E' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_open_abap_pdf_signer=>base64_decode( |TW\nE=| )
      exp = '4D61' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_open_abap_pdf_signer=>base64_decode( 'TQ==' )
      exp = '4D' ).
  ENDMETHOD.

  METHOD pem_decode.
    DATA lt_pems TYPE zcl_open_abap_pdf_signer=>ty_pems.

    lt_pems = zcl_open_abap_pdf_signer=>pem_decode( get_private_key( ) && get_certificate( ) ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_pems )
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_pems[ 1 ]-label
      exp = 'PRIVATE KEY' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_pems[ 2 ]-label
      exp = 'CERTIFICATE' ).
  ENDMETHOD.

  METHOD sign.
    DATA lo_signer TYPE REF TO zif_open_abap_pdf_signer.
    DATA lv_cms TYPE xstring.
    DATA lt_nodes TYPE zcl_open_abap_pdf_asn1=>ty_nodes.
    DATA lt_fields TYPE zcl_open_abap_pdf_asn1=>ty_nodes.

    lo_signer = zcl_open_abap_pdf_signer=>create_from_pem(
      iv_private_key  = get_private_key( )
      iv_certificates = get_certificate( ) ).

    lv_cms = lo_signer->sign(
      iv_data         = cl_abap_codepage=>convert_to( 'hello world' )
      iv_signing_time = '20261014120000' ).

* ContentInfo with OID signedData
    lt_nodes = zcl_open_abap_pdf_asn1=>decode( lv_cms ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_nodes )
      exp = 1 ).
    lt_fields = zcl_open_abap_pdf_asn1=>decode( lt_nodes[ 1 ]-content ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_fields[ 1 ]-raw
      exp = zcl_open_abap_pdf_asn1=>oid( '1.2.840.113549.1.7.2' ) ).
  ENDMETHOD.

  METHOD no_key.
    TRY.
        zcl_open_abap_pdf_signer=>create_from_pem(
          iv_private_key  = get_certificate( )
          iv_certificates = get_certificate( ) ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_open_abap_pdf ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD get_private_key.
    rv_pem =
      |-----BEGIN PRIVATE KEY-----\n|
      && |MIIEvQIBADANBgkqhkiG9w0BAQEFAASCBKcwggSjAgEAAoIBAQC8S2zx4c6KW/Te\n|
      && |jdHBxxp01oykR5XcexZTSMWy/v8bVbxs+/U4NQn3ZCwiqbaMl9qhla57YYGLJIX6\n|
      && |f/ACEVUzQpfVsVs9X9hpALPqmSAWROu5R/2PD+LbXJ3yRrl462r8na9tzZ3i1i+W\n|
      && |ddjBrPJPvH0dJFW2aEDNxejkO88jEZcyp3/QbsuM4zk1NLummnwGW6/APeo+k580\n|
      && |TlsFgq64mykiSjPMZxHqqtOg3zYMCLhMMYHu+AYPttEEgOZGco7krvaQZAaaX3GR\n|
      && |LPG8JKmmHePsf5CSVWyoXcgXZdZSorVYbKSsgt7T7eh6LnSg/du7qWE233MpsCCR\n|
      && |HUu66YHlAgMBAAECggEAFU9P+Bv/HQeFMf7I425eHyT868ayPi6r+yb8QOK9CmHs\n|
      && |nkTxnrfZ9eafXBjq8QRqmOjww0Povk+W3h/GkOVXSCLcPjIBSnPjcRbEReOojvyj\n|
      && |1OdB2gQJP5w+v+GjMf0qsYZDCWFA4iAtaJDTxC+HiOCjJD4teSRZWTBxyArAyY4N\n|
      && |HDmrjmIV8kbhfEe1HvC3WTU+EIL+BZF/Lhx+talARVK7uK16rAxCknl3TwOPfyPp\n|
      && |JN1v5AXgpLvb2UNAf6jEYH6NWj+srlHr9a1OtGes9IjVgJWu2aRZptKCyXyxBZze\n|
      && |I21jlfe8a+IQs9KR0un8FftDjBq3TlNH7q55ubbGTQKBgQD0akCvec6sbrLSNVIt\n|
      && |RkitFUR928eDQCEek9MF3UR3VXnbOwTvW3J//0Ol8nn1QrimDXuv4y+37xURO3ew\n|
      && |OgKKFBfz7YQE0i3XJgsNx9+zBLaPlF271f5AKTemswfrGsfy3wC8yY0TCS8b8MX7\n|
      && |82Ks4v9PsSV0lglttrFgQE7G5wKBgQDFODRKfHdzfs2pihyzWrL+VXmoW4D66Dxs\n|
      && |93i3NbSSWYjiGRG5U4z32jaNyi9SknQy8E5RSb7TMocm+1sW9A+DRRTbvo4scW+e\n|
      && |F0kWLxIBojRrCndhIfsuLpWpsWxLTFxFCRcrl+DU86EPo2xZbNPbbIzif/QRzORM\n|
      && |sG/XLWkzUwKBgQCskGQxQF/yEIpCrhDiFFB1m6Fq9wRLH7kCfL4t0sZnNrs9VNMs\n|
      && |J7WUt2ZMwQDob/4shzJ/wXhMdgm6y/C6RIrMcFllCJoASWDCGYMObML9AuPeRZNU\n|
      && |bkb/h9pZ4OdDT/pB+L7ti6QF/6L6KCmwtGtDw1UdL+ZRBSmNA8MCQQUdmwKBgBmA\n|
      && |LPK8qU1bxgf3/cILJj3Qm4xzWHgY80c7sHYwEYsi3Jovl6UY8o9RveATlo7ctP2p\n|
      && |KNdF69CvJd6afaq4IBFfK1VGjGeeu0n7xM2DhJaFfLWigtEibbII//0THeeWlg+v\n|
      && |HznZx46Bef/lp6rgcXxbytO2q75pedOYtL1j98hVAoGAXzOAlwhPnMwnvuUOuOU6\n|
      && |qa3v21HL7fllGZ/eAuJwOxcE6DdvHTXlHdw2khgIu4lz3KXH8BLisWN85poh6b87\n|
      && |/scqdvzbEJutHuhof4DUYgv3lcVObVhoDKoq+oZCnlChas4fnkO/oJLl46ZbLksY\n|
      && |P1H7EfmxBFfknh9wt8UF6mQ=\n|
      && |-----END PRIVATE KEY-----\n|.
  ENDMETHOD.

  METHOD get_certificate.
    rv_pem =
      |-----BEGIN CERTIFICATE-----\n|
      && |MIIDQzCCAiugAwIBAgIUGRaF7HsFGAEMOC7k+yNtA5Dd9wgwDQYJKoZIhvcNAQEL\n|
      && |BQAwMTEbMBkGA1UEAwwSb3Blbi1hYmFwLXBkZiB0ZXN0MRIwEAYDVQQKDAlvcGVu\n|
      && |LWFiYXAwHhcNMjYxMDE0MTMyMjUxWhcNMzYxMDExMTMyMjUxWjAxMRswGQYDVQQD\n|
      && |DBJvcGVuLWFiYXAtcGRmIHRlc3QxEjAQBgNVBAoMCW9wZW4tYWJhcDCCASIwDQYJ\n|
      && |KoZIhvcNAQEBBQADggEPADCCAQoCggEBALxLbPHhzopb9N6N0cHHGnTWjKRHldx7\n|
      && |FlNIxbL+/xtVvGz79Tg1CfdkLCKptoyX2qGVrnthgYskhfp/8AIRVTNCl9WxWz1f\n|
      && |2GkAs+qZIBZE67lH/Y8P4ttcnfJGuXjravydr23NneLWL5Z12MGs8k+8fR0kVbZo\n|
      && |QM3F6OQ7zyMRlzKnf9Buy4zjOTU0u6aafAZbr8A96j6TnzROWwWCrribKSJKM8xn\n|
      && |Eeqq06DfNgwIuEwxge74Bg+20QSA5kZyjuSu9pBkBppfcZEs8bwkqaYd4+x/kJJV\n|
      && |bKhdyBdl1lKitVhspKyC3tPt6HoudKD927upYTbfcymwIJEdS7rpgeUCAwEAAaNT\n|
      && |MFEwHQYDVR0OBBYEFMOINTF9pzuElj/vNFQV0SXFk20hMB8GA1UdIwQYMBaAFMOI\n|
      && |NTF9pzuElj/vNFQV0SXFk20hMA8GA1UdEwEB/wQFMAMBAf8wDQYJKoZIhvcNAQEL\n|
      && |BQADggEBAISgGGwiVuC8Ouv0WU1yqHluuv1LjlUPeJhCCCWzqSN3cTQ8s5WqJU3Y\n|
      && |wxM6njFADLg3tZmSqlgMZKf2uDMAm4iybOl35vdofKwpYgd0N+9yQ5vVW6yNAnTD\n|
      && |vYvFZG2bQG2Zmi3vwE+Z9iyOf8dC6RbY7g8zeGMJKiKrOCHXcUrkGJmJa6z7PNp1\n|
      && |C8mWKaqrimQFLZnLnTda2sa+vFHiEjp/8hTjXc/diIJfUhst9X+Dl0PBMwbwUeqI\n|
      && |D6JqMGZRVTQxSq3LMxeDq9U2iK2xYAegi9DyHWwvo3DfSVZP783tjuUocGZojREM\n|
      && |dDNClNtOEiyslyiZFov/G4/vGBZisa8=\n|
      && |-----END CERTIFICATE-----\n|.
  ENDMETHOD.

ENDCLASS.
