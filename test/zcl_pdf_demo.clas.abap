CLASS zcl_pdf_demo DEFINITION PUBLIC.
  PUBLIC SECTION.
    "! Run the demo and return the generated PDF as binary
    CLASS-METHODS run
      RETURNING VALUE(rv_pdf) TYPE xstring.

    "! Run demo and write PDF to console as base64 (for browser viewing)
    CLASS-METHODS run_base64
      RETURNING VALUE(rv_base64) TYPE string.

    "! Run the demo and return a digitally signed PDF as binary
    CLASS-METHODS run_signed
      RETURNING VALUE(rv_pdf) TYPE xstring
      RAISING   zcx_open_abap_pdf.

    CLASS-METHODS run_signed_base64
      RETURNING VALUE(rv_base64) TYPE string
      RAISING   zcx_open_abap_pdf.

  PRIVATE SECTION.
    CLASS-METHODS build
      RETURNING VALUE(ro_pdf) TYPE REF TO zcl_open_abap_pdf.

    "! Demo RSA-2048 key, generated with openssl, never use for anything else
    CLASS-METHODS get_private_key
      RETURNING VALUE(rv_pem) TYPE string.

    "! Self-signed demo certificate matching get_private_key
    CLASS-METHODS get_certificate
      RETURNING VALUE(rv_pem) TYPE string.
ENDCLASS.

CLASS zcl_pdf_demo IMPLEMENTATION.

  METHOD run.
    rv_pdf = build( )->render_binary( ).
  ENDMETHOD.

  METHOD run_signed.
    DATA(lo_signer) = zcl_open_abap_pdf_signer=>create_from_pem(
      iv_private_key  = get_private_key( )
      iv_certificates = get_certificate( ) ).

    rv_pdf = build( )->render_signed(
      io_signer    = lo_signer
      is_signature = VALUE #(
        name     = 'open-abap-pdf test'
        reason   = 'Demo signature'
        location = 'open-abap' ) ).
  ENDMETHOD.

  METHOD run_signed_base64.
    rv_base64 = cl_http_utility=>encode_x_base64( run_signed( ) ).
  ENDMETHOD.

  METHOD build.
    DATA(lo_pdf) = zcl_open_abap_pdf=>create( ).

    " ========== Page 1: Title Page ==========
    lo_pdf->add_page( ).

    " Title
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 36 ).
    lo_pdf->set_text_color( iv_r = 0 iv_g = 51 iv_b = 102 ).
    lo_pdf->text( iv_x = 50 iv_y = 100 iv_text = 'open-abap-pdf' ).

    " Subtitle
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 18 ).
    lo_pdf->set_text_color( iv_r = 100 iv_g = 100 iv_b = 100 ).
    lo_pdf->text( iv_x = 50 iv_y = 140 iv_text = 'A Pure ABAP PDF Generation Library' ).

    " Description
    lo_pdf->set_font( iv_name = 'Times-Roman' iv_size = 12 ).
    lo_pdf->set_text_color( iv_r = 0 iv_g = 0 iv_b = 0 ).
    lo_pdf->text( iv_x = 50 iv_y = 200 iv_text = 'This demo showcases the PDF generation capabilities:' ).
    lo_pdf->text( iv_x = 70 iv_y = 220 iv_text = '- Multiple pages support' ).
    lo_pdf->text( iv_x = 70 iv_y = 240 iv_text = '- Text with different fonts and colors' ).
    lo_pdf->text( iv_x = 70 iv_y = 260 iv_text = '- Lines, rectangles, and circles' ).
    lo_pdf->text( iv_x = 70 iv_y = 280 iv_text = '- Fluent API for easy chaining' ).

    " Decorative elements
    lo_pdf->set_draw_color( iv_r = 0 iv_g = 51 iv_b = 102 ).
    lo_pdf->set_line_width( iv_width = 2 ).
    lo_pdf->line( iv_x1 = 50 iv_y1 = 110 iv_x2 = 400 iv_y2 = 110 ).

    " Footer
    lo_pdf->set_font( iv_name = 'Courier' iv_size = 10 ).
    lo_pdf->set_text_color( iv_r = 150 iv_g = 150 iv_b = 150 ).
    lo_pdf->text( iv_x = 50 iv_y = 800 iv_text = 'Generated with open-abap-pdf | Page 1' ).

    " ========== Page 2: Shapes Demo ==========
    lo_pdf->add_page( ).

    " Header
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 24 ).
    lo_pdf->set_text_color( iv_r = 0 iv_g = 51 iv_b = 102 ).
    lo_pdf->text( iv_x = 50 iv_y = 50 iv_text = 'Shapes Demo' ).

    " Rectangles section
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 14 ).
    lo_pdf->set_text_color( iv_r = 0 iv_g = 0 iv_b = 0 ).
    lo_pdf->text( iv_x = 50 iv_y = 100 iv_text = 'Rectangles:' ).

    " Outlined rectangle
    lo_pdf->set_draw_color( iv_r = 0 iv_g = 0 iv_b = 0 ).
    lo_pdf->set_line_width( iv_width = 1 ).
    lo_pdf->rect( iv_x = 50 iv_y = 120 iv_width = 80 iv_height = 50 iv_style = 'D' ).
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 10 ).
    lo_pdf->text( iv_x = 60 iv_y = 180 iv_text = 'Outlined' ).

    " Filled rectangle
    lo_pdf->set_fill_color( iv_r = 100 iv_g = 149 iv_b = 237 ).
    lo_pdf->rect( iv_x = 160 iv_y = 120 iv_width = 80 iv_height = 50 iv_style = 'F' ).
    lo_pdf->text( iv_x = 175 iv_y = 180 iv_text = 'Filled' ).

    " Both outlined and filled
    lo_pdf->set_draw_color( iv_r = 139 iv_g = 0 iv_b = 0 ).
    lo_pdf->set_fill_color( iv_r = 255 iv_g = 182 iv_b = 193 ).
    lo_pdf->set_line_width( iv_width = 2 ).
    lo_pdf->rect( iv_x = 270 iv_y = 120 iv_width = 80 iv_height = 50 iv_style = 'DF' ).
    lo_pdf->text( iv_x = 290 iv_y = 180 iv_text = 'Both' ).

    " Circles section
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 14 ).
    lo_pdf->text( iv_x = 50 iv_y = 230 iv_text = 'Circles:' ).

    " Outlined circle
    lo_pdf->set_draw_color( iv_r = 0 iv_g = 100 iv_b = 0 ).
    lo_pdf->set_line_width( iv_width = 1 ).
    lo_pdf->circle( iv_x = 100 iv_y = 290 iv_radius = 30 iv_style = 'D' ).
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 10 ).
    lo_pdf->text( iv_x = 75 iv_y = 335 iv_text = 'Outlined' ).

    " Filled circle
    lo_pdf->set_fill_color( iv_r = 255 iv_g = 215 iv_b = 0 ).
    lo_pdf->circle( iv_x = 210 iv_y = 290 iv_radius = 30 iv_style = 'F' ).
    lo_pdf->text( iv_x = 193 iv_y = 335 iv_text = 'Filled' ).

    " Both
    lo_pdf->set_draw_color( iv_r = 75 iv_g = 0 iv_b = 130 ).
    lo_pdf->set_fill_color( iv_r = 230 iv_g = 230 iv_b = 250 ).
    lo_pdf->set_line_width( iv_width = 3 ).
    lo_pdf->circle( iv_x = 320 iv_y = 290 iv_radius = 30 iv_style = 'DF' ).
    lo_pdf->text( iv_x = 305 iv_y = 335 iv_text = 'Both' ).

    " Lines section
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 14 ).
    lo_pdf->set_text_color( iv_r = 0 iv_g = 0 iv_b = 0 ).
    lo_pdf->text( iv_x = 50 iv_y = 400 iv_text = 'Lines with different widths:' ).

    lo_pdf->set_draw_color( iv_r = 0 iv_g = 0 iv_b = 0 ).
    lo_pdf->set_line_width( iv_width = 1 ).
    lo_pdf->line( iv_x1 = 50 iv_y1 = 420 iv_x2 = 200 iv_y2 = 420 ).
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 10 ).
    lo_pdf->text( iv_x = 210 iv_y = 423 iv_text = '1pt' ).

    lo_pdf->set_line_width( iv_width = 2 ).
    lo_pdf->line( iv_x1 = 50 iv_y1 = 440 iv_x2 = 200 iv_y2 = 440 ).
    lo_pdf->text( iv_x = 210 iv_y = 443 iv_text = '2pt' ).

    lo_pdf->set_line_width( iv_width = 4 ).
    lo_pdf->line( iv_x1 = 50 iv_y1 = 460 iv_x2 = 200 iv_y2 = 460 ).
    lo_pdf->text( iv_x = 210 iv_y = 463 iv_text = '4pt' ).

    " Footer
    lo_pdf->set_font( iv_name = 'Courier' iv_size = 10 ).
    lo_pdf->set_text_color( iv_r = 150 iv_g = 150 iv_b = 150 ).
    lo_pdf->text( iv_x = 50 iv_y = 800 iv_text = 'Generated with open-abap-pdf | Page 2' ).

    " ========== Page 3: Fonts Demo ==========
    lo_pdf->add_page( ).

    " Header
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 24 ).
    lo_pdf->set_text_color( iv_r = 0 iv_g = 51 iv_b = 102 ).
    lo_pdf->text( iv_x = 50 iv_y = 50 iv_text = 'Fonts Demo' ).

    " Helvetica
    lo_pdf->set_text_color( iv_r = 0 iv_g = 0 iv_b = 0 ).
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 12 ).
    lo_pdf->text( iv_x = 50 iv_y = 100 iv_text = 'Helvetica 12pt - The quick brown fox jumps over the lazy dog' ).
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 16 ).
    lo_pdf->text( iv_x = 50 iv_y = 125 iv_text = 'Helvetica 16pt - The quick brown fox' ).
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 20 ).
    lo_pdf->text( iv_x = 50 iv_y = 155 iv_text = 'Helvetica 20pt - Hello World' ).

    " Times-Roman
    lo_pdf->set_font( iv_name = 'Times-Roman' iv_size = 12 ).
    lo_pdf->text( iv_x = 50 iv_y = 210 iv_text = 'Times-Roman 12pt - The quick brown fox jumps over the lazy dog' ).
    lo_pdf->set_font( iv_name = 'Times-Roman' iv_size = 16 ).
    lo_pdf->text( iv_x = 50 iv_y = 235 iv_text = 'Times-Roman 16pt - The quick brown fox' ).
    lo_pdf->set_font( iv_name = 'Times-Roman' iv_size = 20 ).
    lo_pdf->text( iv_x = 50 iv_y = 265 iv_text = 'Times-Roman 20pt - Hello World' ).

    " Courier
    lo_pdf->set_font( iv_name = 'Courier' iv_size = 12 ).
    lo_pdf->text( iv_x = 50 iv_y = 320 iv_text = 'Courier 12pt - The quick brown fox jumps' ).
    lo_pdf->set_font( iv_name = 'Courier' iv_size = 16 ).
    lo_pdf->text( iv_x = 50 iv_y = 345 iv_text = 'Courier 16pt - Fixed width font' ).
    lo_pdf->set_font( iv_name = 'Courier' iv_size = 20 ).
    lo_pdf->text( iv_x = 50 iv_y = 375 iv_text = 'Courier 20pt - Code Style' ).

    " Colors section
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 14 ).
    lo_pdf->set_text_color( iv_r = 0 iv_g = 0 iv_b = 0 ).
    lo_pdf->text( iv_x = 50 iv_y = 440 iv_text = 'Text Colors:' ).

    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 14 ).
    lo_pdf->set_text_color( iv_r = 255 iv_g = 0 iv_b = 0 ).
    lo_pdf->text( iv_x = 50 iv_y = 470 iv_text = 'Red Text' ).

    lo_pdf->set_text_color( iv_r = 0 iv_g = 128 iv_b = 0 ).
    lo_pdf->text( iv_x = 150 iv_y = 470 iv_text = 'Green Text' ).

    lo_pdf->set_text_color( iv_r = 0 iv_g = 0 iv_b = 255 ).
    lo_pdf->text( iv_x = 270 iv_y = 470 iv_text = 'Blue Text' ).

    lo_pdf->set_text_color( iv_r = 255 iv_g = 165 iv_b = 0 ).
    lo_pdf->text( iv_x = 380 iv_y = 470 iv_text = 'Orange Text' ).

    " Footer
    lo_pdf->set_font( iv_name = 'Courier' iv_size = 10 ).
    lo_pdf->set_text_color( iv_r = 150 iv_g = 150 iv_b = 150 ).
    lo_pdf->text( iv_x = 50 iv_y = 800 iv_text = 'Generated with open-abap-pdf | Page 3' ).

    ro_pdf = lo_pdf.
  ENDMETHOD.

  METHOD run_base64.
    DATA(lv_pdf) = run( ).
    rv_base64 = cl_http_utility=>encode_x_base64( lv_pdf ).
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
