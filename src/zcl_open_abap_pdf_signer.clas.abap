"! Pure ABAP signer, RSA private key and X.509 certificate, creates CMS / PKCS#7 detached
"! SignedData with SHA-256, as used by /SubFilter /adbe.pkcs7.detached
CLASS zcl_open_abap_pdf_signer DEFINITION PUBLIC CREATE PRIVATE.
  PUBLIC SECTION.
    INTERFACES zif_open_abap_pdf_signer.

    TYPES:
      ty_certificates TYPE STANDARD TABLE OF xstring WITH DEFAULT KEY,
      BEGIN OF ty_pem,
        label TYPE string,
        der   TYPE xstring,
      END OF ty_pem,
      ty_pems TYPE STANDARD TABLE OF ty_pem WITH DEFAULT KEY.

    "! @parameter iv_private_key | DER, PKCS#1 RSAPrivateKey or unencrypted PKCS#8 PrivateKeyInfo
    "! @parameter iv_certificate | DER, X.509 certificate matching the private key
    "! @parameter it_chain | DER, X.509 intermediate certificates, optional
    CLASS-METHODS create
      IMPORTING iv_private_key   TYPE xstring
                iv_certificate   TYPE xstring
                it_chain         TYPE ty_certificates OPTIONAL
      RETURNING VALUE(ro_signer) TYPE REF TO zcl_open_abap_pdf_signer
      RAISING   zcx_open_abap_pdf.

    "! @parameter iv_private_key | PEM, "RSA PRIVATE KEY" or unencrypted "PRIVATE KEY"
    "! @parameter iv_certificates | PEM, signing certificate first followed by optional chain
    CLASS-METHODS create_from_pem
      IMPORTING iv_private_key   TYPE string
                iv_certificates  TYPE string
      RETURNING VALUE(ro_signer) TYPE REF TO zcl_open_abap_pdf_signer
      RAISING   zcx_open_abap_pdf.

    "! Decode all PEM blocks
    CLASS-METHODS pem_decode
      IMPORTING iv_pem         TYPE string
      RETURNING VALUE(rt_pems) TYPE ty_pems
      RAISING   zcx_open_abap_pdf.

    CLASS-METHODS base64_decode
      IMPORTING iv_base64       TYPE string
      RETURNING VALUE(rv_bytes) TYPE xstring
      RAISING   zcx_open_abap_pdf.

    CLASS-METHODS sha256
      IMPORTING iv_data        TYPE xstring
      RETURNING VALUE(rv_hash) TYPE xstring
      RAISING   zcx_open_abap_pdf.

  PRIVATE SECTION.
    CONSTANTS:
      BEGIN OF c_oid,
        rsa_encryption       TYPE string VALUE '1.2.840.113549.1.1.1',
        sha256               TYPE string VALUE '2.16.840.1.101.3.4.2.1',
        data                 TYPE string VALUE '1.2.840.113549.1.7.1',
        signed_data          TYPE string VALUE '1.2.840.113549.1.7.2',
        content_type         TYPE string VALUE '1.2.840.113549.1.9.3',
        message_digest       TYPE string VALUE '1.2.840.113549.1.9.4',
        signing_time         TYPE string VALUE '1.2.840.113549.1.9.5',
        signing_certificate2 TYPE string VALUE '1.2.840.113549.1.9.16.2.47',
      END OF c_oid.

    DATA mo_rsa TYPE REF TO zcl_open_abap_pdf_rsa.
    DATA mv_certificate TYPE xstring.
    DATA mt_chain TYPE ty_certificates.
    DATA mv_issuer TYPE xstring.
    DATA mv_serial TYPE xstring.

    METHODS parse_private_key
      IMPORTING iv_der            TYPE xstring
      RETURNING VALUE(rv_modulus) TYPE xstring
      RAISING   zcx_open_abap_pdf.

    METHODS parse_certificate
      IMPORTING iv_der            TYPE xstring
      RETURNING VALUE(rv_modulus) TYPE xstring
      RAISING   zcx_open_abap_pdf.

    CLASS-METHODS decode_single
      IMPORTING iv_der         TYPE xstring
                iv_tag         TYPE x
      RETURNING VALUE(rs_node) TYPE zcl_open_abap_pdf_asn1=>ty_node
      RAISING   zcx_open_abap_pdf.

    CLASS-METHODS attribute
      IMPORTING iv_oid        TYPE string
                iv_value      TYPE xstring
      RETURNING VALUE(rv_der) TYPE xstring.

    CLASS-METHODS algorithm
      IMPORTING iv_oid        TYPE string
      RETURNING VALUE(rv_der) TYPE xstring.
ENDCLASS.

CLASS zcl_open_abap_pdf_signer IMPLEMENTATION.

  METHOD create.
    DATA lv_key_modulus TYPE xstring.
    DATA lv_cert_modulus TYPE xstring.

    CREATE OBJECT ro_signer.
    lv_key_modulus = ro_signer->parse_private_key( iv_private_key ).
    lv_cert_modulus = ro_signer->parse_certificate( iv_certificate ).
    IF lv_key_modulus <> lv_cert_modulus.
      zcx_open_abap_pdf=>raise( 'Signer: certificate does not match private key' ).
    ENDIF.
    ro_signer->mv_certificate = iv_certificate.
    ro_signer->mt_chain = it_chain.
  ENDMETHOD.

  METHOD create_from_pem.
    DATA lt_pems TYPE ty_pems.
    DATA ls_pem TYPE ty_pem.
    DATA lv_key TYPE xstring.
    DATA lv_certificate TYPE xstring.
    DATA lt_chain TYPE ty_certificates.

    lt_pems = pem_decode( iv_private_key ).
    LOOP AT lt_pems INTO ls_pem.
      CASE ls_pem-label.
        WHEN 'RSA PRIVATE KEY' OR 'PRIVATE KEY'.
          lv_key = ls_pem-der.
          EXIT.
        WHEN 'ENCRYPTED PRIVATE KEY'.
          zcx_open_abap_pdf=>raise( 'Signer: encrypted private keys not supported' ).
        WHEN OTHERS.
          CONTINUE.
      ENDCASE.
    ENDLOOP.
    IF lv_key IS INITIAL.
      zcx_open_abap_pdf=>raise( 'Signer: no private key found in PEM' ).
    ENDIF.

    lt_pems = pem_decode( iv_certificates ).
    LOOP AT lt_pems INTO ls_pem WHERE label = 'CERTIFICATE'.
      IF lv_certificate IS INITIAL.
        lv_certificate = ls_pem-der.
      ELSE.
        APPEND ls_pem-der TO lt_chain.
      ENDIF.
    ENDLOOP.
    IF lv_certificate IS INITIAL.
      zcx_open_abap_pdf=>raise( 'Signer: no certificate found in PEM' ).
    ENDIF.

    ro_signer = create(
      iv_private_key = lv_key
      iv_certificate = lv_certificate
      it_chain       = lt_chain ).
  ENDMETHOD.

  METHOD pem_decode.
    CONSTANTS lc_begin TYPE string VALUE `-----BEGIN `.
    CONSTANTS lc_dashes TYPE string VALUE '-----'.
    DATA ls_pem TYPE ty_pem.
    DATA lv_offset TYPE i.
    DATA lv_label_end TYPE i.
    DATA lv_end TYPE i.
    DATA lv_body TYPE i.

    DO.
      lv_offset = find( val = iv_pem sub = lc_begin off = lv_offset ).
      IF lv_offset < 0.
        EXIT.
      ENDIF.
      lv_offset = lv_offset + strlen( lc_begin ).
      lv_label_end = find( val = iv_pem sub = lc_dashes off = lv_offset ).
      IF lv_label_end < 0.
        zcx_open_abap_pdf=>raise( 'PEM: malformed header' ).
      ENDIF.
      CLEAR ls_pem.
      ls_pem-label = substring( val = iv_pem off = lv_offset len = lv_label_end - lv_offset ).
      lv_body = lv_label_end + strlen( lc_dashes ).
      lv_end = find( val = iv_pem sub = |-----END { ls_pem-label }-----| off = lv_body ).
      IF lv_end < 0.
        zcx_open_abap_pdf=>raise( |PEM: missing END { ls_pem-label }| ).
      ENDIF.
      ls_pem-der = base64_decode( substring( val = iv_pem off = lv_body len = lv_end - lv_body ) ).
      APPEND ls_pem TO rt_pems.
      lv_offset = lv_end + 1.
    ENDDO.
  ENDMETHOD.

  METHOD base64_decode.
    CONSTANTS lc_alphabet TYPE string VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'.
    DATA lv_offset TYPE i.
    DATA lv_length TYPE i.
    DATA lv_char TYPE string.
    DATA lv_value TYPE i.
    DATA lv_acc TYPE i.
    DATA lv_bits TYPE i.
    DATA lv_divisor TYPE i.
    DATA lv_byte TYPE x LENGTH 1.

    lv_length = strlen( iv_base64 ).
    WHILE lv_offset < lv_length.
      lv_char = iv_base64+lv_offset(1).
      lv_offset = lv_offset + 1.
      IF lv_char = '='.
        EXIT.
      ENDIF.
      lv_value = find( val = lc_alphabet sub = lv_char ).
      IF lv_value < 0.
* skip line breaks and whitespace
        CONTINUE.
      ENDIF.
      lv_acc = lv_acc * 64 + lv_value.
      lv_bits = lv_bits + 6.
      IF lv_bits >= 8.
        lv_bits = lv_bits - 8.
        lv_divisor = ipow( base = 2 exp = lv_bits ).
        lv_byte = lv_acc DIV lv_divisor.
        CONCATENATE rv_bytes lv_byte INTO rv_bytes IN BYTE MODE.
        lv_acc = lv_acc MOD lv_divisor.
      ENDIF.
    ENDWHILE.
  ENDMETHOD.

  METHOD sha256.
    DATA lx_digest TYPE REF TO cx_abap_message_digest.
    TRY.
        cl_abap_message_digest=>calculate_hash_for_raw(
          EXPORTING
            if_algorithm   = 'SHA256'
            if_data        = iv_data
          IMPORTING
            ef_hashxstring = rv_hash ).
      CATCH cx_abap_message_digest INTO lx_digest.
        RAISE EXCEPTION TYPE zcx_open_abap_pdf
          EXPORTING
            previous = lx_digest
            iv_text  = 'SHA-256 calculation failed'.
    ENDTRY.
  ENDMETHOD.

  METHOD zif_open_abap_pdf_signer~sign.
    DATA lt_attributes TYPE zcl_open_abap_pdf_asn1=>ty_xstrings.
    DATA lt_certificates TYPE zcl_open_abap_pdf_asn1=>ty_xstrings.
    DATA lv_signature TYPE xstring.
    DATA lv_signer_info TYPE xstring.
    DATA lv_signed_data TYPE xstring.

* signed attributes
    APPEND attribute(
      iv_oid   = c_oid-content_type
      iv_value = zcl_open_abap_pdf_asn1=>oid( c_oid-data ) ) TO lt_attributes.
    APPEND attribute(
      iv_oid   = c_oid-signing_time
      iv_value = zcl_open_abap_pdf_asn1=>time( iv_signing_time ) ) TO lt_attributes.
    APPEND attribute(
      iv_oid   = c_oid-message_digest
      iv_value = zcl_open_abap_pdf_asn1=>octet_string( sha256( iv_data ) ) ) TO lt_attributes.
* SigningCertificateV2, ESSCertIDv2 with default hash algorithm SHA-256
    APPEND attribute(
      iv_oid   = c_oid-signing_certificate2
      iv_value = zcl_open_abap_pdf_asn1=>sequence(
        zcl_open_abap_pdf_asn1=>sequence(
        zcl_open_abap_pdf_asn1=>sequence(
        zcl_open_abap_pdf_asn1=>octet_string( sha256( mv_certificate ) ) ) ) ) ) TO lt_attributes.

* the signature is calculated over the DER encoding with SET OF tag
    lv_signature = mo_rsa->sign_sha256( sha256( zcl_open_abap_pdf_asn1=>set_of( lt_attributes ) ) ).

    lv_signer_info = zcl_open_abap_pdf_asn1=>sequence( zcl_open_abap_pdf_asn1=>concat( VALUE #(
      ( zcl_open_abap_pdf_asn1=>integer_from_int( 1 ) )
      ( zcl_open_abap_pdf_asn1=>sequence( zcl_open_abap_pdf_asn1=>concat( VALUE #( ( mv_issuer ) ( mv_serial ) ) ) ) )
      ( algorithm( c_oid-sha256 ) )
      ( zcl_open_abap_pdf_asn1=>set_of( it_elements = lt_attributes iv_tag = zcl_open_abap_pdf_asn1=>c_tag-context_0 ) )
      ( algorithm( c_oid-rsa_encryption ) )
      ( zcl_open_abap_pdf_asn1=>octet_string( lv_signature ) ) ) ) ).

    APPEND mv_certificate TO lt_certificates.
    APPEND LINES OF mt_chain TO lt_certificates.

    lv_signed_data = zcl_open_abap_pdf_asn1=>sequence( zcl_open_abap_pdf_asn1=>concat( VALUE #(
      ( zcl_open_abap_pdf_asn1=>integer_from_int( 1 ) )
      ( zcl_open_abap_pdf_asn1=>set_of( VALUE #( ( algorithm( c_oid-sha256 ) ) ) ) )
      ( zcl_open_abap_pdf_asn1=>sequence( zcl_open_abap_pdf_asn1=>oid( c_oid-data ) ) )
      ( zcl_open_abap_pdf_asn1=>encode(
          iv_tag     = zcl_open_abap_pdf_asn1=>c_tag-context_0
          iv_content = zcl_open_abap_pdf_asn1=>concat( lt_certificates ) ) )
      ( zcl_open_abap_pdf_asn1=>set_of( VALUE #( ( lv_signer_info ) ) ) ) ) ) ).

    rv_cms = zcl_open_abap_pdf_asn1=>sequence( zcl_open_abap_pdf_asn1=>concat( VALUE #(
      ( zcl_open_abap_pdf_asn1=>oid( c_oid-signed_data ) )
      ( zcl_open_abap_pdf_asn1=>encode(
          iv_tag     = zcl_open_abap_pdf_asn1=>c_tag-context_0
          iv_content = lv_signed_data ) ) ) ) ).
  ENDMETHOD.

  METHOD parse_private_key.
    DATA ls_top TYPE zcl_open_abap_pdf_asn1=>ty_node.
    DATA lt_fields TYPE zcl_open_abap_pdf_asn1=>ty_nodes.
    DATA ls_field TYPE zcl_open_abap_pdf_asn1=>ty_node.
    DATA lt_values TYPE zcl_open_abap_pdf_asn1=>ty_xstrings.
    DATA lt_algorithm TYPE zcl_open_abap_pdf_asn1=>ty_nodes.
    DATA ls_algorithm TYPE zcl_open_abap_pdf_asn1=>ty_node.

    ls_top = decode_single( iv_der = iv_der iv_tag = zcl_open_abap_pdf_asn1=>c_tag-sequence ).
    lt_fields = zcl_open_abap_pdf_asn1=>decode( ls_top-content ).

    READ TABLE lt_fields INDEX 2 INTO ls_field.
    IF sy-subrc = 0 AND ls_field-tag = zcl_open_abap_pdf_asn1=>c_tag-sequence.
* PKCS#8 PrivateKeyInfo, version, AlgorithmIdentifier, OCTET STRING with PKCS#1 key
      lt_algorithm = zcl_open_abap_pdf_asn1=>decode( ls_field-content ).
      READ TABLE lt_algorithm INDEX 1 INTO ls_algorithm.
      IF sy-subrc <> 0 OR ls_algorithm-raw <> zcl_open_abap_pdf_asn1=>oid( c_oid-rsa_encryption ).
        zcx_open_abap_pdf=>raise( 'Signer: only RSA private keys supported' ).
      ENDIF.
      READ TABLE lt_fields INDEX 3 INTO ls_field.
      IF sy-subrc <> 0 OR ls_field-tag <> zcl_open_abap_pdf_asn1=>c_tag-octet_string.
        zcx_open_abap_pdf=>raise( 'Signer: malformed PKCS#8 private key' ).
      ENDIF.
      ls_top = decode_single( iv_der = ls_field-content iv_tag = zcl_open_abap_pdf_asn1=>c_tag-sequence ).
      lt_fields = zcl_open_abap_pdf_asn1=>decode( ls_top-content ).
    ENDIF.

* PKCS#1 RSAPrivateKey, version n e d p q dp dq qinv
    LOOP AT lt_fields INTO ls_field.
      IF ls_field-tag <> zcl_open_abap_pdf_asn1=>c_tag-integer.
        zcx_open_abap_pdf=>raise( 'Signer: malformed RSA private key' ).
      ENDIF.
      APPEND zcl_open_abap_pdf_asn1=>to_unsigned( ls_field-content ) TO lt_values.
    ENDLOOP.
    IF lines( lt_values ) <> 9.
      zcx_open_abap_pdf=>raise( 'Signer: malformed RSA private key, multi-prime not supported' ).
    ENDIF.

    rv_modulus = lt_values[ 2 ].
    mo_rsa = zcl_open_abap_pdf_rsa=>create(
      iv_modulus         = lt_values[ 2 ]
      iv_public_exponent = lt_values[ 3 ]
      iv_prime1          = lt_values[ 5 ]
      iv_prime2          = lt_values[ 6 ]
      iv_exponent1       = lt_values[ 7 ]
      iv_exponent2       = lt_values[ 8 ]
      iv_coefficient     = lt_values[ 9 ] ).
  ENDMETHOD.

  METHOD parse_certificate.
    DATA ls_top TYPE zcl_open_abap_pdf_asn1=>ty_node.
    DATA ls_tbs TYPE zcl_open_abap_pdf_asn1=>ty_node.
    DATA lt_fields TYPE zcl_open_abap_pdf_asn1=>ty_nodes.
    DATA ls_field TYPE zcl_open_abap_pdf_asn1=>ty_node.
    DATA lt_children TYPE zcl_open_abap_pdf_asn1=>ty_nodes.
    DATA ls_child TYPE zcl_open_abap_pdf_asn1=>ty_node.
    DATA lv_bits TYPE xstring.

    ls_top = decode_single( iv_der = iv_der iv_tag = zcl_open_abap_pdf_asn1=>c_tag-sequence ).
    lt_fields = zcl_open_abap_pdf_asn1=>decode( ls_top-content ).
    READ TABLE lt_fields INDEX 1 INTO ls_tbs.
    IF sy-subrc <> 0 OR ls_tbs-tag <> zcl_open_abap_pdf_asn1=>c_tag-sequence.
      zcx_open_abap_pdf=>raise( 'Signer: malformed certificate' ).
    ENDIF.

* TBSCertificate, [0] version, serialNumber, signature, issuer, validity, subject, subjectPublicKeyInfo
    lt_fields = zcl_open_abap_pdf_asn1=>decode( ls_tbs-content ).
    READ TABLE lt_fields INDEX 1 INTO ls_field.
    IF sy-subrc = 0 AND ls_field-tag = zcl_open_abap_pdf_asn1=>c_tag-context_0.
      DELETE lt_fields INDEX 1.
    ENDIF.
    IF lines( lt_fields ) < 6.
      zcx_open_abap_pdf=>raise( 'Signer: malformed certificate' ).
    ENDIF.

    mv_serial = lt_fields[ 1 ]-raw.
    mv_issuer = lt_fields[ 3 ]-raw.

* SubjectPublicKeyInfo, AlgorithmIdentifier and BIT STRING with RSAPublicKey
    lt_children = zcl_open_abap_pdf_asn1=>decode( lt_fields[ 6 ]-content ).
    READ TABLE lt_children INDEX 2 INTO ls_child.
    IF sy-subrc <> 0 OR ls_child-tag <> zcl_open_abap_pdf_asn1=>c_tag-bit_string OR xstrlen( ls_child-content ) < 2.
      zcx_open_abap_pdf=>raise( 'Signer: only RSA certificates supported' ).
    ENDIF.
    lv_bits = ls_child-content+1.
    ls_child = decode_single( iv_der = lv_bits iv_tag = zcl_open_abap_pdf_asn1=>c_tag-sequence ).
    lt_children = zcl_open_abap_pdf_asn1=>decode( ls_child-content ).
    READ TABLE lt_children INDEX 1 INTO ls_child.
    IF sy-subrc <> 0 OR ls_child-tag <> zcl_open_abap_pdf_asn1=>c_tag-integer.
      zcx_open_abap_pdf=>raise( 'Signer: only RSA certificates supported' ).
    ENDIF.
    rv_modulus = zcl_open_abap_pdf_asn1=>to_unsigned( ls_child-content ).
  ENDMETHOD.

  METHOD decode_single.
    DATA lt_nodes TYPE zcl_open_abap_pdf_asn1=>ty_nodes.

    lt_nodes = zcl_open_abap_pdf_asn1=>decode( iv_der ).
    IF lines( lt_nodes ) <> 1.
      zcx_open_abap_pdf=>raise( 'ASN.1: expected single element' ).
    ENDIF.
    READ TABLE lt_nodes INDEX 1 INTO rs_node.
    IF rs_node-tag <> iv_tag.
      zcx_open_abap_pdf=>raise( 'ASN.1: unexpected tag' ).
    ENDIF.
  ENDMETHOD.

  METHOD attribute.
    rv_der = zcl_open_abap_pdf_asn1=>sequence( zcl_open_abap_pdf_asn1=>concat( VALUE #(
      ( zcl_open_abap_pdf_asn1=>oid( iv_oid ) )
      ( zcl_open_abap_pdf_asn1=>set_of( VALUE #( ( iv_value ) ) ) ) ) ) ).
  ENDMETHOD.

  METHOD algorithm.
    rv_der = zcl_open_abap_pdf_asn1=>sequence( zcl_open_abap_pdf_asn1=>concat( VALUE #(
      ( zcl_open_abap_pdf_asn1=>oid( iv_oid ) )
      ( zcl_open_abap_pdf_asn1=>null( ) ) ) ) ).
  ENDMETHOD.

ENDCLASS.
