"! Minimal ASN.1 DER encoder and decoder
CLASS zcl_open_abap_pdf_asn1 DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_node,
        tag     TYPE x LENGTH 1,
        content TYPE xstring,
        raw     TYPE xstring,
      END OF ty_node,
      ty_nodes    TYPE STANDARD TABLE OF ty_node WITH DEFAULT KEY,
      ty_xstrings TYPE STANDARD TABLE OF xstring WITH DEFAULT KEY.

    CONSTANTS:
      BEGIN OF c_tag,
        integer      TYPE x LENGTH 1 VALUE '02',
        bit_string   TYPE x LENGTH 1 VALUE '03',
        octet_string TYPE x LENGTH 1 VALUE '04',
        null         TYPE x LENGTH 1 VALUE '05',
        oid          TYPE x LENGTH 1 VALUE '06',
        utc_time     TYPE x LENGTH 1 VALUE '17',
        general_time TYPE x LENGTH 1 VALUE '18',
        sequence     TYPE x LENGTH 1 VALUE '30',
        set          TYPE x LENGTH 1 VALUE '31',
        context_0    TYPE x LENGTH 1 VALUE 'A0',
      END OF c_tag.

    "! Encode tag, length and content
    CLASS-METHODS encode
      IMPORTING iv_tag        TYPE x
                iv_content    TYPE xstring
      RETURNING VALUE(rv_der) TYPE xstring.

    CLASS-METHODS sequence
      IMPORTING iv_content    TYPE xstring
      RETURNING VALUE(rv_der) TYPE xstring.

    "! Concatenate byte strings
    CLASS-METHODS concat
      IMPORTING it_parts        TYPE ty_xstrings
      RETURNING VALUE(rv_bytes) TYPE xstring.

    "! Encode SET OF, elements are sorted as required by DER
    CLASS-METHODS set_of
      IMPORTING it_elements   TYPE ty_xstrings
                iv_tag        TYPE x DEFAULT '31'
      RETURNING VALUE(rv_der) TYPE xstring.

    "! Encode an OBJECT IDENTIFIER given in dotted notation
    CLASS-METHODS oid
      IMPORTING iv_oid        TYPE string
      RETURNING VALUE(rv_der) TYPE xstring.

    "! Encode INTEGER from unsigned big-endian bytes
    CLASS-METHODS integer
      IMPORTING iv_value      TYPE xstring
      RETURNING VALUE(rv_der) TYPE xstring.

    "! Encode small non-negative INTEGER
    CLASS-METHODS integer_from_int
      IMPORTING iv_value      TYPE i
      RETURNING VALUE(rv_der) TYPE xstring.

    CLASS-METHODS octet_string
      IMPORTING iv_value      TYPE xstring
      RETURNING VALUE(rv_der) TYPE xstring.

    CLASS-METHODS null
      RETURNING VALUE(rv_der) TYPE xstring.

    "! Encode UTCTime or GeneralizedTime (from year 2050)
    "! @parameter iv_time | UTC time, format YYYYMMDDhhmmss
    CLASS-METHODS time
      IMPORTING iv_time       TYPE string
      RETURNING VALUE(rv_der) TYPE xstring.

    "! Decode all top level elements
    CLASS-METHODS decode
      IMPORTING iv_der          TYPE xstring
      RETURNING VALUE(rt_nodes) TYPE ty_nodes
      RAISING   zcx_open_abap_pdf.

    "! Unsigned big-endian bytes of INTEGER content, ie. leading zeros removed
    CLASS-METHODS to_unsigned
      IMPORTING iv_content      TYPE xstring
      RETURNING VALUE(rv_value) TYPE xstring.

    "! DER ordering, shorter value padded with trailing zeros, returns -1, 0 or 1
    CLASS-METHODS compare
      IMPORTING iv_a             TYPE xstring
                iv_b             TYPE xstring
      RETURNING VALUE(rv_result) TYPE i.

  PRIVATE SECTION.
    CLASS-METHODS encode_length
      IMPORTING iv_length       TYPE i
      RETURNING VALUE(rv_bytes) TYPE xstring.

    CLASS-METHODS base128
      IMPORTING iv_value        TYPE i
      RETURNING VALUE(rv_bytes) TYPE xstring.
ENDCLASS.

CLASS zcl_open_abap_pdf_asn1 IMPLEMENTATION.

  METHOD encode.
    DATA lv_length TYPE xstring.
    lv_length = encode_length( xstrlen( iv_content ) ).
    CONCATENATE iv_tag lv_length iv_content INTO rv_der IN BYTE MODE.
  ENDMETHOD.

  METHOD sequence.
    rv_der = encode( iv_tag = c_tag-sequence iv_content = iv_content ).
  ENDMETHOD.

  METHOD concat.
    DATA lv_part TYPE xstring.
    LOOP AT it_parts INTO lv_part.
      CONCATENATE rv_bytes lv_part INTO rv_bytes IN BYTE MODE.
    ENDLOOP.
  ENDMETHOD.

  METHOD set_of.
    DATA lt_sorted TYPE ty_xstrings.
    DATA lv_element TYPE xstring.
    DATA lv_sorted TYPE xstring.
    DATA lv_index TYPE i.
    DATA lv_content TYPE xstring.

* insertion sort, sets are small
    LOOP AT it_elements INTO lv_element.
      lv_index = 0.
      LOOP AT lt_sorted INTO lv_sorted.
        IF compare( iv_a = lv_element iv_b = lv_sorted ) < 0.
          lv_index = sy-tabix.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF lv_index = 0.
        APPEND lv_element TO lt_sorted.
      ELSE.
        INSERT lv_element INTO lt_sorted INDEX lv_index.
      ENDIF.
    ENDLOOP.

    LOOP AT lt_sorted INTO lv_sorted.
      CONCATENATE lv_content lv_sorted INTO lv_content IN BYTE MODE.
    ENDLOOP.

    rv_der = encode( iv_tag = iv_tag iv_content = lv_content ).
  ENDMETHOD.

  METHOD oid.
    DATA lt_parts TYPE string_table.
    DATA lv_part TYPE string.
    DATA lv_first TYPE i.
    DATA lv_value TYPE i.
    DATA lv_bytes TYPE xstring.
    DATA lv_content TYPE xstring.

    SPLIT iv_oid AT '.' INTO TABLE lt_parts.
    LOOP AT lt_parts INTO lv_part.
      lv_value = lv_part.
      CASE sy-tabix.
        WHEN 1.
          lv_first = lv_value * 40.
        WHEN 2.
          lv_bytes = base128( lv_first + lv_value ).
          CONCATENATE lv_content lv_bytes INTO lv_content IN BYTE MODE.
        WHEN OTHERS.
          lv_bytes = base128( lv_value ).
          CONCATENATE lv_content lv_bytes INTO lv_content IN BYTE MODE.
      ENDCASE.
    ENDLOOP.

    rv_der = encode( iv_tag = c_tag-oid iv_content = lv_content ).
  ENDMETHOD.

  METHOD integer.
    DATA lv_value TYPE xstring.
    DATA lv_first TYPE x LENGTH 1.
    DATA lv_zero TYPE x LENGTH 1 VALUE '00'.

    lv_value = to_unsigned( iv_value ).
    IF xstrlen( lv_value ) = 0.
      lv_value = lv_zero.
    ELSE.
      lv_first = lv_value(1).
      IF lv_first >= '80'.
        CONCATENATE lv_zero lv_value INTO lv_value IN BYTE MODE.
      ENDIF.
    ENDIF.

    rv_der = encode( iv_tag = c_tag-integer iv_content = lv_value ).
  ENDMETHOD.

  METHOD integer_from_int.
    DATA lv_byte TYPE x LENGTH 1.
    DATA lv_value TYPE i.
    DATA lv_bytes TYPE xstring.

    lv_value = iv_value.
    DO.
      lv_byte = lv_value MOD 256.
      CONCATENATE lv_byte lv_bytes INTO lv_bytes IN BYTE MODE.
      lv_value = lv_value DIV 256.
      IF lv_value = 0.
        EXIT.
      ENDIF.
    ENDDO.

    rv_der = integer( lv_bytes ).
  ENDMETHOD.

  METHOD octet_string.
    rv_der = encode( iv_tag = c_tag-octet_string iv_content = iv_value ).
  ENDMETHOD.

  METHOD null.
    DATA lv_empty TYPE xstring.
    rv_der = encode( iv_tag = c_tag-null iv_content = lv_empty ).
  ENDMETHOD.

  METHOD time.
    DATA lv_year TYPE i.

    lv_year = iv_time(4).
    IF lv_year >= 1950 AND lv_year < 2050.
      rv_der = encode(
        iv_tag     = c_tag-utc_time
        iv_content = cl_abap_codepage=>convert_to( iv_time+2(12) && 'Z' ) ).
    ELSE.
      rv_der = encode(
        iv_tag     = c_tag-general_time
        iv_content = cl_abap_codepage=>convert_to( iv_time(14) && 'Z' ) ).
    ENDIF.
  ENDMETHOD.

  METHOD decode.
    DATA ls_node TYPE ty_node.
    DATA lv_pos TYPE i.
    DATA lv_start TYPE i.
    DATA lv_total TYPE i.
    DATA lv_byte TYPE x LENGTH 1.
    DATA lv_length TYPE i.
    DATA lv_count TYPE i.
    DATA lv_int TYPE i.

    lv_total = xstrlen( iv_der ).
    WHILE lv_pos < lv_total.
      CLEAR ls_node.
      lv_start = lv_pos.
      IF lv_pos + 2 > lv_total.
        zcx_open_abap_pdf=>raise( 'ASN.1: unexpected end of data' ).
      ENDIF.

      ls_node-tag = iv_der+lv_pos(1).
      lv_byte = ls_node-tag.
      lv_int = lv_byte.
      IF lv_int MOD 32 = 31.
        zcx_open_abap_pdf=>raise( 'ASN.1: multi byte tags not supported' ).
      ENDIF.
      lv_pos = lv_pos + 1.

      lv_byte = iv_der+lv_pos(1).
      lv_length = lv_byte.
      lv_pos = lv_pos + 1.
      IF lv_length = 128.
        zcx_open_abap_pdf=>raise( 'ASN.1: indefinite length not supported' ).
      ELSEIF lv_length > 128.
        lv_count = lv_length - 128.
        IF lv_count > 3 OR lv_pos + lv_count > lv_total.
          zcx_open_abap_pdf=>raise( 'ASN.1: invalid length' ).
        ENDIF.
        lv_length = 0.
        DO lv_count TIMES.
          lv_byte = iv_der+lv_pos(1).
          lv_int = lv_byte.
          lv_length = lv_length * 256 + lv_int.
          lv_pos = lv_pos + 1.
        ENDDO.
      ENDIF.

      IF lv_pos + lv_length > lv_total.
        zcx_open_abap_pdf=>raise( 'ASN.1: length exceeds data' ).
      ENDIF.

      ls_node-content = iv_der+lv_pos(lv_length).
      lv_pos = lv_pos + lv_length.
      lv_length = lv_pos - lv_start.
      ls_node-raw = iv_der+lv_start(lv_length).
      APPEND ls_node TO rt_nodes.
    ENDWHILE.
  ENDMETHOD.

  METHOD to_unsigned.
    DATA lv_length TYPE i.
    DATA lv_offset TYPE i.

    lv_length = xstrlen( iv_content ).
    WHILE lv_offset < lv_length AND iv_content+lv_offset(1) = '00'.
      lv_offset = lv_offset + 1.
    ENDWHILE.
    rv_value = iv_content+lv_offset.
  ENDMETHOD.

  METHOD compare.
    DATA lv_length_a TYPE i.
    DATA lv_length_b TYPE i.
    DATA lv_max TYPE i.
    DATA lv_pos TYPE i.
    DATA lv_byte_a TYPE x LENGTH 1.
    DATA lv_byte_b TYPE x LENGTH 1.

    lv_length_a = xstrlen( iv_a ).
    lv_length_b = xstrlen( iv_b ).
    lv_max = nmax( val1 = lv_length_a val2 = lv_length_b ).

    WHILE lv_pos < lv_max.
      CLEAR lv_byte_a.
      CLEAR lv_byte_b.
      IF lv_pos < lv_length_a.
        lv_byte_a = iv_a+lv_pos(1).
      ENDIF.
      IF lv_pos < lv_length_b.
        lv_byte_b = iv_b+lv_pos(1).
      ENDIF.
      IF lv_byte_a < lv_byte_b.
        rv_result = -1.
        RETURN.
      ELSEIF lv_byte_a > lv_byte_b.
        rv_result = 1.
        RETURN.
      ENDIF.
      lv_pos = lv_pos + 1.
    ENDWHILE.

    rv_result = 0.
  ENDMETHOD.

  METHOD encode_length.
    DATA lv_byte TYPE x LENGTH 1.
    DATA lv_length TYPE i.
    DATA lv_count TYPE i.

    IF iv_length < 128.
      lv_byte = iv_length.
      rv_bytes = lv_byte.
      RETURN.
    ENDIF.

    lv_length = iv_length.
    WHILE lv_length > 0.
      lv_byte = lv_length MOD 256.
      CONCATENATE lv_byte rv_bytes INTO rv_bytes IN BYTE MODE.
      lv_length = lv_length DIV 256.
      lv_count = lv_count + 1.
    ENDWHILE.

    lv_byte = 128 + lv_count.
    CONCATENATE lv_byte rv_bytes INTO rv_bytes IN BYTE MODE.
  ENDMETHOD.

  METHOD base128.
    DATA lv_byte TYPE x LENGTH 1.
    DATA lv_value TYPE i.
    DATA lv_more TYPE abap_bool.

    lv_value = iv_value.
    DO.
      IF lv_more = abap_true.
        lv_byte = lv_value MOD 128 + 128.
      ELSE.
        lv_byte = lv_value MOD 128.
      ENDIF.
      CONCATENATE lv_byte rv_bytes INTO rv_bytes IN BYTE MODE.
      lv_more = abap_true.
      lv_value = lv_value DIV 128.
      IF lv_value = 0.
        EXIT.
      ENDIF.
    ENDDO.
  ENDMETHOD.

ENDCLASS.
