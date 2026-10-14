CLASS ltcl_asn1 DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS oid FOR TESTING RAISING cx_static_check.
    METHODS integer FOR TESTING RAISING cx_static_check.
    METHODS long_length FOR TESTING RAISING cx_static_check.
    METHODS set_of_sorted FOR TESTING RAISING cx_static_check.
    METHODS time FOR TESTING RAISING cx_static_check.
    METHODS decode FOR TESTING RAISING cx_static_check.
    METHODS decode_truncated FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_asn1 IMPLEMENTATION.

  METHOD oid.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_open_abap_pdf_asn1=>oid( '1.2.840.113549.1.1.1' )
      exp = '06092A864886F70D010101' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_open_abap_pdf_asn1=>oid( '2.16.840.1.101.3.4.2.1' )
      exp = '0609608648016503040201' ).
  ENDMETHOD.

  METHOD integer.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_open_abap_pdf_asn1=>integer_from_int( 1 )
      exp = '020101' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_open_abap_pdf_asn1=>integer_from_int( 0 )
      exp = '020100' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_open_abap_pdf_asn1=>integer_from_int( 128 )
      exp = '02020080' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_open_abap_pdf_asn1=>integer( '0000FF01' )
      exp = '020300FF01' ).
  ENDMETHOD.

  METHOD long_length.
    DATA lv_content TYPE xstring.
    DATA lv_der TYPE xstring.
    DATA lv_header TYPE xstring.
    DATA lv_byte TYPE x LENGTH 1 VALUE 'AB'.

    DO 300 TIMES.
      CONCATENATE lv_content lv_byte INTO lv_content IN BYTE MODE.
    ENDDO.
    lv_der = zcl_open_abap_pdf_asn1=>octet_string( lv_content ).
    lv_header = lv_der(4).

    cl_abap_unit_assert=>assert_equals(
      act = lv_header
      exp = '0482012C' ).
    cl_abap_unit_assert=>assert_equals(
      act = xstrlen( lv_der )
      exp = 304 ).
  ENDMETHOD.

  METHOD set_of_sorted.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_open_abap_pdf_asn1=>set_of( VALUE #( ( CONV xstring( '0402AAAA' ) ) ( CONV xstring( '020101' ) ) ) )
      exp = CONV xstring( '31070201010402AAAA' ) ).
  ENDMETHOD.

  METHOD time.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_open_abap_pdf_asn1=>time( '20261014120000' )
      exp = CONV xstring( '170D3236313031343132303030305A' ) ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_open_abap_pdf_asn1=>time( '20501014120000' )
      exp = CONV xstring( '180F32303530313031343132303030305A' ) ).
  ENDMETHOD.

  METHOD decode.
    DATA lt_nodes TYPE zcl_open_abap_pdf_asn1=>ty_nodes.

    lt_nodes = zcl_open_abap_pdf_asn1=>decode( '300502010105000500' ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_nodes )
      exp = 2 ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes[ 1 ]-tag
      exp = zcl_open_abap_pdf_asn1=>c_tag-sequence ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes[ 1 ]-content
      exp = '0201010500' ).
    cl_abap_unit_assert=>assert_equals(
      act = lt_nodes[ 2 ]-raw
      exp = '0500' ).

    lt_nodes = zcl_open_abap_pdf_asn1=>decode( lt_nodes[ 1 ]-content ).
    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_nodes )
      exp = 2 ).
  ENDMETHOD.

  METHOD decode_truncated.
    TRY.
        zcl_open_abap_pdf_asn1=>decode( '300602010105' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_open_abap_pdf ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
