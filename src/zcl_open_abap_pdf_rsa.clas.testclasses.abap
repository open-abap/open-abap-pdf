CLASS ltcl_rsa DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS mod_pow_small FOR TESTING RAISING cx_static_check.
    METHODS mod_pow_mersenne FOR TESTING RAISING cx_static_check.
    METHODS mod_pow_fermat FOR TESTING RAISING cx_static_check.
    METHODS mod_pow_zero_exponent FOR TESTING RAISING cx_static_check.
    METHODS even_modulus FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_rsa IMPLEMENTATION.

  METHOD mod_pow_small.
* 4^13 mod 497 = 445
    cl_abap_unit_assert=>assert_equals(
      act = zcl_open_abap_pdf_rsa=>mod_pow(
        iv_base     = '04'
        iv_exponent = '0D'
        iv_modulus  = '01F1' )
      exp = '01BD' ).
  ENDMETHOD.

  METHOD mod_pow_mersenne.
* 2^100 mod ( 2^89 - 1 ) = 2^11
    cl_abap_unit_assert=>assert_equals(
      act = zcl_open_abap_pdf_rsa=>mod_pow(
        iv_base     = '02'
        iv_exponent = '64'
        iv_modulus  = '01FFFFFFFFFFFFFFFFFFFFFF' )
      exp = '000000000000000000000800' ).
  ENDMETHOD.

  METHOD mod_pow_fermat.
* p = 2^127 - 1 is prime, so 3^(p-1) mod p = 1, and 3^p mod p = 3
    cl_abap_unit_assert=>assert_equals(
      act = zcl_open_abap_pdf_rsa=>mod_pow(
        iv_base     = '03'
        iv_exponent = '7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE'
        iv_modulus  = '7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' )
      exp = '00000000000000000000000000000001' ).
    cl_abap_unit_assert=>assert_equals(
      act = zcl_open_abap_pdf_rsa=>mod_pow(
        iv_base     = '03'
        iv_exponent = '7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF'
        iv_modulus  = '7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF' )
      exp = '00000000000000000000000000000003' ).
  ENDMETHOD.

  METHOD mod_pow_zero_exponent.
    cl_abap_unit_assert=>assert_equals(
      act = zcl_open_abap_pdf_rsa=>mod_pow(
        iv_base     = '1234'
        iv_exponent = '00'
        iv_modulus  = 'FFF1' )
      exp = '0001' ).
  ENDMETHOD.

  METHOD even_modulus.
    TRY.
        zcl_open_abap_pdf_rsa=>mod_pow(
          iv_base     = '02'
          iv_exponent = '03'
          iv_modulus  = '10' ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_open_abap_pdf ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

ENDCLASS.
