CLASS ltcl_fake_signer DEFINITION FOR TESTING.
  PUBLIC SECTION.
    INTERFACES zif_open_abap_pdf_signer.
    DATA mv_data TYPE xstring.
    DATA mv_signing_time TYPE string.
ENDCLASS.

CLASS ltcl_fake_signer IMPLEMENTATION.
  METHOD zif_open_abap_pdf_signer~sign.
    mv_data = iv_data.
    mv_signing_time = iv_signing_time.
    rv_cms = 'DEADBEEF'.
  ENDMETHOD.
ENDCLASS.

CLASS ltcl_pdf_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS test_create FOR TESTING RAISING cx_static_check.
    METHODS test_add_page FOR TESTING RAISING cx_static_check.
    METHODS test_render_empty FOR TESTING RAISING cx_static_check.
    METHODS test_text FOR TESTING RAISING cx_static_check.
    METHODS test_multiple_pages FOR TESTING RAISING cx_static_check.
    METHODS test_shapes FOR TESTING RAISING cx_static_check.
    METHODS test_mm_to_pt FOR TESTING RAISING cx_static_check.
    METHODS test_inch_to_pt FOR TESTING RAISING cx_static_check.
    METHODS test_fluent_api FOR TESTING RAISING cx_static_check.
    METHODS test_xref_offsets FOR TESTING RAISING cx_static_check.
    METHODS test_signed_byte_range FOR TESTING RAISING cx_static_check.
    METHODS test_signed_no_page FOR TESTING RAISING cx_static_check.
    METHODS test_signed_too_small FOR TESTING RAISING cx_static_check.
    METHODS test_signed_bad_time FOR TESTING RAISING cx_static_check.

    METHODS read_integer
      IMPORTING iv_text         TYPE string
                iv_offset       TYPE i
      RETURNING VALUE(rv_value) TYPE i.
ENDCLASS.

CLASS ltcl_pdf_test IMPLEMENTATION.

  METHOD test_create.
    DATA(lo_pdf) = zcl_open_abap_pdf=>create( ).
    cl_abap_unit_assert=>assert_not_initial( lo_pdf ).
  ENDMETHOD.

  METHOD test_add_page.
    DATA(lo_pdf) = zcl_open_abap_pdf=>create( ).
    lo_pdf->add_page( ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_pdf->get_page_count( )
      exp = 1 ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_pdf->get_page_width( )
      exp = zcl_open_abap_pdf=>c_a4_width ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_pdf->get_page_height( )
      exp = zcl_open_abap_pdf=>c_a4_height ).
  ENDMETHOD.

  METHOD test_render_empty.
    DATA(lo_pdf) = zcl_open_abap_pdf=>create( ).
    lo_pdf->add_page( ).
    DATA(lv_result) = lo_pdf->render( ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*%PDF-1.4*' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*%%EOF*' ).
  ENDMETHOD.

  METHOD test_text.
    DATA(lo_pdf) = zcl_open_abap_pdf=>create( ).
    lo_pdf->add_page( ).
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 16 ).
    lo_pdf->text( iv_x = 50 iv_y = 50 iv_text = 'Hello World' ).

    DATA(lv_result) = lo_pdf->render( ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*(Hello World)*' ).
  ENDMETHOD.

  METHOD test_multiple_pages.
    DATA(lo_pdf) = zcl_open_abap_pdf=>create( ).
    lo_pdf->add_page( ).
    lo_pdf->add_page( ).
    lo_pdf->add_page( ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_pdf->get_page_count( )
      exp = 3 ).

    DATA(lv_result) = lo_pdf->render( ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*/Count 3*' ).
  ENDMETHOD.

  METHOD test_shapes.
    DATA(lo_pdf) = zcl_open_abap_pdf=>create( ).
    lo_pdf->add_page( ).
    lo_pdf->line( iv_x1 = 10 iv_y1 = 10 iv_x2 = 100 iv_y2 = 100 ).
    lo_pdf->rect( iv_x = 50 iv_y = 50 iv_width = 100 iv_height = 50 ).

    DATA(lv_result) = lo_pdf->render( ).

    " Just verify it renders without error
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*%PDF-1.4*' ).
  ENDMETHOD.

  METHOD test_mm_to_pt.
    DATA(lv_pt) = zcl_open_abap_pdf=>mm_to_pt( 10 ).
    DATA(lv_pt_int) = CONV i( lv_pt ).
    " 10mm should be approximately 28.35 points
    cl_abap_unit_assert=>assert_number_between(
      lower = 28
      upper = 29
      number = lv_pt_int ).
  ENDMETHOD.

  METHOD test_inch_to_pt.
    DATA(lv_pt) = zcl_open_abap_pdf=>inch_to_pt( 1 ).
    " 1 inch = 72 points
    cl_abap_unit_assert=>assert_equals(
      act = lv_pt
      exp = 72 ).
  ENDMETHOD.

  METHOD test_fluent_api.
    " Test that fluent API works
    DATA(lv_result) = zcl_open_abap_pdf=>create(
      )->add_page(
      )->set_font( iv_name = 'Courier' iv_size = 12
      )->set_text_color( iv_r = 255 iv_g = 0 iv_b = 0
      )->text( iv_x = 100 iv_y = 100 iv_text = 'Red Text'
      )->render( ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*(Red Text)*' ).
  ENDMETHOD.

  METHOD test_xref_offsets.
    DATA lv_pdf TYPE xstring.
    DATA lv_text TYPE string.
    DATA lv_xref TYPE i.
    DATA lv_entry TYPE i.
    DATA lv_offset TYPE i.
    DATA lv_object TYPE xstring.

    lv_pdf = zcl_open_abap_pdf=>create(
      )->add_page(
      )->text( iv_x = 10 iv_y = 10 iv_text = 'Offsets'
      )->add_page(
      )->render_binary( ).

* the header contains multi byte UTF-8 characters, so byte and character offsets differ
    lv_text = cl_abap_codepage=>convert_from( lv_pdf ).
    lv_xref = find( val = lv_text sub = |0000000000 65535 f \n| ) + 20.

    DO 6 TIMES.
      lv_entry = lv_xref + ( sy-index - 1 ) * 20.
      lv_offset = lv_text+lv_entry(10).
      lv_object = lv_pdf+lv_offset(8).
      cl_abap_unit_assert=>assert_char_cp(
        act = cl_abap_codepage=>convert_from( lv_object )
        exp = |{ sy-index } 0 obj*| ).
    ENDDO.
  ENDMETHOD.

  METHOD test_signed_byte_range.
    DATA lo_signer TYPE REF TO ltcl_fake_signer.
    DATA lv_pdf TYPE xstring.
    DATA lv_text TYPE string.
    DATA lv_offset TYPE i.
    DATA lv_start TYPE i.
    DATA lv_length1 TYPE i.
    DATA lv_offset2 TYPE i.
    DATA lv_length2 TYPE i.
    DATA lv_data TYPE xstring.
    DATA lv_rest TYPE xstring.
    DATA lv_byte TYPE xstring.

    CREATE OBJECT lo_signer.

    lv_pdf = zcl_open_abap_pdf=>create(
      )->add_page(
      )->text( iv_x = 10 iv_y = 10 iv_text = 'Signed'
      )->render_signed(
        io_signer    = lo_signer
        is_signature = VALUE #(
          name          = 'Tester'
          reason        = 'Approval (test)'
          signing_time  = '20261014120000'
          contents_size = 100 ) ).

    lv_text = cl_abap_codepage=>convert_from( lv_pdf ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_text
      exp = '*/Contents <DEADBEEF0000*' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_text
      exp = '*/Reason (Approval \(test\))*' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_text
      exp = '*/AcroForm << /Fields [* 0 R] /SigFlags 3 >>*' ).
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_text
      exp = '*/Annots [*' ).
    cl_abap_unit_assert=>assert_equals(
      act = lo_signer->mv_signing_time
      exp = '20261014120000' ).

* /ByteRange [0 a b c], everything but the hex string is signed
    lv_offset = find( val = lv_text sub = `/ByteRange [0 ` ) + 14.
    lv_start = read_integer( iv_text = lv_text iv_offset = lv_offset ).
    lv_offset = find( val = lv_text sub = ` ` off = lv_offset ) + 1.
    lv_offset2 = read_integer( iv_text = lv_text iv_offset = lv_offset ).
    lv_offset = find( val = lv_text sub = ` ` off = lv_offset ) + 1.
    lv_length2 = read_integer( iv_text = lv_text iv_offset = lv_offset ).
    lv_length1 = lv_start.

    cl_abap_unit_assert=>assert_equals(
      act = lv_offset2 - lv_start
      exp = 202 ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_offset2 + lv_length2
      exp = xstrlen( lv_pdf ) ).

    lv_byte = lv_pdf+lv_start(1).
    cl_abap_unit_assert=>assert_equals(
      act = cl_abap_codepage=>convert_from( lv_byte )
      exp = '<' ).
    lv_offset = lv_offset2 - 1.
    lv_byte = lv_pdf+lv_offset(1).
    cl_abap_unit_assert=>assert_equals(
      act = cl_abap_codepage=>convert_from( lv_byte )
      exp = '>' ).

    lv_data = lv_pdf(lv_length1).
    lv_rest = lv_pdf+lv_offset2(lv_length2).
    CONCATENATE lv_data lv_rest INTO lv_data IN BYTE MODE.
    cl_abap_unit_assert=>assert_equals(
      act = lo_signer->mv_data
      exp = lv_data ).
  ENDMETHOD.

  METHOD test_signed_no_page.
    DATA lo_signer TYPE REF TO ltcl_fake_signer.
    CREATE OBJECT lo_signer.
    TRY.
        zcl_open_abap_pdf=>create( )->render_signed( lo_signer ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_open_abap_pdf ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD test_signed_too_small.
    DATA lo_signer TYPE REF TO ltcl_fake_signer.
    CREATE OBJECT lo_signer.
    TRY.
        zcl_open_abap_pdf=>create( )->add_page( )->render_signed(
          io_signer    = lo_signer
          is_signature = VALUE #( contents_size = 2 ) ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_open_abap_pdf ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD test_signed_bad_time.
    DATA lo_signer TYPE REF TO ltcl_fake_signer.
    CREATE OBJECT lo_signer.
    TRY.
        zcl_open_abap_pdf=>create( )->add_page( )->render_signed(
          io_signer    = lo_signer
          is_signature = VALUE #( signing_time = '2026-10-14' ) ).
        cl_abap_unit_assert=>fail( ).
      CATCH zcx_open_abap_pdf ##NO_HANDLER.
    ENDTRY.
  ENDMETHOD.

  METHOD read_integer.
    DATA lv_offset TYPE i.
    DATA lv_length TYPE i.

    lv_offset = iv_offset.
    WHILE iv_text+lv_offset(1) CO '0123456789'.
      lv_offset = lv_offset + 1.
    ENDWHILE.
    lv_length = lv_offset - iv_offset.
    rv_value = iv_text+iv_offset(lv_length).
  ENDMETHOD.

ENDCLASS.
