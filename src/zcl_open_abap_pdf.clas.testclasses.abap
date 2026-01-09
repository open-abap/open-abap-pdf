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
ENDCLASS.

CLASS ltcl_pdf_test IMPLEMENTATION.

  METHOD test_create.
    DATA lo_pdf TYPE REF TO zcl_open_abap_pdf.
    lo_pdf = zcl_open_abap_pdf=>create( ).
    cl_abap_unit_assert=>assert_not_initial( lo_pdf ).
  ENDMETHOD.

  METHOD test_add_page.
    DATA lo_pdf TYPE REF TO zcl_open_abap_pdf.
    lo_pdf = zcl_open_abap_pdf=>create( ).
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
    DATA lo_pdf TYPE REF TO zcl_open_abap_pdf.
    DATA lv_result TYPE string.

    lo_pdf = zcl_open_abap_pdf=>create( ).
    lo_pdf->add_page( ).
    lv_result = lo_pdf->render( ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*%PDF-1.4*' ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*%%EOF*' ).
  ENDMETHOD.

  METHOD test_text.
    DATA lo_pdf TYPE REF TO zcl_open_abap_pdf.
    DATA lv_result TYPE string.

    lo_pdf = zcl_open_abap_pdf=>create( ).
    lo_pdf->add_page( ).
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 16 ).
    lo_pdf->text( iv_x = 50 iv_y = 50 iv_text = 'Hello World' ).

    lv_result = lo_pdf->render( ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*(Hello World)*' ).
  ENDMETHOD.

  METHOD test_multiple_pages.
    DATA lo_pdf TYPE REF TO zcl_open_abap_pdf.
    DATA lv_result TYPE string.

    lo_pdf = zcl_open_abap_pdf=>create( ).
    lo_pdf->add_page( ).
    lo_pdf->add_page( ).
    lo_pdf->add_page( ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_pdf->get_page_count( )
      exp = 3 ).

    lv_result = lo_pdf->render( ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*/Count 3*' ).
  ENDMETHOD.

  METHOD test_shapes.
    DATA lo_pdf TYPE REF TO zcl_open_abap_pdf.
    DATA lv_result TYPE string.

    lo_pdf = zcl_open_abap_pdf=>create( ).
    lo_pdf->add_page( ).
    lo_pdf->line( iv_x1 = 10 iv_y1 = 10 iv_x2 = 100 iv_y2 = 100 ).
    lo_pdf->rect( iv_x = 50 iv_y = 50 iv_width = 100 iv_height = 50 ).

    lv_result = lo_pdf->render( ).

    " Just verify it renders without error
    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*%PDF-1.4*' ).
  ENDMETHOD.

  METHOD test_mm_to_pt.
    DATA lv_pt TYPE f.
    DATA lv_pt_int TYPE i.
    lv_pt = zcl_open_abap_pdf=>mm_to_pt( 10 ).
    lv_pt_int = lv_pt.
    " 10mm should be approximately 28.35 points
    cl_abap_unit_assert=>assert_number_between(
      lower = 28
      upper = 29
      number = lv_pt_int ).
  ENDMETHOD.

  METHOD test_inch_to_pt.
    DATA lv_pt TYPE f.
    lv_pt = zcl_open_abap_pdf=>inch_to_pt( 1 ).
    " 1 inch = 72 points
    cl_abap_unit_assert=>assert_equals(
      act = lv_pt
      exp = 72 ).
  ENDMETHOD.

  METHOD test_fluent_api.
    DATA lo_pdf TYPE REF TO zcl_open_abap_pdf.
    DATA lv_result TYPE string.

    " Test that fluent API works
    lv_result = zcl_open_abap_pdf=>create(
      )->add_page(
      )->set_font( iv_name = 'Courier' iv_size = 12
      )->set_text_color( iv_r = 255 iv_g = 0 iv_b = 0
      )->text( iv_x = 100 iv_y = 100 iv_text = 'Red Text'
      )->render( ).

    cl_abap_unit_assert=>assert_char_cp(
      act = lv_result
      exp = '*(Red Text)*' ).
  ENDMETHOD.

ENDCLASS.
