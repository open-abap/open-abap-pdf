CLASS zcl_open_abap_pdf DEFINITION PUBLIC.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_font,
        name   TYPE string,
        id     TYPE i,
        obj_id TYPE i,
      END OF ty_font,
      ty_fonts TYPE STANDARD TABLE OF ty_font WITH DEFAULT KEY.

    CONSTANTS:
      c_pt_per_mm     TYPE f VALUE '2.83465',
      c_a4_width      TYPE f VALUE '595.28',  " 210mm in points
      c_a4_height     TYPE f VALUE '841.89',  " 297mm in points
      c_letter_width  TYPE f VALUE '612',     " 8.5 inches in points
      c_letter_height TYPE f VALUE '792'.     " 11 inches in points

    "! Create a new PDF document
    CLASS-METHODS create
      RETURNING VALUE(ro_pdf) TYPE REF TO zcl_open_abap_pdf.

    "! Add a new page to the document
    "! @parameter iv_width | Page width in points (default A4)
    "! @parameter iv_height | Page height in points (default A4)
    METHODS add_page
      IMPORTING iv_width      TYPE f DEFAULT '595.28'
                iv_height     TYPE f DEFAULT '841.89'
      RETURNING VALUE(ro_pdf) TYPE REF TO zcl_open_abap_pdf.

    "! Set the current font
    "! @parameter iv_name | Font name (Helvetica, Times-Roman, Courier)
    "! @parameter iv_size | Font size in points
    METHODS set_font
      IMPORTING iv_name       TYPE string DEFAULT 'Helvetica'
                iv_size       TYPE f DEFAULT 12
      RETURNING VALUE(ro_pdf) TYPE REF TO zcl_open_abap_pdf.

    "! Set the current text color (RGB 0-255)
    METHODS set_text_color
      IMPORTING iv_r          TYPE i DEFAULT 0
                iv_g          TYPE i DEFAULT 0
                iv_b          TYPE i DEFAULT 0
      RETURNING VALUE(ro_pdf) TYPE REF TO zcl_open_abap_pdf.

    "! Set the current draw color for lines and shapes (RGB 0-255)
    METHODS set_draw_color
      IMPORTING iv_r          TYPE i DEFAULT 0
                iv_g          TYPE i DEFAULT 0
                iv_b          TYPE i DEFAULT 0
      RETURNING VALUE(ro_pdf) TYPE REF TO zcl_open_abap_pdf.

    "! Set the current fill color (RGB 0-255)
    METHODS set_fill_color
      IMPORTING iv_r          TYPE i DEFAULT 255
                iv_g          TYPE i DEFAULT 255
                iv_b          TYPE i DEFAULT 255
      RETURNING VALUE(ro_pdf) TYPE REF TO zcl_open_abap_pdf.

    "! Set line width
    METHODS set_line_width
      IMPORTING iv_width      TYPE f DEFAULT 1
      RETURNING VALUE(ro_pdf) TYPE REF TO zcl_open_abap_pdf.

    "! Draw text at position (x, y from top-left)
    METHODS text
      IMPORTING iv_x          TYPE f
                iv_y          TYPE f
                iv_text       TYPE string
      RETURNING VALUE(ro_pdf) TYPE REF TO zcl_open_abap_pdf.

    "! Draw a line from (x1, y1) to (x2, y2)
    METHODS line
      IMPORTING iv_x1         TYPE f
                iv_y1         TYPE f
                iv_x2         TYPE f
                iv_y2         TYPE f
      RETURNING VALUE(ro_pdf) TYPE REF TO zcl_open_abap_pdf.

    "! Draw a rectangle
    "! @parameter iv_style | D=Draw, F=Fill, DF=Both
    METHODS rect
      IMPORTING iv_x          TYPE f
                iv_y          TYPE f
                iv_width      TYPE f
                iv_height     TYPE f
                iv_style      TYPE string DEFAULT 'D'
      RETURNING VALUE(ro_pdf) TYPE REF TO zcl_open_abap_pdf.

    "! Draw a circle
    METHODS circle
      IMPORTING iv_x          TYPE f
                iv_y          TYPE f
                iv_radius     TYPE f
                iv_style      TYPE string DEFAULT 'D'
      RETURNING VALUE(ro_pdf) TYPE REF TO zcl_open_abap_pdf.

    "! Render the PDF and return as string
    METHODS render
      RETURNING VALUE(rv_pdf) TYPE string.

    "! Render the PDF and return as xstring (binary)
    METHODS render_binary
      RETURNING VALUE(rv_pdf) TYPE xstring.

    "! Get current page number
    METHODS get_page_count
      RETURNING VALUE(rv_count) TYPE i.

    "! Get page width of current page
    METHODS get_page_width
      RETURNING VALUE(rv_width) TYPE f.

    "! Get page height of current page
    METHODS get_page_height
      RETURNING VALUE(rv_height) TYPE f.

    "! Convert millimeters to points
    CLASS-METHODS mm_to_pt
      IMPORTING iv_mm        TYPE f
      RETURNING VALUE(rv_pt) TYPE f.

    "! Convert inches to points
    CLASS-METHODS inch_to_pt
      IMPORTING iv_inch      TYPE f
      RETURNING VALUE(rv_pt) TYPE f.

  PRIVATE SECTION.
    DATA mt_pages TYPE STANDARD TABLE OF REF TO zcl_open_abap_pdf_page WITH DEFAULT KEY.
    DATA mt_fonts TYPE ty_fonts.
    DATA mo_renderer TYPE REF TO zcl_open_abap_pdf_renderer.
    DATA mv_current_page TYPE i.
    DATA mv_current_font TYPE string.
    DATA mv_current_font_size TYPE f.
    DATA mv_current_font_id TYPE i.
    DATA mv_text_color TYPE string.
    DATA mv_draw_color TYPE string.
    DATA mv_fill_color TYPE string.
    DATA mv_line_width TYPE f.

    METHODS escape_string
      IMPORTING iv_text           TYPE string
      RETURNING VALUE(rv_escaped) TYPE string.

    METHODS get_font_id
      IMPORTING iv_name      TYPE string
      RETURNING VALUE(rv_id) TYPE i.

    METHODS ensure_font
      IMPORTING iv_name TYPE string.

    METHODS get_current_page
      RETURNING VALUE(ro_page) TYPE REF TO zcl_open_abap_pdf_page.

    METHODS append_to_page
      IMPORTING iv_content TYPE string.
ENDCLASS.

CLASS zcl_open_abap_pdf IMPLEMENTATION.

  METHOD create.
    CREATE OBJECT ro_pdf.
    CREATE OBJECT ro_pdf->mo_renderer.
    ro_pdf->mv_current_font = 'Helvetica'.
    ro_pdf->mv_current_font_size = 12.
    ro_pdf->mv_text_color = '0 0 0 rg'.
    ro_pdf->mv_draw_color = '0 0 0 RG'.
    ro_pdf->mv_fill_color = '1 1 1 rg'.
    ro_pdf->mv_line_width = 1.
  ENDMETHOD.

  METHOD add_page.
    mv_current_page = lines( mt_pages ) + 1.
    DATA(lo_page) = NEW zcl_open_abap_pdf_page(
      iv_id = mv_current_page
      iv_width = iv_width
      iv_height = iv_height ).
    APPEND lo_page TO mt_pages.

    ro_pdf = me.
  ENDMETHOD.

  METHOD set_font.
    ensure_font( iv_name ).
    mv_current_font = iv_name.
    mv_current_font_size = iv_size.
    mv_current_font_id = get_font_id( iv_name ).

    DATA(lv_content) = |/F{ mv_current_font_id } { zcl_open_abap_pdf_color=>format_number( iv_size ) } Tf|.
    append_to_page( lv_content ).

    ro_pdf = me.
  ENDMETHOD.

  METHOD set_text_color.
    mv_text_color = zcl_open_abap_pdf_color=>rgb_to_pdf_string(
      iv_r = iv_r
      iv_g = iv_g
      iv_b = iv_b
      iv_operator = 'rg' ).
    append_to_page( mv_text_color ).

    ro_pdf = me.
  ENDMETHOD.

  METHOD set_draw_color.
    mv_draw_color = zcl_open_abap_pdf_color=>rgb_to_pdf_string(
      iv_r = iv_r
      iv_g = iv_g
      iv_b = iv_b
      iv_operator = 'RG' ).
    append_to_page( mv_draw_color ).

    ro_pdf = me.
  ENDMETHOD.

  METHOD set_fill_color.
    mv_fill_color = zcl_open_abap_pdf_color=>rgb_to_pdf_string(
      iv_r = iv_r
      iv_g = iv_g
      iv_b = iv_b
      iv_operator = 'rg' ).

    ro_pdf = me.
  ENDMETHOD.

  METHOD set_line_width.
    mv_line_width = iv_width.
    DATA(lv_content) = zcl_open_abap_pdf_graphics=>set_line_width( iv_width ).
    append_to_page( lv_content ).
    ro_pdf = me.
  ENDMETHOD.

  METHOD text.
    DATA(lo_page) = get_current_page( ).
    IF lo_page IS NOT BOUND.
      RETURN.
    ENDIF.

    DATA(lv_y) = lo_page->transform_y( iv_y ).
    DATA(lv_escaped) = escape_string( iv_text ).
    DATA(lv_content) = |BT { zcl_open_abap_pdf_color=>format_number( iv_x ) } { zcl_open_abap_pdf_color=>format_number( lv_y ) } Td ({ lv_escaped }) Tj ET|.
    append_to_page( lv_content ).

    ro_pdf = me.
  ENDMETHOD.

  METHOD line.
    DATA(lo_page) = get_current_page( ).
    IF lo_page IS NOT BOUND.
      RETURN.
    ENDIF.

    DATA(lv_y1) = lo_page->transform_y( iv_y1 ).
    DATA(lv_y2) = lo_page->transform_y( iv_y2 ).
    DATA(lv_content) = zcl_open_abap_pdf_graphics=>draw_line(
      iv_x1 = iv_x1
      iv_y1 = lv_y1
      iv_x2 = iv_x2
      iv_y2 = lv_y2 ).
    append_to_page( lv_content ).

    ro_pdf = me.
  ENDMETHOD.

  METHOD rect.
    DATA(lo_page) = get_current_page( ).
    IF lo_page IS NOT BOUND.
      RETURN.
    ENDIF.

    DATA(lv_y) = lo_page->transform_y( iv_y + iv_height ).
    DATA(lv_content) = zcl_open_abap_pdf_graphics=>draw_rect(
      iv_x = iv_x
      iv_y = lv_y
      iv_width = iv_width
      iv_height = iv_height
      iv_style = iv_style ).
    append_to_page( lv_content ).

    ro_pdf = me.
  ENDMETHOD.

  METHOD circle.
    DATA(lo_page) = get_current_page( ).
    IF lo_page IS NOT BOUND.
      RETURN.
    ENDIF.

    DATA(lv_y) = lo_page->transform_y( iv_y ).
    DATA(lv_content) = zcl_open_abap_pdf_graphics=>draw_circle(
      iv_x = iv_x
      iv_y = lv_y
      iv_radius = iv_radius
      iv_style = iv_style ).
    append_to_page( lv_content ).

    ro_pdf = me.
  ENDMETHOD.

  METHOD render.
    DATA lo_page_ref TYPE REF TO zcl_open_abap_pdf_page.
    " Convert page objects to structures for renderer
    DATA lt_page_structs TYPE STANDARD TABLE OF zcl_open_abap_pdf_page=>ty_page WITH EMPTY KEY.

    LOOP AT mt_pages INTO lo_page_ref.
      DATA(ls_page_struct) = VALUE zcl_open_abap_pdf_page=>ty_page(
        id = lo_page_ref->get_id( )
        width = lo_page_ref->get_width( )
        height = lo_page_ref->get_height( )
        content = lo_page_ref->get_content( ) ).
      APPEND ls_page_struct TO lt_page_structs.
    ENDLOOP.

    rv_pdf = mo_renderer->render(
      it_pages = lt_page_structs
      it_fonts = mt_fonts ).
  ENDMETHOD.

  METHOD render_binary.
    rv_pdf = cl_abap_codepage=>convert_to( render( ) ).
  ENDMETHOD.

  METHOD get_page_count.
    rv_count = lines( mt_pages ).
  ENDMETHOD.

  METHOD get_page_width.
    DATA(lo_page) = get_current_page( ).
    IF lo_page IS BOUND.
      rv_width = lo_page->get_width( ).
    ENDIF.
  ENDMETHOD.

  METHOD get_page_height.
    DATA(lo_page) = get_current_page( ).
    IF lo_page IS BOUND.
      rv_height = lo_page->get_height( ).
    ENDIF.
  ENDMETHOD.

  METHOD mm_to_pt.
    rv_pt = iv_mm * c_pt_per_mm.
  ENDMETHOD.

  METHOD inch_to_pt.
    rv_pt = iv_inch * 72.
  ENDMETHOD.

  METHOD escape_string.
    DATA lv_char TYPE string.
    DATA lv_i TYPE i.
    DATA lv_len TYPE i.

    rv_escaped = ''.
    lv_len = strlen( iv_text ).
    lv_i = 0.
    WHILE lv_i < lv_len.
      lv_char = iv_text+lv_i(1).
      CASE lv_char.
        WHEN '(' OR ')' OR '\'.
          rv_escaped = rv_escaped && '\' && lv_char.
        WHEN OTHERS.
          rv_escaped = rv_escaped && lv_char.
      ENDCASE.
      lv_i = lv_i + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD get_font_id.
    DATA ls_font TYPE ty_font.
    LOOP AT mt_fonts INTO ls_font WHERE name = iv_name.
      rv_id = ls_font-id.
      RETURN.
    ENDLOOP.
    rv_id = 0.
  ENDMETHOD.

  METHOD ensure_font.
    DATA ls_font TYPE ty_font.
    READ TABLE mt_fonts WITH KEY name = iv_name TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      ls_font-name = iv_name.
      ls_font-id = lines( mt_fonts ) + 1.
      APPEND ls_font TO mt_fonts.
    ENDIF.
  ENDMETHOD.

  METHOD get_current_page.
    READ TABLE mt_pages INTO ro_page INDEX mv_current_page.
  ENDMETHOD.

  METHOD append_to_page.
    DATA(lo_page) = get_current_page( ).
    IF lo_page IS BOUND.
      lo_page->append_content( iv_content ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
