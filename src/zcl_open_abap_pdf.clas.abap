CLASS zcl_open_abap_pdf DEFINITION PUBLIC.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_font,
        name   TYPE string,
        id     TYPE i,
        obj_id TYPE i,
      END OF ty_font,
      ty_fonts TYPE STANDARD TABLE OF ty_font WITH DEFAULT KEY,

      BEGIN OF ty_object,
        id      TYPE i,
        content TYPE string,
      END OF ty_object,
      ty_objects TYPE STANDARD TABLE OF ty_object WITH DEFAULT KEY,

      BEGIN OF ty_page,
        id         TYPE i,
        obj_id     TYPE i,
        content_id TYPE i,
        width      TYPE f,
        height     TYPE f,
        content    TYPE string,
      END OF ty_page,
      ty_pages TYPE STANDARD TABLE OF ty_page WITH DEFAULT KEY.

    CONSTANTS:
      c_pt_per_mm     TYPE f VALUE '2.83465',
      c_a4_width      TYPE f VALUE '595.28',  " 210mm in points
      c_a4_height     TYPE f VALUE '841.89'.  " 297mm in points

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
    DATA mt_pages TYPE ty_pages.
    DATA mt_fonts TYPE ty_fonts.
    DATA mt_objects TYPE ty_objects.
    DATA mv_current_page TYPE i.
    DATA mv_current_font TYPE string.
    DATA mv_current_font_size TYPE f.
    DATA mv_current_font_id TYPE i.
    DATA mv_next_obj_id TYPE i.
    DATA mv_text_color TYPE string.
    DATA mv_draw_color TYPE string.
    DATA mv_fill_color TYPE string.
    DATA mv_line_width TYPE f.

    METHODS add_object
      IMPORTING iv_content   TYPE string
      RETURNING VALUE(rv_id) TYPE i.

    METHODS escape_string
      IMPORTING iv_text           TYPE string
      RETURNING VALUE(rv_escaped) TYPE string.

    METHODS get_font_id
      IMPORTING iv_name      TYPE string
      RETURNING VALUE(rv_id) TYPE i.

    METHODS ensure_font
      IMPORTING iv_name TYPE string.

    METHODS transform_y
      IMPORTING iv_y        TYPE f
      RETURNING VALUE(rv_y) TYPE f.

    METHODS format_number
      IMPORTING iv_number        TYPE f
      RETURNING VALUE(rv_string) TYPE string.

    METHODS append_to_page
      IMPORTING iv_content TYPE string.
ENDCLASS.

CLASS zcl_open_abap_pdf IMPLEMENTATION.

  METHOD create.
    CREATE OBJECT ro_pdf.
    ro_pdf->mv_next_obj_id = 1.
    ro_pdf->mv_current_font = 'Helvetica'.
    ro_pdf->mv_current_font_size = 12.
    ro_pdf->mv_text_color = '0 0 0 rg'.
    ro_pdf->mv_draw_color = '0 0 0 RG'.
    ro_pdf->mv_fill_color = '1 1 1 rg'.
    ro_pdf->mv_line_width = 1.
  ENDMETHOD.

  METHOD add_page.
    mv_current_page = lines( mt_pages ) + 1.
    DATA(ls_page) = VALUE ty_page(
      id = mv_current_page
      width = iv_width
      height = iv_height
      content = '' ).
    APPEND ls_page TO mt_pages.

    ro_pdf = me.
  ENDMETHOD.

  METHOD set_font.
    ensure_font( iv_name ).
    mv_current_font = iv_name.
    mv_current_font_size = iv_size.
    mv_current_font_id = get_font_id( iv_name ).

    DATA(lv_content) = |/F{ mv_current_font_id } { format_number( iv_size ) } Tf|.
    append_to_page( lv_content ).

    ro_pdf = me.
  ENDMETHOD.

  METHOD set_text_color.
    DATA(lv_r) = CONV f( iv_r / 255 ).
    DATA(lv_g) = CONV f( iv_g / 255 ).
    DATA(lv_b) = CONV f( iv_b / 255 ).

    mv_text_color = |{ format_number( lv_r ) } { format_number( lv_g ) } { format_number( lv_b ) } rg|.
    append_to_page( mv_text_color ).

    ro_pdf = me.
  ENDMETHOD.

  METHOD set_draw_color.
    DATA(lv_r) = CONV f( iv_r / 255 ).
    DATA(lv_g) = CONV f( iv_g / 255 ).
    DATA(lv_b) = CONV f( iv_b / 255 ).

    mv_draw_color = |{ format_number( lv_r ) } { format_number( lv_g ) } { format_number( lv_b ) } RG|.
    append_to_page( mv_draw_color ).

    ro_pdf = me.
  ENDMETHOD.

  METHOD set_fill_color.
    DATA(lv_r) = CONV f( iv_r / 255 ).
    DATA(lv_g) = CONV f( iv_g / 255 ).
    DATA(lv_b) = CONV f( iv_b / 255 ).

    mv_fill_color = |{ format_number( lv_r ) } { format_number( lv_g ) } { format_number( lv_b ) } rg|.

    ro_pdf = me.
  ENDMETHOD.

  METHOD set_line_width.
    mv_line_width = iv_width.
    append_to_page( |{ format_number( iv_width ) } w| ).
    ro_pdf = me.
  ENDMETHOD.

  METHOD text.
    DATA(lv_y) = transform_y( iv_y ).
    DATA(lv_escaped) = escape_string( iv_text ).
    DATA(lv_content) = |BT { format_number( iv_x ) } { format_number( lv_y ) } Td ({ lv_escaped }) Tj ET|.
    append_to_page( lv_content ).

    ro_pdf = me.
  ENDMETHOD.

  METHOD line.
    DATA(lv_y1) = transform_y( iv_y1 ).
    DATA(lv_y2) = transform_y( iv_y2 ).
    DATA(lv_content) = |{ format_number( iv_x1 ) } { format_number( lv_y1 ) } m { format_number( iv_x2 ) } { format_number( lv_y2 ) } l S|.
    append_to_page( lv_content ).

    ro_pdf = me.
  ENDMETHOD.

  METHOD rect.
    DATA(lv_y) = transform_y( iv_y + iv_height ).
    DATA lv_op TYPE string.
    DATA lv_content TYPE string.

    CASE iv_style.
      WHEN 'F'.
        lv_op = 'f'.
      WHEN 'DF' OR 'FD'.
        lv_op = 'B'.
      WHEN OTHERS.
        lv_op = 'S'.
    ENDCASE.

    lv_content = |{ format_number( iv_x ) } { format_number( lv_y ) } { format_number( iv_width ) } { format_number( iv_height ) } re { lv_op }|.
    append_to_page( lv_content ).

    ro_pdf = me.
  ENDMETHOD.

  METHOD circle.
    DATA(lv_y) = transform_y( iv_y ).
    DATA(lv_k) = iv_radius * '0.5523'.  " Bezier curve approximation
    DATA lv_op TYPE string.
    DATA lv_content TYPE string.

    CASE iv_style.
      WHEN 'F'.
        lv_op = 'f'.
      WHEN 'DF' OR 'FD'.
        lv_op = 'B'.
      WHEN OTHERS.
        lv_op = 'S'.
    ENDCASE.

    " Draw circle using 4 Bezier curves
    lv_content = |{ format_number( iv_x + iv_radius ) } { format_number( lv_y ) } m |.
    lv_content = lv_content && |{ format_number( iv_x + iv_radius ) } { format_number( lv_y + lv_k ) } { format_number( iv_x + lv_k ) } { format_number( lv_y + iv_radius ) } { format_number( iv_x ) } { format_number( lv_y + iv_radius ) } c |.
    lv_content = lv_content && |{ format_number( iv_x - lv_k ) } { format_number( lv_y + iv_radius ) } { format_number( iv_x - iv_radius ) } { format_number( lv_y + lv_k ) } { format_number( iv_x - iv_radius ) } { format_number( lv_y ) } c |.
    lv_content = lv_content && |{ format_number( iv_x - iv_radius ) } { format_number( lv_y - lv_k ) } { format_number( iv_x - lv_k ) } { format_number( lv_y - iv_radius ) } { format_number( iv_x ) } { format_number( lv_y - iv_radius ) } c |.
    lv_content = lv_content && |{ format_number( iv_x + lv_k ) } { format_number( lv_y - iv_radius ) } { format_number( iv_x + iv_radius ) } { format_number( lv_y - lv_k ) } { format_number( iv_x + iv_radius ) } { format_number( lv_y ) } c |.
    lv_content = lv_content && lv_op.

    append_to_page( lv_content ).

    ro_pdf = me.
  ENDMETHOD.

  METHOD render.
    DATA lv_pdf TYPE string.
    DATA lv_catalog_id TYPE i.
    DATA lv_pages_id TYPE i.
    DATA lv_page_ids TYPE string.
    DATA lv_font_resources TYPE string.
    DATA lv_xref TYPE string.
    DATA lv_startxref TYPE i.
    DATA ls_page TYPE ty_page.
    DATA ls_font TYPE ty_font.
    DATA ls_object TYPE ty_object.
    DATA lv_obj_count TYPE i.
    DATA lv_content_length TYPE i.
    DATA lv_stream TYPE string.
    DATA lt_offsets TYPE STANDARD TABLE OF i WITH DEFAULT KEY.
    DATA lv_offset_val TYPE i.
    DATA lv_offset_str TYPE string.
    DATA lv_page_tabix TYPE i.
    DATA lv_obj_id TYPE i.
    DATA lv_marker TYPE string.

    " Reset objects for fresh render
    CLEAR mt_objects.
    mv_next_obj_id = 1.

    " Add fonts first
    LOOP AT mt_fonts INTO ls_font.
      ls_font-obj_id = add_object( |<< /Type /Font /Subtype /Type1 /BaseFont /{ ls_font-name } >>| ).
      MODIFY mt_fonts FROM ls_font INDEX sy-tabix.
    ENDLOOP.

    " Add page content streams and page objects
    LOOP AT mt_pages INTO ls_page.
      lv_page_tabix = sy-tabix.
      " Prepare content stream with font setup
      lv_stream = ''.
      IF mv_current_font IS NOT INITIAL.
        LOOP AT mt_fonts INTO ls_font WHERE name = mv_current_font.
          lv_stream = |/F{ ls_font-id } { format_number( mv_current_font_size ) } Tf |.
          EXIT.
        ENDLOOP.
      ENDIF.
      lv_stream = lv_stream && ls_page-content.

      lv_content_length = strlen( lv_stream ).
      ls_page-content_id = add_object( |<< /Length { lv_content_length } >>\nstream\n{ lv_stream }\nendstream| ).
      MODIFY mt_pages FROM ls_page INDEX lv_page_tabix.
    ENDLOOP.

    " Build font resources string
    LOOP AT mt_fonts INTO ls_font.
      IF lv_font_resources IS NOT INITIAL.
        lv_font_resources = lv_font_resources && | |.
      ENDIF.
      lv_font_resources = lv_font_resources && |/F{ ls_font-id } { ls_font-obj_id } 0 R|.
    ENDLOOP.

    " Add page objects
    lv_pages_id = mv_next_obj_id + lines( mt_pages ).
    LOOP AT mt_pages INTO ls_page.
      lv_obj_id = add_object( |<< /Type /Page /Parent { lv_pages_id } 0 R /MediaBox [0 0 { format_number( ls_page-width ) } { format_number( ls_page-height ) }] /Contents { ls_page-content_id } 0 R /Resources << /Font << { lv_font_resources } >> >> >>| ).

      IF lv_page_ids IS NOT INITIAL.
        lv_page_ids = lv_page_ids && | |.
      ENDIF.
      lv_page_ids = lv_page_ids && |{ lv_obj_id } 0 R|.
    ENDLOOP.

    " Add pages object
    lv_pages_id = add_object( |<< /Type /Pages /Kids [{ lv_page_ids }] /Count { lines( mt_pages ) } >>| ).

    " Add catalog
    lv_catalog_id = add_object( |<< /Type /Catalog /Pages { lv_pages_id } 0 R >>| ).

    " Build PDF header with binary marker (high-bit chars indicate binary content)
    lv_marker = cl_abap_codepage=>convert_from( CONV xstring( 'C2B5C2B6' ) ).
    lv_pdf = |%PDF-1.4\n%| && lv_marker && |\n|.

    " Write objects and track offsets
    LOOP AT mt_objects INTO ls_object.
      APPEND strlen( lv_pdf ) TO lt_offsets.
      lv_pdf = lv_pdf && |{ ls_object-id } 0 obj\n{ ls_object-content }\nendobj\n|.
    ENDLOOP.

    " Cross-reference table
    lv_startxref = strlen( lv_pdf ).
    lv_obj_count = lines( mt_objects ) + 1.
    lv_xref = |xref\n0 { lv_obj_count }\n0000000000 65535 f \n|.

    LOOP AT lt_offsets INTO lv_offset_val.
      lv_offset_str = |{ lv_offset_val }|.
      WHILE strlen( lv_offset_str ) < 10.
        lv_offset_str = '0' && lv_offset_str.
      ENDWHILE.
      lv_xref = lv_xref && |{ lv_offset_str } 00000 n \n|.
    ENDLOOP.

    lv_pdf = lv_pdf && lv_xref.
    lv_pdf = lv_pdf && |trailer\n<< /Size { lv_obj_count } /Root { lv_catalog_id } 0 R >>\nstartxref\n{ lv_startxref }\n%%EOF|.

    rv_pdf = lv_pdf.
  ENDMETHOD.

  METHOD render_binary.
    rv_pdf = cl_abap_codepage=>convert_to( render( ) ).
  ENDMETHOD.

  METHOD get_page_count.
    rv_count = lines( mt_pages ).
  ENDMETHOD.

  METHOD get_page_width.
    DATA ls_page TYPE ty_page.
    READ TABLE mt_pages INTO ls_page INDEX mv_current_page.
    IF sy-subrc = 0.
      rv_width = ls_page-width.
    ENDIF.
  ENDMETHOD.

  METHOD get_page_height.
    DATA ls_page TYPE ty_page.
    READ TABLE mt_pages INTO ls_page INDEX mv_current_page.
    IF sy-subrc = 0.
      rv_height = ls_page-height.
    ENDIF.
  ENDMETHOD.

  METHOD mm_to_pt.
    rv_pt = iv_mm * c_pt_per_mm.
  ENDMETHOD.

  METHOD inch_to_pt.
    rv_pt = iv_inch * 72.
  ENDMETHOD.

  METHOD add_object.
    DATA(ls_object) = VALUE ty_object(
        id = mv_next_obj_id
        content = iv_content ).
    APPEND ls_object TO mt_objects.
    mv_next_obj_id = mv_next_obj_id + 1.
    rv_id = ls_object-id.
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

  METHOD transform_y.
    DATA ls_page TYPE ty_page.
    READ TABLE mt_pages INTO ls_page INDEX mv_current_page.
    IF sy-subrc = 0.
      rv_y = ls_page-height - iv_y.
    ELSE.
      rv_y = iv_y.
    ENDIF.
  ENDMETHOD.

  METHOD format_number.
    DATA lv_int TYPE i.
    DATA lv_dec TYPE i.
    DATA lv_abs TYPE f.
    DATA lv_str TYPE string.
    DATA lv_dec_str TYPE string.
    DATA lv_neg TYPE abap_bool.
    DATA lv_fraction TYPE f.
    DATA lv_temp TYPE f.

    " Handle negative numbers
    IF iv_number < 0.
      lv_neg = abap_true.
      lv_abs = iv_number * -1.
    ELSE.
      lv_neg = abap_false.
      lv_abs = iv_number.
    ENDIF.

    " Get integer part
    lv_int = floor( lv_abs ).

    " Get decimal part (2 decimal places is enough for PDF)
    lv_fraction = lv_abs - lv_int.
    lv_temp = lv_fraction * 100.
    lv_dec = round( val = lv_temp dec = 0 ).

    " Handle rounding up to next integer
    IF lv_dec >= 100.
      lv_int = lv_int + 1.
      lv_dec = 0.
    ENDIF.

    " Build result
    IF lv_neg = abap_true.
      lv_str = |-{ lv_int }|.
    ELSE.
      lv_str = |{ lv_int }|.
    ENDIF.

    " Add decimals if non-zero
    IF lv_dec > 0.
      IF lv_dec < 10.
        lv_dec_str = |0{ lv_dec }|.
      ELSE.
        lv_dec_str = |{ lv_dec }|.
      ENDIF.
      " Remove trailing zero
      IF strlen( lv_dec_str ) = 2.
        IF lv_dec_str+1(1) = '0'.
          lv_dec_str = lv_dec_str(1).
        ENDIF.
      ENDIF.
      lv_str = lv_str && '.' && lv_dec_str.
    ENDIF.

    rv_string = lv_str.
  ENDMETHOD.

  METHOD append_to_page.
    FIELD-SYMBOLS <ls_page> TYPE ty_page.

    READ TABLE mt_pages ASSIGNING <ls_page> INDEX mv_current_page.
    IF sy-subrc = 0.
      IF <ls_page>-content IS NOT INITIAL.
        <ls_page>-content = <ls_page>-content && ` `.
      ENDIF.
      <ls_page>-content = <ls_page>-content && iv_content.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
