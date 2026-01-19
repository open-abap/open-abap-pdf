CLASS zcl_open_abap_pdf_graphics DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    CONSTANTS c_bezier_k TYPE f VALUE '0.5523'. " Bezier curve approximation constant for circles

    "! Draw a line from (x1, y1) to (x2, y2)
    CLASS-METHODS draw_line
      IMPORTING iv_x1             TYPE f
                iv_y1             TYPE f
                iv_x2             TYPE f
                iv_y2             TYPE f
      RETURNING VALUE(rv_content) TYPE string.

    "! Draw a rectangle
    "! @parameter iv_style | D=Draw, F=Fill, DF=Both
    CLASS-METHODS draw_rect
      IMPORTING iv_x              TYPE f
                iv_y              TYPE f
                iv_width          TYPE f
                iv_height         TYPE f
                iv_style          TYPE string DEFAULT 'D'
      RETURNING VALUE(rv_content) TYPE string.

    "! Draw a circle using Bezier curves
    CLASS-METHODS draw_circle
      IMPORTING iv_x              TYPE f
                iv_y              TYPE f
                iv_radius         TYPE f
                iv_style          TYPE string DEFAULT 'D'
      RETURNING VALUE(rv_content) TYPE string.

    "! Set line width
    CLASS-METHODS set_line_width
      IMPORTING iv_width          TYPE f
      RETURNING VALUE(rv_content) TYPE string.

  PRIVATE SECTION.
    "! Get PDF operator for shape style
    CLASS-METHODS get_style_operator
      IMPORTING iv_style     TYPE string
      RETURNING VALUE(rv_op) TYPE string.
ENDCLASS.

CLASS zcl_open_abap_pdf_graphics IMPLEMENTATION.

  METHOD draw_line.
    rv_content = |{ zcl_open_abap_pdf_color=>format_number( iv_x1 ) } { zcl_open_abap_pdf_color=>format_number( iv_y1 ) } m { zcl_open_abap_pdf_color=>format_number( iv_x2 ) } { zcl_open_abap_pdf_color=>format_number( iv_y2 ) } l S|.
  ENDMETHOD.

  METHOD draw_rect.
    DATA(lv_op) = get_style_operator( iv_style ).
    rv_content = |{ zcl_open_abap_pdf_color=>format_number( iv_x ) } { zcl_open_abap_pdf_color=>format_number( iv_y ) } { zcl_open_abap_pdf_color=>format_number( iv_width ) } { zcl_open_abap_pdf_color=>format_number( iv_height ) } re { lv_op }|.
  ENDMETHOD.

  METHOD draw_circle.
    DATA(lv_k) = iv_radius * c_bezier_k.  " Bezier curve approximation
    DATA(lv_op) = get_style_operator( iv_style ).

    " Draw circle using 4 Bezier curves
    DATA(lv_content) = |{ zcl_open_abap_pdf_color=>format_number( iv_x + iv_radius ) } { zcl_open_abap_pdf_color=>format_number( iv_y ) } m |.
    lv_content = lv_content && |{ zcl_open_abap_pdf_color=>format_number( iv_x + iv_radius ) } { zcl_open_abap_pdf_color=>format_number( iv_y + lv_k ) } { zcl_open_abap_pdf_color=>format_number( iv_x + lv_k ) } { zcl_open_abap_pdf_color=>format_number( iv_y + iv_radius ) } { zcl_open_abap_pdf_color=>format_number( iv_x ) } { zcl_open_abap_pdf_color=>format_number( iv_y + iv_radius ) } c |.
    lv_content = lv_content && |{ zcl_open_abap_pdf_color=>format_number( iv_x - lv_k ) } { zcl_open_abap_pdf_color=>format_number( iv_y + iv_radius ) } { zcl_open_abap_pdf_color=>format_number( iv_x - iv_radius ) } { zcl_open_abap_pdf_color=>format_number( iv_y + lv_k ) } { zcl_open_abap_pdf_color=>format_number( iv_x - iv_radius ) } { zcl_open_abap_pdf_color=>format_number( iv_y ) } c |.
    lv_content = lv_content && |{ zcl_open_abap_pdf_color=>format_number( iv_x - iv_radius ) } { zcl_open_abap_pdf_color=>format_number( iv_y - lv_k ) } { zcl_open_abap_pdf_color=>format_number( iv_x - lv_k ) } { zcl_open_abap_pdf_color=>format_number( iv_y - iv_radius ) } { zcl_open_abap_pdf_color=>format_number( iv_x ) } { zcl_open_abap_pdf_color=>format_number( iv_y - iv_radius ) } c |.
    lv_content = lv_content && |{ zcl_open_abap_pdf_color=>format_number( iv_x + lv_k ) } { zcl_open_abap_pdf_color=>format_number( iv_y - iv_radius ) } { zcl_open_abap_pdf_color=>format_number( iv_x + iv_radius ) } { zcl_open_abap_pdf_color=>format_number( iv_y - lv_k ) } { zcl_open_abap_pdf_color=>format_number( iv_x + iv_radius ) } { zcl_open_abap_pdf_color=>format_number( iv_y ) } c |.
    lv_content = lv_content && lv_op.

    rv_content = lv_content.
  ENDMETHOD.

  METHOD set_line_width.
    rv_content = |{ zcl_open_abap_pdf_color=>format_number( iv_width ) } w|.
  ENDMETHOD.

  METHOD get_style_operator.
    CASE iv_style.
      WHEN 'F'.
        rv_op = 'f'.
      WHEN 'DF' OR 'FD'.
        rv_op = 'B'.
      WHEN OTHERS.
        rv_op = 'S'.
    ENDCASE.
  ENDMETHOD.

ENDCLASS.
