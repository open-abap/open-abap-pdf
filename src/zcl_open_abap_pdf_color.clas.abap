CLASS zcl_open_abap_pdf_color DEFINITION PUBLIC CREATE PRIVATE.
  PUBLIC SECTION.
    "! Convert RGB color to PDF color string
    "! @parameter iv_r | Red component (0-255)
    "! @parameter iv_g | Green component (0-255)
    "! @parameter iv_b | Blue component (0-255)
    "! @parameter iv_operator | PDF operator (rg for fill, RG for stroke)
    CLASS-METHODS rgb_to_pdf_string
      IMPORTING iv_r             TYPE i DEFAULT 0
                iv_g             TYPE i DEFAULT 0
                iv_b             TYPE i DEFAULT 0
                iv_operator      TYPE string DEFAULT 'rg'
      RETURNING VALUE(rv_string) TYPE string.

    "! Format a number for PDF output (max 2 decimal places, no trailing zeros)
    CLASS-METHODS format_number
      IMPORTING iv_number        TYPE f
      RETURNING VALUE(rv_string) TYPE string.
ENDCLASS.

CLASS zcl_open_abap_pdf_color IMPLEMENTATION.

  METHOD rgb_to_pdf_string.
    " Validate RGB values are within range (0-255)
    DATA(lv_r_val) = COND i( WHEN iv_r < 0 THEN 0
                               WHEN iv_r > 255 THEN 255
                               ELSE iv_r ).
    DATA(lv_g_val) = COND i( WHEN iv_g < 0 THEN 0
                               WHEN iv_g > 255 THEN 255
                               ELSE iv_g ).
    DATA(lv_b_val) = COND i( WHEN iv_b < 0 THEN 0
                               WHEN iv_b > 255 THEN 255
                               ELSE iv_b ).

    " Convert to PDF color space (0.0 to 1.0)
    DATA(lv_r) = CONV f( lv_r_val / 255 ).
    DATA(lv_g) = CONV f( lv_g_val / 255 ).
    DATA(lv_b) = CONV f( lv_b_val / 255 ).

    rv_string = |{ format_number( lv_r ) } { format_number( lv_g ) } { format_number( lv_b ) } { iv_operator }|.
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

ENDCLASS.
