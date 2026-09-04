CLASS zcl_open_abap_pdf DEFINITION PUBLIC.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_cmap_segment,
        start_code   TYPE i,
        end_code     TYPE i,
        id_delta     TYPE i,
        id_range_off TYPE i,
        range_pos    TYPE i,
      END OF ty_cmap_segment,
      ty_cmap_segments TYPE STANDARD TABLE OF ty_cmap_segment WITH DEFAULT KEY,

      BEGIN OF ty_font_char,
        unicode TYPE i,
        glyph   TYPE i,
      END OF ty_font_char,
      ty_font_chars TYPE STANDARD TABLE OF ty_font_char WITH DEFAULT KEY,

      BEGIN OF ty_font,
        name         TYPE string,
        id           TYPE i,
        obj_id       TYPE i,
        embedded     TYPE abap_bool,
        data         TYPE xstring,
        units_per_em TYPE i,
        hmtx_offset  TYPE i,
        num_hmetrics TYPE i,
        ascent       TYPE i,
        descent      TYPE i,
        segments     TYPE ty_cmap_segments,
        used_chars   TYPE ty_font_chars,
      END OF ty_font,
      ty_fonts TYPE STANDARD TABLE OF ty_font WITH DEFAULT KEY,

      ty_strings TYPE STANDARD TABLE OF string WITH DEFAULT KEY,

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

    "! Register and embed a TrueType font
    "! @parameter iv_name | Font name used by set_font
    "! @parameter iv_data | Complete TrueType font as xstring
    METHODS add_font
      IMPORTING iv_name       TYPE string
                iv_data       TYPE xstring
      RETURNING VALUE(ro_pdf) TYPE REF TO zcl_open_abap_pdf.

    "! Draw text at position (x, y from top-left)
    METHODS text
      IMPORTING iv_x          TYPE f
                iv_y          TYPE f
                iv_text       TYPE string
      RETURNING VALUE(ro_pdf) TYPE REF TO zcl_open_abap_pdf.

    "! Draw wrapped text, aligned inside a box, flowing to new pages
    "! @parameter iv_height | Box height; zero means the remaining page height
    "! @parameter iv_align | L/LEFT, C/CENTER, or R/RIGHT
    "! @parameter iv_line_height | Line height; zero means 120 percent of font size
    METHODS text_box
      IMPORTING iv_x           TYPE f
                iv_y           TYPE f
                iv_width       TYPE f
                iv_text        TYPE string
                iv_height      TYPE f DEFAULT 0
                iv_align       TYPE string DEFAULT 'L'
                iv_line_height TYPE f DEFAULT 0
      RETURNING VALUE(ro_pdf)  TYPE REF TO zcl_open_abap_pdf.

    "! Return the estimated width of text in the current font
    METHODS get_text_width
      IMPORTING iv_text         TYPE string
      RETURNING VALUE(rv_width) TYPE f.

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

    METHODS parse_custom_font
      CHANGING cs_font TYPE ty_font.

    METHODS read_uint8
      IMPORTING iv_data         TYPE xstring
                iv_offset       TYPE i
      RETURNING VALUE(rv_value) TYPE i.

    METHODS read_uint16
      IMPORTING iv_data         TYPE xstring
                iv_offset       TYPE i
      RETURNING VALUE(rv_value) TYPE i.

    METHODS read_int16
      IMPORTING iv_data         TYPE xstring
                iv_offset       TYPE i
      RETURNING VALUE(rv_value) TYPE i.

    METHODS read_uint32
      IMPORTING iv_data         TYPE xstring
                iv_offset       TYPE i
      RETURNING VALUE(rv_value) TYPE i.

    METHODS unicode_codepoint
      IMPORTING iv_char             TYPE string
      RETURNING VALUE(rv_codepoint) TYPE i.

    METHODS custom_glyph_id
      IMPORTING is_font         TYPE ty_font
                iv_codepoint    TYPE i
      RETURNING VALUE(rv_glyph) TYPE i.

    METHODS custom_glyph_width
      IMPORTING is_font         TYPE ty_font
                iv_glyph        TYPE i
      RETURNING VALUE(rv_width) TYPE f.

    METHODS encode_custom_text
      IMPORTING iv_text        TYPE string
                is_font        TYPE ty_font
      RETURNING VALUE(rv_text) TYPE string.

    METHODS remember_custom_chars
      IMPORTING iv_text TYPE string.

    METHODS int_to_hex4
      IMPORTING iv_value      TYPE i
      RETURNING VALUE(rv_hex) TYPE string.

    METHODS nibble_to_hex
      IMPORTING iv_value      TYPE i
      RETURNING VALUE(rv_hex) TYPE string.

    METHODS build_to_unicode
      IMPORTING is_font           TYPE ty_font
      RETURNING VALUE(rv_content) TYPE string.

    METHODS font_data_to_hex
      IMPORTING iv_data       TYPE xstring
      RETURNING VALUE(rv_hex) TYPE string.

    METHODS wrap_text
      IMPORTING iv_text         TYPE string
                iv_width        TYPE f
      RETURNING VALUE(rt_lines) TYPE ty_strings.
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

  METHOD add_font.
    DATA ls_font TYPE ty_font.
    DATA ls_existing TYPE ty_font.

    CLEAR ls_font.
    ls_font-name = iv_name.
    ls_font-data = iv_data.
    parse_custom_font( CHANGING cs_font = ls_font ).
    IF ls_font-embedded <> abap_true.
      ro_pdf = me.
      RETURN.
    ENDIF.

    READ TABLE mt_fonts INTO ls_existing WITH KEY name = iv_name.
    IF sy-subrc = 0.
      IF ls_existing-embedded = abap_true.
        ro_pdf = me.
        RETURN.
      ENDIF.
      DELETE mt_fonts INDEX sy-tabix.
    ENDIF.

    ls_font-id = lines( mt_fonts ) + 1.
    APPEND ls_font TO mt_fonts.

    ro_pdf = me.
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

  METHOD text_box.
    DATA lt_lines TYPE ty_strings.
    DATA lv_line TYPE string.
    DATA lv_line_height TYPE f.
    DATA lv_page_width TYPE f.
    DATA lv_page_height TYPE f.
    DATA lv_y TYPE f.
    DATA lv_x TYPE f.
    DATA lv_line_width TYPE f.
    DATA lv_max_lines TYPE i.
    DATA lv_page_line TYPE i.
    DATA lv_align TYPE string.

    IF mv_current_page = 0.
      add_page( ).
    ENDIF.
    ensure_font( mv_current_font ).

    lv_page_width = get_page_width( ).
    lv_page_height = get_page_height( ).
    lv_line_height = iv_line_height.
    IF lv_line_height <= 0.
      lv_line_height = mv_current_font_size * '1.2'.
    ENDIF.
    IF lv_line_height <= 0.
      lv_line_height = 12.
    ENDIF.

    IF iv_height > 0.
      lv_max_lines = floor( iv_height / lv_line_height ).
      IF lv_max_lines < 1.
        lv_max_lines = 1.
      ENDIF.
    ELSE.
      lv_max_lines = 0.
    ENDIF.

    lt_lines = wrap_text(
      iv_text = iv_text
      iv_width = iv_width ).
    lv_align = iv_align.
    TRANSLATE lv_align TO UPPER CASE.
    lv_page_line = 0.

    LOOP AT lt_lines INTO lv_line.
      IF lv_max_lines > 0 AND lv_page_line >= lv_max_lines.
        add_page(
          iv_width = lv_page_width
          iv_height = lv_page_height ).
        lv_page_line = 0.
      ENDIF.

      lv_y = iv_y + lv_page_line * lv_line_height.
      IF lv_y + lv_line_height > lv_page_height.
        add_page(
          iv_width = lv_page_width
          iv_height = lv_page_height ).
        lv_page_line = 0.
        lv_y = iv_y.
        IF lv_y + lv_line_height > lv_page_height.
          lv_y = 0.
        ENDIF.
      ENDIF.

      lv_line_width = get_text_width( lv_line ).
      lv_x = iv_x.
      CASE lv_align.
        WHEN 'C' OR 'CENTER' OR 'CENTRE'.
          lv_x = iv_x + ( iv_width - lv_line_width ) / 2.
        WHEN 'R' OR 'RIGHT'.
          lv_x = iv_x + iv_width - lv_line_width.
        WHEN OTHERS.
          lv_x = iv_x.
      ENDCASE.
      text(
        iv_x = lv_x
        iv_y = lv_y
        iv_text = lv_line ).
      lv_page_line = lv_page_line + 1.
    ENDLOOP.

    ro_pdf = me.
  ENDMETHOD.

  METHOD get_text_width.
    DATA ls_font TYPE ty_font.
    DATA lv_char TYPE string.
    DATA lv_i TYPE i.
    DATA lv_len TYPE i.
    DATA lv_codepoint TYPE i.
    DATA lv_glyph TYPE i.
    DATA lv_char_width TYPE f.

    READ TABLE mt_fonts INTO ls_font WITH KEY name = mv_current_font.
    IF sy-subrc <> 0.
      rv_width = 0.
      RETURN.
    ENDIF.

    lv_len = strlen( iv_text ).
    WHILE lv_i < lv_len.
      lv_char = iv_text+lv_i(1).
      IF ls_font-embedded = abap_true.
        lv_codepoint = unicode_codepoint( lv_char ).
        lv_glyph = custom_glyph_id(
          is_font = ls_font
          iv_codepoint = lv_codepoint ).
        lv_char_width = custom_glyph_width(
          is_font = ls_font
          iv_glyph = lv_glyph ).
        rv_width = rv_width + lv_char_width * mv_current_font_size / ls_font-units_per_em.
      ELSE.
        lv_char_width = '500'.
        IF mv_current_font = 'Courier'.
          lv_char_width = '600'.
        ELSEIF lv_char = ' '.
          IF mv_current_font = 'Times-Roman'.
            lv_char_width = '250'.
          ELSE.
            lv_char_width = '278'.
          ENDIF.
        ELSEIF lv_char = 'i' OR lv_char = 'l' OR lv_char = 'I'.
          lv_char_width = '278'.
        ELSEIF lv_char = 'm' OR lv_char = 'w' OR lv_char = 'M' OR lv_char = 'W'.
          lv_char_width = '833'.
        ELSEIF lv_char >= 'A' AND lv_char <= 'Z'.
          lv_char_width = '667'.
        ENDIF.
        rv_width = rv_width + lv_char_width * mv_current_font_size / 1000.
      ENDIF.
      lv_i = lv_i + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD text.
    DATA lv_y TYPE f.
    DATA lv_escaped TYPE string.
    DATA lv_content TYPE string.
    DATA ls_font TYPE ty_font.

    lv_y = transform_y( iv_y ).

    READ TABLE mt_fonts INTO ls_font WITH KEY name = mv_current_font.
    IF sy-subrc = 0 AND ls_font-embedded = abap_true.
      lv_escaped = encode_custom_text(
        iv_text = iv_text
        is_font = ls_font ).
      remember_custom_chars( iv_text = iv_text ).
      lv_content = |BT { format_number( iv_x ) } { format_number( lv_y ) } Td <{ lv_escaped }> Tj ET|.
    ELSE.
      lv_escaped = escape_string( iv_text ).
      lv_content = |BT { format_number( iv_x ) } { format_number( lv_y ) } Td ({ lv_escaped }) Tj ET|.
    ENDIF.
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
    DATA lv_font_hex TYPE string.
    DATA lv_font_file_id TYPE i.
    DATA lv_descriptor_id TYPE i.
    DATA lv_to_unicode_id TYPE i.
    DATA lv_widths TYPE string.
    DATA ls_used_char TYPE ty_font_char.
    DATA lv_glyph_width TYPE f.
    DATA lv_cid_font_id TYPE i.
    DATA lv_font_ascent TYPE f.
    DATA lv_font_descent TYPE f.

    " Reset objects for fresh render
    CLEAR mt_objects.
    mv_next_obj_id = 1.

    " Add fonts first
    LOOP AT mt_fonts INTO ls_font.
      IF ls_font-embedded = abap_true.
        lv_font_hex = font_data_to_hex( ls_font-data ).
        lv_font_file_id = add_object(
          |<< /Length { strlen( lv_font_hex ) } /Filter /ASCIIHexDecode >>\nstream\n{ lv_font_hex }\n>\nendstream| ).
        lv_font_ascent = ls_font-ascent * 1000 / ls_font-units_per_em.
        lv_font_descent = ls_font-descent * 1000 / ls_font-units_per_em.
        lv_descriptor_id = add_object(
          |<< /Type /FontDescriptor /FontName /{ ls_font-name } /Flags 32 /FontBBox [-1000 -300 1200 1000] /ItalicAngle 0 /Ascent { format_number( lv_font_ascent ) } /Descent { format_number( lv_font_descent ) } /CapHeight { format_number( lv_font_ascent ) } /StemV 80 /FontFile2 { lv_font_file_id } 0 R >>| ).
        lv_to_unicode_id = add_object( build_to_unicode( ls_font ) ).
        CLEAR lv_widths.
        LOOP AT ls_font-used_chars INTO ls_used_char.
          lv_glyph_width = custom_glyph_width(
            is_font = ls_font
            iv_glyph = ls_used_char-glyph ).
          IF lv_widths IS NOT INITIAL.
            lv_widths = lv_widths && | |.
          ENDIF.
          lv_glyph_width = lv_glyph_width * 1000 / ls_font-units_per_em.
          lv_widths = lv_widths && |{ ls_used_char-glyph } [{ format_number( lv_glyph_width ) }]|.
        ENDLOOP.
        lv_cid_font_id = add_object(
          |<< /Type /Font /Subtype /CIDFontType2 /BaseFont /{ ls_font-name } /CIDSystemInfo << /Registry (Adobe) /Ordering (Identity) /Supplement 0 >> /FontDescriptor { lv_descriptor_id } 0 R /DW 1000 /W [{ lv_widths }] /CIDToGIDMap /Identity >>| ).
        ls_font-obj_id = add_object(
          |<< /Type /Font /Subtype /Type0 /BaseFont /{ ls_font-name } /Encoding /Identity-H /DescendantFonts [{ lv_cid_font_id } 0 R] /ToUnicode { lv_to_unicode_id } 0 R >>| ).
      ELSE.
        ls_font-obj_id = add_object( |<< /Type /Font /Subtype /Type1 /BaseFont /{ ls_font-name } >>| ).
      ENDIF.
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

  METHOD parse_custom_font.
    DATA lv_num_tables TYPE i.
    DATA lv_directory_pos TYPE i.
    DATA lv_table_pos TYPE i.
    DATA lv_tag TYPE string.
    DATA lv_offset TYPE i.
    DATA lv_length TYPE i.
    DATA lv_cmap_offset TYPE i.
    DATA lv_cmap_length TYPE i.
    DATA lv_head_offset TYPE i.
    DATA lv_hhea_offset TYPE i.
    DATA lv_maxp_offset TYPE i.
    DATA lv_hmtx_offset TYPE i.
    DATA lv_version TYPE string.
    DATA lv_cmap_tables TYPE i.
    DATA lv_platform TYPE i.
    DATA lv_encoding TYPE i.
    DATA lv_subtable_offset TYPE i.
    DATA lv_format TYPE i.
    DATA lv_seg_count TYPE i.
    DATA lv_i TYPE i.
    DATA lv_end_pos TYPE i.
    DATA lv_start_pos TYPE i.
    DATA lv_delta_pos TYPE i.
    DATA lv_range_pos TYPE i.
    DATA ls_segment TYPE ty_cmap_segment.

    CLEAR cs_font-segments.
    cs_font-embedded = abap_false.
    IF xstrlen( cs_font-data ) < 12.
      RETURN.
    ENDIF.

    lv_version = CONV string( cs_font-data+0(4) ).
    IF lv_version <> '00010000' AND lv_version <> '4F54544F'.
      RETURN.
    ENDIF.
    IF lv_version = '4F54544F'.
      " CFF/OpenType programs are intentionally not handled here.
      RETURN.
    ENDIF.

    lv_num_tables = read_uint16(
      iv_data = cs_font-data
      iv_offset = 4 ).
    IF lv_num_tables < 1 OR xstrlen( cs_font-data ) < 12 + lv_num_tables * 16.
      RETURN.
    ENDIF.

    lv_directory_pos = 12.
    DO lv_num_tables TIMES.
      lv_tag = cl_abap_codepage=>convert_from( cs_font-data+lv_directory_pos(4) ).
      lv_table_pos = lv_directory_pos + 8.
      lv_offset = read_uint32(
        iv_data = cs_font-data
        iv_offset = lv_table_pos ).
      lv_table_pos = lv_directory_pos + 12.
      lv_length = read_uint32(
        iv_data = cs_font-data
        iv_offset = lv_table_pos ).
      IF lv_offset >= 0 AND lv_length >= 0 AND lv_offset + lv_length <= xstrlen( cs_font-data ).
        CASE lv_tag.
          WHEN 'cmap'.
            lv_cmap_offset = lv_offset.
            lv_cmap_length = lv_length.
          WHEN 'head'.
            lv_head_offset = lv_offset.
          WHEN 'hhea'.
            lv_hhea_offset = lv_offset.
          WHEN 'maxp'.
            lv_maxp_offset = lv_offset.
          WHEN 'hmtx'.
            lv_hmtx_offset = lv_offset.
        ENDCASE.
      ENDIF.
      lv_directory_pos = lv_directory_pos + 16.
    ENDDO.

    IF lv_cmap_offset = 0 OR lv_head_offset = 0 OR lv_hhea_offset = 0 OR lv_maxp_offset = 0 OR lv_hmtx_offset = 0.
      RETURN.
    ENDIF.
    IF lv_head_offset + 20 > xstrlen( cs_font-data ) OR lv_hhea_offset + 36 > xstrlen( cs_font-data ).
      RETURN.
    ENDIF.

    lv_table_pos = lv_head_offset + 18.
    cs_font-units_per_em = read_uint16(
      iv_data = cs_font-data
      iv_offset = lv_table_pos ).
    lv_table_pos = lv_hhea_offset + 4.
    cs_font-ascent = read_int16(
      iv_data = cs_font-data
      iv_offset = lv_table_pos ).
    lv_table_pos = lv_hhea_offset + 6.
    cs_font-descent = read_int16(
      iv_data = cs_font-data
      iv_offset = lv_table_pos ).
    lv_table_pos = lv_hhea_offset + 34.
    cs_font-num_hmetrics = read_uint16(
      iv_data = cs_font-data
      iv_offset = lv_table_pos ).
    cs_font-hmtx_offset = lv_hmtx_offset.
    IF cs_font-units_per_em = 0 OR cs_font-num_hmetrics = 0.
      RETURN.
    ENDIF.
    IF lv_maxp_offset + 6 > xstrlen( cs_font-data ).
      RETURN.
    ENDIF.

    IF lv_cmap_offset + 4 > xstrlen( cs_font-data ) OR lv_cmap_offset + lv_cmap_length > xstrlen( cs_font-data ).
      RETURN.
    ENDIF.
    lv_table_pos = lv_cmap_offset + 2.
    lv_cmap_tables = read_uint16(
      iv_data = cs_font-data
      iv_offset = lv_table_pos ).
    lv_i = 0.
    WHILE lv_i < lv_cmap_tables.
      lv_table_pos = lv_cmap_offset + 4 + lv_i * 8.
      IF lv_table_pos + 8 > xstrlen( cs_font-data ).
        RETURN.
      ENDIF.
      lv_platform = read_uint16(
        iv_data = cs_font-data
        iv_offset = lv_table_pos ).
      DATA(lv_encoding_pos) = lv_table_pos + 2.
      lv_encoding = read_uint16(
        iv_data = cs_font-data
        iv_offset = lv_encoding_pos ).
      DATA(lv_subtable_pos) = lv_table_pos + 4.
      lv_subtable_offset = read_uint32(
        iv_data = cs_font-data
        iv_offset = lv_subtable_pos ).
      IF ( ( lv_platform = 3 AND lv_encoding = 1 ) OR lv_platform = 0 )
          AND lv_cmap_offset + lv_subtable_offset + 2 <= xstrlen( cs_font-data ).
        lv_table_pos = lv_cmap_offset + lv_subtable_offset.
        lv_format = read_uint16(
          iv_data = cs_font-data
          iv_offset = lv_table_pos ).
        IF lv_format = 4.
          lv_table_pos = lv_cmap_offset + lv_subtable_offset.
          DATA(lv_seg_count_pos) = lv_table_pos + 6.
          lv_seg_count = read_uint16(
            iv_data = cs_font-data
            iv_offset = lv_seg_count_pos ) / 2.
          lv_end_pos = lv_table_pos + 14.
          lv_start_pos = lv_end_pos + lv_seg_count * 2 + 2.
          lv_delta_pos = lv_start_pos + lv_seg_count * 2.
          lv_range_pos = lv_delta_pos + lv_seg_count * 2.
          IF lv_range_pos + lv_seg_count * 2 <= xstrlen( cs_font-data ).
            DO lv_seg_count TIMES.
              DATA(lv_seg_index) = sy-index - 1.
              DATA(lv_segment_offset) = lv_seg_index * 2.
              ls_segment-start_code = read_uint16(
                iv_data = cs_font-data
                iv_offset = lv_start_pos + lv_segment_offset ).
              ls_segment-end_code = read_uint16(
                iv_data = cs_font-data
                iv_offset = lv_end_pos + lv_segment_offset ).
              ls_segment-id_delta = read_int16(
                iv_data = cs_font-data
                iv_offset = lv_delta_pos + lv_segment_offset ).
              ls_segment-id_range_off = read_uint16(
                iv_data = cs_font-data
                iv_offset = lv_range_pos + lv_segment_offset ).
              ls_segment-range_pos = lv_range_pos + lv_segment_offset.
              APPEND ls_segment TO cs_font-segments.
            ENDDO.
            IF lines( cs_font-segments ) > 0.
              cs_font-embedded = abap_true.
            ENDIF.
            RETURN.
          ENDIF.
        ENDIF.
      ENDIF.
      lv_i = lv_i + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD read_uint8.
    rv_value = CONV i( iv_data+iv_offset(1) ).
  ENDMETHOD.

  METHOD read_uint16.
    rv_value = CONV i( iv_data+iv_offset(2) ).
  ENDMETHOD.

  METHOD read_int16.
    rv_value = read_uint16(
      iv_data = iv_data
      iv_offset = iv_offset ).
    IF rv_value >= 32768.
      rv_value = rv_value - 65536.
    ENDIF.
  ENDMETHOD.

  METHOD read_uint32.
    rv_value = read_uint16(
      iv_data = iv_data
      iv_offset = iv_offset ) * 65536
      + read_uint16(
        iv_data = iv_data
        iv_offset = iv_offset + 2 ).
  ENDMETHOD.

  METHOD unicode_codepoint.
    DATA lv_utf8 TYPE xstring.
    DATA lv_first TYPE i.
    DATA lv_second TYPE i.
    DATA lv_third TYPE i.
    DATA lv_fourth TYPE i.

    lv_utf8 = cl_abap_codepage=>convert_to( iv_char ).
    IF xstrlen( lv_utf8 ) = 0.
      RETURN.
    ENDIF.
    lv_first = read_uint8(
      iv_data = lv_utf8
      iv_offset = 0 ).
    IF lv_first < 128.
      rv_codepoint = lv_first.
    ELSEIF lv_first < 224 AND xstrlen( lv_utf8 ) >= 2.
      lv_second = read_uint8(
        iv_data = lv_utf8
        iv_offset = 1 ).
      rv_codepoint = ( lv_first - 192 ) * 64 + lv_second - 128.
    ELSEIF lv_first < 240 AND xstrlen( lv_utf8 ) >= 3.
      lv_second = read_uint8(
        iv_data = lv_utf8
        iv_offset = 1 ).
      lv_third = read_uint8(
        iv_data = lv_utf8
        iv_offset = 2 ).
      rv_codepoint = ( lv_first - 224 ) * 4096 + ( lv_second - 128 ) * 64 + lv_third - 128.
    ELSEIF xstrlen( lv_utf8 ) >= 4.
      lv_second = read_uint8(
        iv_data = lv_utf8
        iv_offset = 1 ).
      lv_third = read_uint8(
        iv_data = lv_utf8
        iv_offset = 2 ).
      lv_fourth = read_uint8(
        iv_data = lv_utf8
        iv_offset = 3 ).
      rv_codepoint = ( lv_first - 240 ) * 262144 + ( lv_second - 128 ) * 4096 + ( lv_third - 128 ) * 64 + lv_fourth - 128.
    ENDIF.
  ENDMETHOD.

  METHOD custom_glyph_id.
    DATA ls_segment TYPE ty_cmap_segment.
    DATA lv_glyph TYPE i.
    DATA lv_position TYPE i.

    LOOP AT is_font-segments INTO ls_segment.
      IF iv_codepoint >= ls_segment-start_code AND iv_codepoint <= ls_segment-end_code.
        IF ls_segment-id_range_off = 0.
          lv_glyph = ( iv_codepoint + ls_segment-id_delta ) MOD 65536.
        ELSE.
          lv_position = ls_segment-range_pos + ls_segment-id_range_off + 2 * ( iv_codepoint - ls_segment-start_code ).
          IF lv_position + 2 <= xstrlen( is_font-data ).
            lv_glyph = read_uint16(
              iv_data = is_font-data
              iv_offset = lv_position ).
            IF lv_glyph <> 0.
              lv_glyph = ( lv_glyph + ls_segment-id_delta ) MOD 65536.
            ENDIF.
          ENDIF.
        ENDIF.
        IF lv_glyph < 0.
          lv_glyph = lv_glyph + 65536.
        ENDIF.
        rv_glyph = lv_glyph.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD custom_glyph_width.
    DATA lv_metric_pos TYPE i.
    DATA lv_width TYPE i.
    DATA lv_last_metric TYPE i.

    IF is_font-num_hmetrics = 0 OR is_font-units_per_em = 0.
      rv_width = 0.
      RETURN.
    ENDIF.
    lv_last_metric = is_font-num_hmetrics - 1.
    IF iv_glyph < is_font-num_hmetrics.
      lv_metric_pos = is_font-hmtx_offset + iv_glyph * 4.
    ELSE.
      lv_metric_pos = is_font-hmtx_offset + lv_last_metric * 4.
    ENDIF.
    IF lv_metric_pos + 2 <= xstrlen( is_font-data ).
      lv_width = read_uint16(
        iv_data = is_font-data
        iv_offset = lv_metric_pos ).
      rv_width = lv_width.
    ENDIF.
  ENDMETHOD.

  METHOD encode_custom_text.
    DATA lv_char TYPE string.
    DATA lv_i TYPE i.
    DATA lv_len TYPE i.
    DATA lv_codepoint TYPE i.
    DATA lv_glyph TYPE i.

    lv_len = strlen( iv_text ).
    WHILE lv_i < lv_len.
      lv_char = iv_text+lv_i(1).
      lv_codepoint = unicode_codepoint( lv_char ).
      lv_glyph = custom_glyph_id(
        is_font = is_font
        iv_codepoint = lv_codepoint ).
      rv_text = rv_text && int_to_hex4( lv_glyph ).
      lv_i = lv_i + 1.
    ENDWHILE.
  ENDMETHOD.

  METHOD remember_custom_chars.
    DATA lv_char TYPE string.
    DATA lv_i TYPE i.
    DATA lv_len TYPE i.
    DATA lv_codepoint TYPE i.
    DATA ls_used TYPE ty_font_char.
    DATA ls_font TYPE ty_font.
    DATA lv_font_tabix TYPE i.

    READ TABLE mt_fonts INTO ls_font WITH KEY name = mv_current_font.
    IF sy-subrc <> 0 OR ls_font-embedded <> abap_true.
      RETURN.
    ENDIF.
    lv_font_tabix = sy-tabix.

    lv_len = strlen( iv_text ).
    WHILE lv_i < lv_len.
      lv_char = iv_text+lv_i(1).
      lv_codepoint = unicode_codepoint( lv_char ).
      READ TABLE ls_font-used_chars WITH KEY unicode = lv_codepoint TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        ls_used-unicode = lv_codepoint.
        ls_used-glyph = custom_glyph_id(
          is_font = ls_font
          iv_codepoint = lv_codepoint ).
        APPEND ls_used TO ls_font-used_chars.
      ENDIF.
      lv_i = lv_i + 1.
    ENDWHILE.
    MODIFY mt_fonts FROM ls_font INDEX lv_font_tabix.
  ENDMETHOD.

  METHOD int_to_hex4.
    DATA lv_value TYPE i.
    lv_value = iv_value MOD 65536.
    IF lv_value < 0.
      lv_value = lv_value + 65536.
    ENDIF.
    rv_hex = nibble_to_hex( lv_value DIV 4096 )
      && nibble_to_hex( ( lv_value MOD 4096 ) DIV 256 )
      && nibble_to_hex( ( lv_value MOD 256 ) DIV 16 )
      && nibble_to_hex( lv_value MOD 16 ).
  ENDMETHOD.

  METHOD nibble_to_hex.
    CASE iv_value.
      WHEN 0.
        rv_hex = '0'.
      WHEN 1.
        rv_hex = '1'.
      WHEN 2.
        rv_hex = '2'.
      WHEN 3.
        rv_hex = '3'.
      WHEN 4.
        rv_hex = '4'.
      WHEN 5.
        rv_hex = '5'.
      WHEN 6.
        rv_hex = '6'.
      WHEN 7.
        rv_hex = '7'.
      WHEN 8.
        rv_hex = '8'.
      WHEN 9.
        rv_hex = '9'.
      WHEN 10.
        rv_hex = 'A'.
      WHEN 11.
        rv_hex = 'B'.
      WHEN 12.
        rv_hex = 'C'.
      WHEN 13.
        rv_hex = 'D'.
      WHEN 14.
        rv_hex = 'E'.
      WHEN OTHERS.
        rv_hex = 'F'.
    ENDCASE.
  ENDMETHOD.

  METHOD build_to_unicode.
    DATA ls_used TYPE ty_font_char.
    DATA lv_count TYPE i.
    DATA lv_stream TYPE string.

    lv_count = lines( is_font-used_chars ).
    lv_stream = |/CIDInit /ProcSet findresource begin\n12 dict begin\nbegincmap\n/CIDSystemInfo << /Registry (Adobe) /Ordering (Identity) /Supplement 0 >> def\n/CMapName /Adobe-Identity-UCS def\n/CMapType 2 def\n1 begincodespacerange\n<0000> <FFFF>\nendcodespacerange\n{ lv_count } beginbfchar\n|.
    LOOP AT is_font-used_chars INTO ls_used.
      lv_stream = lv_stream && |<{ int_to_hex4( ls_used-glyph ) }> <{ int_to_hex4( ls_used-unicode ) }>\n|.
    ENDLOOP.
    lv_stream = lv_stream && |endbfchar\nendcmap\nCMapName currentdict /CMap defineresource pop\nend\nend|.
    rv_content = |<< /Length { strlen( lv_stream ) } >>\nstream\n{ lv_stream }\nendstream|.
  ENDMETHOD.

  METHOD font_data_to_hex.
    rv_hex = CONV string( iv_data ).
  ENDMETHOD.

  METHOD wrap_text.
    DATA lv_text TYPE string.
    DATA lt_paragraphs TYPE ty_strings.
    DATA lt_words TYPE ty_strings.
    DATA lv_paragraph TYPE string.
    DATA lv_word TYPE string.
    DATA lv_current TYPE string.
    DATA lv_candidate TYPE string.
    DATA lv_part TYPE string.
    DATA lv_char TYPE string.
    DATA lv_i TYPE i.
    DATA lv_len TYPE i.

    lv_text = iv_text.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN lv_text WITH cl_abap_char_utilities=>newline.
    SPLIT lv_text AT cl_abap_char_utilities=>newline INTO TABLE lt_paragraphs.

    LOOP AT lt_paragraphs INTO lv_paragraph.
      IF lv_paragraph IS INITIAL.
        APPEND '' TO rt_lines.
        CONTINUE.
      ENDIF.
      IF iv_width <= 0.
        APPEND lv_paragraph TO rt_lines.
        CONTINUE.
      ENDIF.

      CLEAR lv_current.
      SPLIT lv_paragraph AT space INTO TABLE lt_words.
      LOOP AT lt_words INTO lv_word.
        IF lv_word IS INITIAL.
          CONTINUE.
        ENDIF.
        IF get_text_width( lv_word ) > iv_width.
          IF lv_current IS NOT INITIAL.
            APPEND lv_current TO rt_lines.
            CLEAR lv_current.
          ENDIF.
          CLEAR lv_part.
          lv_len = strlen( lv_word ).
          lv_i = 0.
          WHILE lv_i < lv_len.
            lv_char = lv_word+lv_i(1).
            lv_candidate = lv_part && lv_char.
            IF lv_part IS NOT INITIAL AND get_text_width( lv_candidate ) > iv_width.
              APPEND lv_part TO rt_lines.
              lv_part = lv_char.
            ELSE.
              lv_part = lv_candidate.
            ENDIF.
            lv_i = lv_i + 1.
          ENDWHILE.
          lv_current = lv_part.
        ELSE.
          IF lv_current IS INITIAL.
            lv_candidate = lv_word.
          ELSE.
            lv_candidate = lv_current && | { lv_word }|.
          ENDIF.
          IF lv_current IS NOT INITIAL AND get_text_width( lv_candidate ) > iv_width.
            APPEND lv_current TO rt_lines.
            lv_current = lv_word.
          ELSE.
            lv_current = lv_candidate.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF lv_current IS NOT INITIAL.
        APPEND lv_current TO rt_lines.
      ENDIF.
    ENDLOOP.
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
