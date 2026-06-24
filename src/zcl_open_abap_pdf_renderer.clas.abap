CLASS zcl_open_abap_pdf_renderer DEFINITION PUBLIC CREATE PUBLIC.
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
      ty_objects TYPE STANDARD TABLE OF ty_object WITH DEFAULT KEY.

    "! Constructor
    METHODS constructor.

    "! Add a PDF object and get its ID
    METHODS add_object
      IMPORTING iv_content   TYPE string
      RETURNING VALUE(rv_id) TYPE i.

    "! Render PDF from pages and fonts
    METHODS render
      IMPORTING it_pages      TYPE STANDARD TABLE
                it_fonts      TYPE ty_fonts
      RETURNING VALUE(rv_pdf) TYPE string.

  PRIVATE SECTION.
    DATA mt_objects TYPE ty_objects.
    DATA mv_next_obj_id TYPE i.
ENDCLASS.

CLASS zcl_open_abap_pdf_renderer IMPLEMENTATION.

  METHOD constructor.
    mv_next_obj_id = 1.
  ENDMETHOD.

  METHOD add_object.
    DATA(ls_object) = VALUE ty_object(
        id = mv_next_obj_id
        content = iv_content ).
    APPEND ls_object TO mt_objects.
    mv_next_obj_id = mv_next_obj_id + 1.
    rv_id = ls_object-id.
  ENDMETHOD.

  METHOD render.
    DATA lv_pdf TYPE string.
    DATA lv_catalog_id TYPE i.
    DATA lv_pages_id TYPE i.
    DATA lv_page_ids TYPE string.
    DATA lv_font_resources TYPE string.
    DATA lv_xref TYPE string.
    DATA lv_startxref TYPE i.
    DATA ls_font TYPE ty_font.
    DATA ls_object TYPE ty_object.
    DATA lv_obj_count TYPE i.
    DATA lv_content_length TYPE i.
    DATA lv_stream TYPE string.
    DATA lt_offsets TYPE STANDARD TABLE OF i WITH EMPTY KEY.
    DATA lv_offset_val TYPE i.
    DATA lv_offset_str TYPE string.
    DATA lv_obj_id TYPE i.
    DATA lv_marker TYPE string.
    DATA lt_fonts_copy TYPE ty_fonts.
    DATA ls_page TYPE zcl_open_abap_pdf_page=>ty_page.
    DATA lt_pages_copy TYPE STANDARD TABLE OF zcl_open_abap_pdf_page=>ty_page WITH EMPTY KEY.

    " Reset objects for fresh render
    CLEAR mt_objects.
    mv_next_obj_id = 1.

    " Add fonts first
    lt_fonts_copy = it_fonts.
    LOOP AT lt_fonts_copy INTO ls_font.
      ls_font-obj_id = add_object( |<< /Type /Font /Subtype /Type1 /BaseFont /{ ls_font-name } >>| ).
      MODIFY lt_fonts_copy FROM ls_font INDEX sy-tabix.
    ENDLOOP.

    " Add page content streams
    lt_pages_copy = it_pages.

    LOOP AT lt_pages_copy INTO ls_page.
      lv_stream = ls_page-content.
      lv_content_length = strlen( lv_stream ).
      ls_page-content_id = add_object( |<< /Length { lv_content_length } >>\nstream\n{ lv_stream }\nendstream| ).
      MODIFY lt_pages_copy FROM ls_page INDEX sy-tabix.
    ENDLOOP.

    " Build font resources string
    LOOP AT lt_fonts_copy INTO ls_font.
      IF lv_font_resources IS NOT INITIAL.
        lv_font_resources = lv_font_resources && | |.
      ENDIF.
      lv_font_resources = lv_font_resources && |/F{ ls_font-id } { ls_font-obj_id } 0 R|.
    ENDLOOP.

    " Add page objects
    lv_pages_id = mv_next_obj_id + lines( lt_pages_copy ).
    LOOP AT lt_pages_copy INTO ls_page.
      lv_obj_id = add_object( |<< /Type /Page /Parent { lv_pages_id } 0 R /MediaBox [0 0 { zcl_open_abap_pdf_color=>format_number( ls_page-width ) } { zcl_open_abap_pdf_color=>format_number( ls_page-height ) }] /Contents { ls_page-content_id } 0 R /Resources << /Font << { lv_font_resources } >> >> >>| ).

      IF lv_page_ids IS NOT INITIAL.
        lv_page_ids = lv_page_ids && | |.
      ENDIF.
      lv_page_ids = lv_page_ids && |{ lv_obj_id } 0 R|.
    ENDLOOP.

    " Add pages object
    lv_pages_id = add_object( |<< /Type /Pages /Kids [{ lv_page_ids }] /Count { lines( lt_pages_copy ) } >>| ).

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

ENDCLASS.
