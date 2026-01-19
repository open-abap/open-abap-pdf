CLASS zcl_open_abap_pdf_page DEFINITION PUBLIC CREATE PUBLIC.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_page,
        id         TYPE i,
        obj_id     TYPE i,
        content_id TYPE i,
        width      TYPE f,
        height     TYPE f,
        content    TYPE string,
      END OF ty_page.

    "! Constructor
    "! @parameter iv_id | Page number
    "! @parameter iv_width | Page width in points
    "! @parameter iv_height | Page height in points
    METHODS constructor
      IMPORTING iv_id     TYPE i
                iv_width  TYPE f DEFAULT '595.28'
                iv_height TYPE f DEFAULT '841.89'.

    "! Get page ID
    METHODS get_id
      RETURNING VALUE(rv_id) TYPE i.

    "! Get page width
    METHODS get_width
      RETURNING VALUE(rv_width) TYPE f.

    "! Get page height
    METHODS get_height
      RETURNING VALUE(rv_height) TYPE f.

    "! Append content to page stream
    METHODS append_content
      IMPORTING iv_content TYPE string.

    "! Get page content
    METHODS get_content
      RETURNING VALUE(rv_content) TYPE string.

    "! Set content object ID
    METHODS set_content_id
      IMPORTING iv_id TYPE i.

    "! Get content object ID
    METHODS get_content_id
      RETURNING VALUE(rv_id) TYPE i.

    "! Set page object ID
    METHODS set_obj_id
      IMPORTING iv_id TYPE i.

    "! Get page object ID
    METHODS get_obj_id
      RETURNING VALUE(rv_id) TYPE i.

    "! Transform Y coordinate (PDF uses bottom-left origin)
    METHODS transform_y
      IMPORTING iv_y        TYPE f
      RETURNING VALUE(rv_y) TYPE f.

  PRIVATE SECTION.
    DATA mv_id TYPE i.
    DATA mv_obj_id TYPE i.
    DATA mv_content_id TYPE i.
    DATA mv_width TYPE f.
    DATA mv_height TYPE f.
    DATA mv_content TYPE string.
ENDCLASS.

CLASS zcl_open_abap_pdf_page IMPLEMENTATION.

  METHOD constructor.
    mv_id = iv_id.
    mv_width = iv_width.
    mv_height = iv_height.
    mv_content = ''.
  ENDMETHOD.

  METHOD get_id.
    rv_id = mv_id.
  ENDMETHOD.

  METHOD get_width.
    rv_width = mv_width.
  ENDMETHOD.

  METHOD get_height.
    rv_height = mv_height.
  ENDMETHOD.

  METHOD append_content.
    IF mv_content IS NOT INITIAL.
      mv_content = mv_content && ` `.
    ENDIF.
    mv_content = mv_content && iv_content.
  ENDMETHOD.

  METHOD get_content.
    rv_content = mv_content.
  ENDMETHOD.

  METHOD set_content_id.
    mv_content_id = iv_id.
  ENDMETHOD.

  METHOD get_content_id.
    rv_id = mv_content_id.
  ENDMETHOD.

  METHOD set_obj_id.
    mv_obj_id = iv_id.
  ENDMETHOD.

  METHOD get_obj_id.
    rv_id = mv_obj_id.
  ENDMETHOD.

  METHOD transform_y.
    rv_y = mv_height - iv_y.
  ENDMETHOD.

ENDCLASS.
