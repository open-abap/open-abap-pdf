CLASS zcx_open_abap_pdf DEFINITION PUBLIC INHERITING FROM cx_static_check CREATE PUBLIC.
  PUBLIC SECTION.
    DATA mv_text TYPE string READ-ONLY.

    METHODS constructor
      IMPORTING
        textid   LIKE textid OPTIONAL
        previous LIKE previous OPTIONAL
        iv_text  TYPE string OPTIONAL.

    "! Raise exception with the given text
    CLASS-METHODS raise
      IMPORTING iv_text TYPE string
      RAISING   zcx_open_abap_pdf.

    METHODS get_text REDEFINITION.
ENDCLASS.

CLASS zcx_open_abap_pdf IMPLEMENTATION.

  METHOD constructor.
    super->constructor( textid = textid previous = previous ).
    mv_text = iv_text.
  ENDMETHOD.

  METHOD raise.
    RAISE EXCEPTION TYPE zcx_open_abap_pdf
      EXPORTING
        iv_text = iv_text.
  ENDMETHOD.

  METHOD get_text.
    result = mv_text.
  ENDMETHOD.

ENDCLASS.
