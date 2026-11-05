CLASS zcl_sales_order_report DEFINITION PUBLIC.
  PUBLIC SECTION.
    TYPES:
      BEGIN OF ty_order_item,
        material   TYPE string,
        quantity   TYPE i,
        unit_price TYPE f,
        currency   TYPE string,
      END OF ty_order_item,
      ty_order_items TYPE STANDARD TABLE OF ty_order_item WITH DEFAULT KEY,

      BEGIN OF ty_sales_order,
        customer   TYPE string,
        sales_org  TYPE string,
        order_date TYPE string,
        items      TYPE ty_order_items,
      END OF ty_sales_order,

      BEGIN OF ty_result,
        order_number TYPE string,
        success      TYPE abap_bool,
        message      TYPE string,
        pdf          TYPE xstring,
      END OF ty_result.

    "! Create a new instance of the sales order report
    CLASS-METHODS create
      RETURNING VALUE(ro_report) TYPE REF TO zcl_sales_order_report.

    "! Run the report: validates input, creates the sales order and generates a PDF confirmation
    "! @parameter is_order | Input sales order data (header and items)
    "! @parameter rs_result | Result containing order number, status, and PDF confirmation
    METHODS run
      IMPORTING is_order         TYPE ty_sales_order
      RETURNING VALUE(rs_result) TYPE ty_result.

  PRIVATE SECTION.
    CLASS-DATA mv_order_counter TYPE i.

    METHODS validate_order
      IMPORTING is_order          TYPE ty_sales_order
      RETURNING VALUE(rv_message) TYPE string.

    METHODS build_order_number
      RETURNING VALUE(rv_number) TYPE string.

    METHODS calculate_total
      IMPORTING it_items        TYPE ty_order_items
      RETURNING VALUE(rv_total) TYPE f.

    METHODS format_amount
      IMPORTING iv_amount      TYPE f
      RETURNING VALUE(rv_text) TYPE string.

    METHODS build_pdf
      IMPORTING is_order        TYPE ty_sales_order
                iv_order_number TYPE string
                iv_total        TYPE f
      RETURNING VALUE(rv_pdf)   TYPE xstring.
ENDCLASS.

CLASS zcl_sales_order_report IMPLEMENTATION.

  METHOD create.
    ro_report = NEW zcl_sales_order_report( ).
  ENDMETHOD.

  METHOD run.
    DATA lv_validation_message TYPE string.
    DATA lv_order_number TYPE string.
    DATA lv_total TYPE f.

    lv_validation_message = validate_order( is_order ).

    IF lv_validation_message IS NOT INITIAL.
      rs_result-success = abap_false.
      rs_result-message = lv_validation_message.
      RETURN.
    ENDIF.

    " In a real SAP system this would call the standard Sales Order API,
    " e.g. the SAP S/4HANA Sales Order (A2X) API or BAPI_SALESORDER_CREATEFROMDAT2
    lv_order_number = build_order_number( ).
    lv_total = calculate_total( is_order-items ).

    rs_result-order_number = lv_order_number.
    rs_result-success = abap_true.
    rs_result-message = |Sales order { lv_order_number } created successfully|.
    rs_result-pdf = build_pdf(
      is_order        = is_order
      iv_order_number = lv_order_number
      iv_total        = lv_total ).
  ENDMETHOD.

  METHOD validate_order.
    DATA ls_item TYPE ty_order_item.

    IF is_order-customer IS INITIAL.
      rv_message = 'Customer is required'.
      RETURN.
    ENDIF.

    IF is_order-sales_org IS INITIAL.
      rv_message = 'Sales organization is required'.
      RETURN.
    ENDIF.

    IF is_order-order_date IS INITIAL.
      rv_message = 'Order date is required'.
      RETURN.
    ENDIF.

    IF lines( is_order-items ) = 0.
      rv_message = 'At least one order item is required'.
      RETURN.
    ENDIF.

    LOOP AT is_order-items INTO ls_item.
      IF ls_item-material IS INITIAL.
        rv_message = 'Material is required for all items'.
        RETURN.
      ENDIF.
      IF ls_item-quantity <= 0.
        rv_message = 'Quantity must be greater than zero'.
        RETURN.
      ENDIF.
      IF ls_item-unit_price <= 0.
        rv_message = 'Unit price must be greater than zero'.
        RETURN.
      ENDIF.
      IF strlen( ls_item-currency ) <> 3.
        rv_message = 'Currency must be a 3-character ISO code'.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.

  METHOD build_order_number.
    DATA lv_padded TYPE string.

    " In production SAP, a number range object (NROB) would be used here
    mv_order_counter = mv_order_counter + 1.
    lv_padded = |{ mv_order_counter }|.
    WHILE strlen( lv_padded ) < 10.
      lv_padded = '0' && lv_padded.
    ENDWHILE.
    rv_number = |SO-{ lv_padded }|.
  ENDMETHOD.

  METHOD calculate_total.
    DATA ls_item TYPE ty_order_item.

    rv_total = 0.
    LOOP AT it_items INTO ls_item.
      rv_total = rv_total + ( ls_item-unit_price * ls_item-quantity ).
    ENDLOOP.
  ENDMETHOD.

  METHOD format_amount.
    DATA lv_abs TYPE f.
    DATA lv_int TYPE i.
    DATA lv_dec TYPE i.
    DATA lv_frac TYPE f.

    IF iv_amount < 0.
      rv_text = '-'.
      lv_abs = iv_amount * -1.
    ELSE.
      rv_text = ''.
      lv_abs = iv_amount.
    ENDIF.

    lv_int = floor( lv_abs ).
    lv_frac = ( lv_abs - lv_int ) * 100.
    lv_dec = round( val = lv_frac dec = 0 ).

    IF lv_dec >= 100.
      lv_int = lv_int + 1.
      lv_dec = 0.
    ENDIF.

    rv_text = rv_text && |{ lv_int }|.
    IF lv_dec > 0.
      IF lv_dec < 10.
        rv_text = rv_text && |.0{ lv_dec }|.
      ELSE.
        rv_text = rv_text && |.{ lv_dec }|.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  METHOD build_pdf.
    DATA lo_pdf TYPE REF TO zcl_open_abap_pdf.
    DATA lv_row TYPE f.
    DATA ls_item TYPE ty_order_item.
    DATA lv_line_total TYPE f.

    lo_pdf = zcl_open_abap_pdf=>create( ).
    lo_pdf->add_page( ).

    " Title
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 24 ).
    lo_pdf->set_text_color( iv_r = 0 iv_g = 51 iv_b = 102 ).
    lo_pdf->text( iv_x = 50 iv_y = 50 iv_text = 'Sales Order Confirmation' ).

    lo_pdf->set_draw_color( iv_r = 0 iv_g = 51 iv_b = 102 ).
    lo_pdf->set_line_width( iv_width = 2 ).
    lo_pdf->line( iv_x1 = 50 iv_y1 = 65 iv_x2 = 545 iv_y2 = 65 ).

    " Order header details
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 12 ).
    lo_pdf->set_text_color( iv_r = 0 iv_g = 0 iv_b = 0 ).
    lo_pdf->text( iv_x = 50 iv_y = 90 iv_text = |Order Number:       { iv_order_number }| ).
    lo_pdf->text( iv_x = 50 iv_y = 110 iv_text = |Customer:           { is_order-customer }| ).
    lo_pdf->text( iv_x = 50 iv_y = 130 iv_text = |Sales Organization: { is_order-sales_org }| ).
    lo_pdf->text( iv_x = 50 iv_y = 150 iv_text = |Order Date:         { is_order-order_date }| ).

    " Items table header
    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 11 ).
    lo_pdf->set_fill_color( iv_r = 0 iv_g = 51 iv_b = 102 ).
    lo_pdf->rect( iv_x = 50 iv_y = 175 iv_width = 495 iv_height = 20 iv_style = 'F' ).
    lo_pdf->set_text_color( iv_r = 255 iv_g = 255 iv_b = 255 ).
    lo_pdf->text( iv_x = 55 iv_y = 189 iv_text = 'Material' ).
    lo_pdf->text( iv_x = 255 iv_y = 189 iv_text = 'Qty' ).
    lo_pdf->text( iv_x = 355 iv_y = 189 iv_text = 'Unit Price' ).
    lo_pdf->text( iv_x = 455 iv_y = 189 iv_text = 'Amount' ).

    " Item rows
    lo_pdf->set_text_color( iv_r = 0 iv_g = 0 iv_b = 0 ).
    lo_pdf->set_font( iv_name = 'Times-Roman' iv_size = 11 ).
    lv_row = 215.

    LOOP AT is_order-items INTO ls_item.
      lv_line_total = ls_item-unit_price * ls_item-quantity.
      lo_pdf->text( iv_x = 55  iv_y = lv_row iv_text = ls_item-material ).
      lo_pdf->text( iv_x = 255 iv_y = lv_row iv_text = |{ ls_item-quantity }| ).
      lo_pdf->text( iv_x = 355 iv_y = lv_row iv_text = |{ ls_item-currency } { format_amount( ls_item-unit_price ) }| ).
      lo_pdf->text( iv_x = 455 iv_y = lv_row iv_text = |{ ls_item-currency } { format_amount( lv_line_total ) }| ).
      lv_row = lv_row + 20.
    ENDLOOP.

    " Total line
    lo_pdf->set_draw_color( iv_r = 0 iv_g = 0 iv_b = 0 ).
    lo_pdf->set_line_width( iv_width = 1 ).
    lo_pdf->line( iv_x1 = 350 iv_y1 = lv_row iv_x2 = 545 iv_y2 = lv_row ).
    lv_row = lv_row + 15.

    lo_pdf->set_font( iv_name = 'Helvetica' iv_size = 12 ).
    lo_pdf->text( iv_x = 355 iv_y = lv_row iv_text = |Total: { format_amount( iv_total ) }| ).

    " Footer
    lo_pdf->set_font( iv_name = 'Courier' iv_size = 10 ).
    lo_pdf->set_text_color( iv_r = 150 iv_g = 150 iv_b = 150 ).
    lo_pdf->text( iv_x = 50 iv_y = 800 iv_text = 'Generated with open-abap-pdf' ).

    rv_pdf = lo_pdf->render_binary( ).
  ENDMETHOD.

ENDCLASS.
