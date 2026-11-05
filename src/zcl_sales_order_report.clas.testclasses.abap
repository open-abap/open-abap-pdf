CLASS ltcl_sales_order_report_test DEFINITION FOR TESTING RISK LEVEL HARMLESS DURATION SHORT.
  PRIVATE SECTION.
    METHODS test_create FOR TESTING RAISING cx_static_check.
    METHODS test_run_valid_order FOR TESTING RAISING cx_static_check.
    METHODS test_run_no_customer FOR TESTING RAISING cx_static_check.
    METHODS test_run_no_sales_org FOR TESTING RAISING cx_static_check.
    METHODS test_run_no_order_date FOR TESTING RAISING cx_static_check.
    METHODS test_run_no_items FOR TESTING RAISING cx_static_check.
    METHODS test_run_invalid_qty FOR TESTING RAISING cx_static_check.
    METHODS test_run_invalid_price FOR TESTING RAISING cx_static_check.
    METHODS test_run_invalid_currency FOR TESTING RAISING cx_static_check.
    METHODS test_run_generates_pdf FOR TESTING RAISING cx_static_check.
    METHODS test_run_multiple_items FOR TESTING RAISING cx_static_check.
    METHODS test_order_numbers_unique FOR TESTING RAISING cx_static_check.
ENDCLASS.

CLASS ltcl_sales_order_report_test IMPLEMENTATION.

  METHOD test_create.
    DATA(lo_report) = zcl_sales_order_report=>create( ).
    cl_abap_unit_assert=>assert_not_initial( lo_report ).
  ENDMETHOD.

  METHOD test_run_valid_order.
    DATA lo_report TYPE REF TO zcl_sales_order_report.
    DATA ls_order TYPE zcl_sales_order_report=>ty_sales_order.
    DATA ls_item TYPE zcl_sales_order_report=>ty_order_item.
    DATA ls_result TYPE zcl_sales_order_report=>ty_result.

    lo_report = zcl_sales_order_report=>create( ).

    ls_item-material = 'LAPTOP-PRO'.
    ls_item-quantity = 2.
    ls_item-unit_price = 100.
    ls_item-currency = 'USD'.
    APPEND ls_item TO ls_order-items.

    ls_order-customer = 'CUST001'.
    ls_order-sales_org = 'US01'.
    ls_order-order_date = '2026-03-11'.

    ls_result = lo_report->run( ls_order ).

    cl_abap_unit_assert=>assert_not_initial( ls_result-success ).
    cl_abap_unit_assert=>assert_not_initial( ls_result-order_number ).
    cl_abap_unit_assert=>assert_not_initial( ls_result-message ).
  ENDMETHOD.

  METHOD test_run_no_customer.
    DATA lo_report TYPE REF TO zcl_sales_order_report.
    DATA ls_order TYPE zcl_sales_order_report=>ty_sales_order.
    DATA ls_item TYPE zcl_sales_order_report=>ty_order_item.
    DATA ls_result TYPE zcl_sales_order_report=>ty_result.

    lo_report = zcl_sales_order_report=>create( ).

    ls_item-material = 'ITEM01'.
    ls_item-quantity = 1.
    ls_item-unit_price = 50.
    ls_item-currency = 'EUR'.
    APPEND ls_item TO ls_order-items.

    ls_order-sales_org = 'DE01'.
    ls_order-order_date = '2026-03-11'.

    ls_result = lo_report->run( ls_order ).

    cl_abap_unit_assert=>assert_initial( ls_result-success ).
    cl_abap_unit_assert=>assert_not_initial( ls_result-message ).
  ENDMETHOD.

  METHOD test_run_no_sales_org.
    DATA lo_report TYPE REF TO zcl_sales_order_report.
    DATA ls_order TYPE zcl_sales_order_report=>ty_sales_order.
    DATA ls_item TYPE zcl_sales_order_report=>ty_order_item.
    DATA ls_result TYPE zcl_sales_order_report=>ty_result.

    lo_report = zcl_sales_order_report=>create( ).

    ls_item-material = 'ITEM01'.
    ls_item-quantity = 1.
    ls_item-unit_price = 50.
    ls_item-currency = 'USD'.
    APPEND ls_item TO ls_order-items.

    ls_order-customer = 'CUST001'.
    ls_order-order_date = '2026-03-11'.

    ls_result = lo_report->run( ls_order ).

    cl_abap_unit_assert=>assert_initial( ls_result-success ).
    cl_abap_unit_assert=>assert_not_initial( ls_result-message ).
  ENDMETHOD.

  METHOD test_run_no_order_date.
    DATA lo_report TYPE REF TO zcl_sales_order_report.
    DATA ls_order TYPE zcl_sales_order_report=>ty_sales_order.
    DATA ls_item TYPE zcl_sales_order_report=>ty_order_item.
    DATA ls_result TYPE zcl_sales_order_report=>ty_result.

    lo_report = zcl_sales_order_report=>create( ).

    ls_item-material = 'ITEM01'.
    ls_item-quantity = 1.
    ls_item-unit_price = 50.
    ls_item-currency = 'USD'.
    APPEND ls_item TO ls_order-items.

    ls_order-customer = 'CUST001'.
    ls_order-sales_org = 'US01'.

    ls_result = lo_report->run( ls_order ).

    cl_abap_unit_assert=>assert_initial( ls_result-success ).
    cl_abap_unit_assert=>assert_not_initial( ls_result-message ).
  ENDMETHOD.

  METHOD test_run_no_items.
    DATA lo_report TYPE REF TO zcl_sales_order_report.
    DATA ls_order TYPE zcl_sales_order_report=>ty_sales_order.
    DATA ls_result TYPE zcl_sales_order_report=>ty_result.

    lo_report = zcl_sales_order_report=>create( ).

    ls_order-customer = 'CUST001'.
    ls_order-sales_org = 'US01'.
    ls_order-order_date = '2026-03-11'.

    ls_result = lo_report->run( ls_order ).

    cl_abap_unit_assert=>assert_initial( ls_result-success ).
    cl_abap_unit_assert=>assert_not_initial( ls_result-message ).
  ENDMETHOD.

  METHOD test_run_invalid_qty.
    DATA lo_report TYPE REF TO zcl_sales_order_report.
    DATA ls_order TYPE zcl_sales_order_report=>ty_sales_order.
    DATA ls_item TYPE zcl_sales_order_report=>ty_order_item.
    DATA ls_result TYPE zcl_sales_order_report=>ty_result.

    lo_report = zcl_sales_order_report=>create( ).

    ls_item-material = 'ITEM01'.
    ls_item-quantity = 0.
    ls_item-unit_price = 100.
    ls_item-currency = 'USD'.
    APPEND ls_item TO ls_order-items.

    ls_order-customer = 'CUST001'.
    ls_order-sales_org = 'US01'.
    ls_order-order_date = '2026-03-11'.

    ls_result = lo_report->run( ls_order ).

    cl_abap_unit_assert=>assert_initial( ls_result-success ).
    cl_abap_unit_assert=>assert_not_initial( ls_result-message ).
  ENDMETHOD.

  METHOD test_run_invalid_price.
    DATA lo_report TYPE REF TO zcl_sales_order_report.
    DATA ls_order TYPE zcl_sales_order_report=>ty_sales_order.
    DATA ls_item TYPE zcl_sales_order_report=>ty_order_item.
    DATA ls_result TYPE zcl_sales_order_report=>ty_result.

    lo_report = zcl_sales_order_report=>create( ).

    ls_item-material = 'ITEM01'.
    ls_item-quantity = 1.
    ls_item-unit_price = 0.
    ls_item-currency = 'EUR'.
    APPEND ls_item TO ls_order-items.

    ls_order-customer = 'CUST001'.
    ls_order-sales_org = 'DE01'.
    ls_order-order_date = '2026-03-11'.

    ls_result = lo_report->run( ls_order ).

    cl_abap_unit_assert=>assert_initial( ls_result-success ).
    cl_abap_unit_assert=>assert_not_initial( ls_result-message ).
  ENDMETHOD.

  METHOD test_run_invalid_currency.
    DATA lo_report TYPE REF TO zcl_sales_order_report.
    DATA ls_order TYPE zcl_sales_order_report=>ty_sales_order.
    DATA ls_item TYPE zcl_sales_order_report=>ty_order_item.
    DATA ls_result TYPE zcl_sales_order_report=>ty_result.

    lo_report = zcl_sales_order_report=>create( ).

    ls_item-material = 'ITEM01'.
    ls_item-quantity = 1.
    ls_item-unit_price = 100.
    ls_item-currency = 'US'.
    APPEND ls_item TO ls_order-items.

    ls_order-customer = 'CUST001'.
    ls_order-sales_org = 'US01'.
    ls_order-order_date = '2026-03-11'.

    ls_result = lo_report->run( ls_order ).

    cl_abap_unit_assert=>assert_initial( ls_result-success ).
    cl_abap_unit_assert=>assert_not_initial( ls_result-message ).
  ENDMETHOD.

  METHOD test_run_generates_pdf.
    DATA lo_report TYPE REF TO zcl_sales_order_report.
    DATA ls_order TYPE zcl_sales_order_report=>ty_sales_order.
    DATA ls_item TYPE zcl_sales_order_report=>ty_order_item.
    DATA ls_result TYPE zcl_sales_order_report=>ty_result.

    lo_report = zcl_sales_order_report=>create( ).

    ls_item-material = 'SCREEN-4K'.
    ls_item-quantity = 3.
    ls_item-unit_price = 450.
    ls_item-currency = 'EUR'.
    APPEND ls_item TO ls_order-items.

    ls_order-customer = 'CUST002'.
    ls_order-sales_org = 'DE01'.
    ls_order-order_date = '2026-03-11'.

    ls_result = lo_report->run( ls_order ).

    cl_abap_unit_assert=>assert_not_initial( ls_result-success ).
    cl_abap_unit_assert=>assert_not_initial( ls_result-pdf ).
  ENDMETHOD.

  METHOD test_run_multiple_items.
    DATA lo_report TYPE REF TO zcl_sales_order_report.
    DATA ls_order TYPE zcl_sales_order_report=>ty_sales_order.
    DATA ls_item TYPE zcl_sales_order_report=>ty_order_item.
    DATA ls_result TYPE zcl_sales_order_report=>ty_result.

    lo_report = zcl_sales_order_report=>create( ).

    ls_item-material = 'ITEM-A'.
    ls_item-quantity = 2.
    ls_item-unit_price = 100.
    ls_item-currency = 'USD'.
    APPEND ls_item TO ls_order-items.

    ls_item-material = 'ITEM-B'.
    ls_item-quantity = 5.
    ls_item-unit_price = 20.
    ls_item-currency = 'USD'.
    APPEND ls_item TO ls_order-items.

    ls_item-material = 'ITEM-C'.
    ls_item-quantity = 1.
    ls_item-unit_price = 500.
    ls_item-currency = 'USD'.
    APPEND ls_item TO ls_order-items.

    ls_order-customer = 'CUST003'.
    ls_order-sales_org = 'US01'.
    ls_order-order_date = '2026-03-11'.

    ls_result = lo_report->run( ls_order ).

    cl_abap_unit_assert=>assert_not_initial( ls_result-success ).
    cl_abap_unit_assert=>assert_not_initial( ls_result-order_number ).
    cl_abap_unit_assert=>assert_not_initial( ls_result-pdf ).
  ENDMETHOD.

  METHOD test_order_numbers_unique.
    DATA lo_report TYPE REF TO zcl_sales_order_report.
    DATA ls_order TYPE zcl_sales_order_report=>ty_sales_order.
    DATA ls_item TYPE zcl_sales_order_report=>ty_order_item.
    DATA ls_result1 TYPE zcl_sales_order_report=>ty_result.
    DATA ls_result2 TYPE zcl_sales_order_report=>ty_result.

    lo_report = zcl_sales_order_report=>create( ).

    ls_item-material = 'ITEM01'.
    ls_item-quantity = 1.
    ls_item-unit_price = 100.
    ls_item-currency = 'USD'.
    APPEND ls_item TO ls_order-items.

    ls_order-customer = 'CUST001'.
    ls_order-sales_org = 'US01'.
    ls_order-order_date = '2026-03-11'.

    ls_result1 = lo_report->run( ls_order ).
    ls_result2 = lo_report->run( ls_order ).

    cl_abap_unit_assert=>assert_not_initial( ls_result1-success ).
    cl_abap_unit_assert=>assert_not_initial( ls_result2-success ).
    cl_abap_unit_assert=>assert_differs(
      act = ls_result1-order_number
      exp = ls_result2-order_number ).
  ENDMETHOD.

ENDCLASS.
