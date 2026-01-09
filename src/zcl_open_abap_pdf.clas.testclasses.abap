CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test_dummy FOR TESTING.
ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.
  METHOD test_dummy.
    cl_abap_unit_assert=>assert_equals(
      act = 1
      exp = 1 ).
  ENDMETHOD.
ENDCLASS.
