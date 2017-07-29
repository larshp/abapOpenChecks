CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      to_string1 FOR TESTING,
      to_string2 FOR TESTING.

ENDCLASS.       "ltcl_Test

CLASS ltcl_test IMPLEMENTATION.

  METHOD to_string1.

    DATA: lo_and     TYPE REF TO zcl_aoc_boolean_node,
          lv_string  TYPE string,
          lo_compare TYPE REF TO zcl_aoc_boolean_node.


    CREATE OBJECT lo_and
      EXPORTING
        iv_type = zcl_aoc_boolean_node=>c_type-and.

    CREATE OBJECT lo_compare
      EXPORTING
        iv_type = zcl_aoc_boolean_node=>c_type-compare.

    lo_and->append_child( lo_compare ).
    lo_and->append_child( lo_compare ).

    lv_string = lo_and->to_string( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_string
      exp = 'AND( COMPARE COMPARE )' ).

  ENDMETHOD.

  METHOD to_string2.

    DATA: lo_not     TYPE REF TO zcl_aoc_boolean_node,
          lv_string  TYPE string,
          lo_compare TYPE REF TO zcl_aoc_boolean_node.


    CREATE OBJECT lo_not
      EXPORTING
        iv_type = zcl_aoc_boolean_node=>c_type-not.

    CREATE OBJECT lo_compare
      EXPORTING
        iv_type = zcl_aoc_boolean_node=>c_type-compare.

    lo_not->append_child( lo_compare ).

    lv_string = lo_not->to_string( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_string
      exp = 'NOT( COMPARE )' ).

  ENDMETHOD.

ENDCLASS.
