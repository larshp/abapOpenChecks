*----------------------------------------------------------------------*
*       CLASS lcl_Test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
  CLASS lcl_test DEFINITION FOR TESTING
    DURATION SHORT
    RISK LEVEL HARMLESS
    FINAL.


    PRIVATE SECTION.
* ================

      METHODS: parse_str FOR TESTING.

  ENDCLASS.       "lcl_Test

*----------------------------------------------------------------------*
*       CLASS lcl_Test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
  CLASS lcl_test IMPLEMENTATION.
* ==============================

    METHOD parse_str.
* =================

      DATA: lt_code  TYPE string_table,
            lv_match TYPE abap_bool.


      _code 'BACK.'.

      lv_match = zcl_aoc_parser=>parse_str( lt_code ).

      cl_abap_unit_assert=>assert_equals( exp = abap_true
                                          act = lv_match ).

    ENDMETHOD.       "parse_Str

  ENDCLASS.       "lcl_Test