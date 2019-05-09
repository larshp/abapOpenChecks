*----------------------------------------------------------------------*
*       CLASS lcl_Test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    DATA: mt_code   TYPE string_table,
          ms_result TYPE scirest_ad,
          mv_text   TYPE string,
          mo_check  TYPE REF TO zcl_aoc_check_91.

    METHODS:
      setup,
      message_handler FOR EVENT message OF zcl_aoc_check_91 IMPORTING p_param_1,
      export_import FOR TESTING,
      test001 FOR TESTING.

ENDCLASS.       "lcl_Test

*----------------------------------------------------------------------*
*       CLASS lcl_Test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_test IMPLEMENTATION.

  DEFINE _code.
    APPEND &1 TO mt_code.
  END-OF-DEFINITION.

  METHOD setup.
    CREATE OBJECT mo_check.
    SET HANDLER message_handler FOR mo_check.
    zcl_aoc_unit_test=>set_check( mo_check ).
  ENDMETHOD.                    "setup

  METHOD message_handler.
    mv_text = p_param_1.
  ENDMETHOD.

  METHOD export_import.
    zcl_aoc_unit_test=>export_import( mo_check ).
  ENDMETHOD.

  METHOD test001.
    _code 'METHOD test.'.

    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.

    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.

    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.

    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.

    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'WRITE ''Just a Dummy Test''.'.

    _code 'WRITE ''Just a Dummy Test''.'.
    _code 'ENDMETHOD.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = 'E'
                                        act = ms_result-kind ).
    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).
  ENDMETHOD.


ENDCLASS.       "lcl_Test
