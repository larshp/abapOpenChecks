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
* ================

    DATA: mt_code   TYPE string_table,
          ms_result TYPE scirest_ad,
          mo_check  TYPE REF TO zcl_aoc_check_15.

    METHODS: setup.

    METHODS:
      test001_01 FOR TESTING,
      test001_02 FOR TESTING.

ENDCLASS.       "lcl_Test

*----------------------------------------------------------------------*
*       CLASS lcl_Test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_test IMPLEMENTATION.
* ==============================

  DEFINE _code.
    append &1 to mt_code.
  END-OF-DEFINITION.

  METHOD setup.
    CREATE OBJECT mo_check.
    zcl_aoc_unit_test=>set_check( mo_check ).
  ENDMETHOD.                    "setup

  METHOD test001_01.
* ===========

    _code 'CALL ''C_DIR_READ_FINISH'''.
    _code 'ID ''ERRNO''  FIELD file-errno'.
    _code 'ID ''ERRMSG'' FIELD file-errmsg.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test1

  METHOD test001_02.
* ===========

    _code 'CALL FUNCTION ''FOOBAR''.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test2

ENDCLASS.       "lcl_Test