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

    DATA: mt_code   TYPE string_table,
          ms_result TYPE scirest_ad,
          mo_check  TYPE REF TO zcl_aoc_check_17.

    METHODS: setup,
             test001_01 FOR TESTING,
             test001_02 FOR TESTING,
             test001_03 FOR TESTING,
             test001_04 FOR TESTING,
             test001_05 FOR TESTING.

ENDCLASS.       "lcl_Test

*----------------------------------------------------------------------*
*       CLASS lcl_Test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test IMPLEMENTATION.
* ==============================

  DEFINE _code.
    append &1 to mt_code.
  END-OF-DEFINITION.

  METHOD setup.
    CREATE OBJECT mo_check.
  ENDMETHOD.                    "setup

  METHOD test001_01.
* ===========

    _code 'FORM foo.'.
    _code '  DATA: lv_bar TYPE i.'.
    _code '  WRITE: / lv_bar.'.
    _code '  DATA: lv_moo TYPE i.'.
    _code '  WRITE: / lv_moo.'.
    _code 'ENDFORM.'.

    ms_result = zcl_aoc_unit_test=>check( it_code  = mt_code
                                          io_check = mo_check ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test1

  METHOD test001_02.
* ===========

    _code 'FORM foo.'.
    _code '  DATA: lv_bar TYPE i.'.
    _code '  DATA: lv_moo TYPE i.'.
    _code '  WRITE: / lv_bar.'.
    _code '  WRITE: / lv_moo.'.
    _code 'ENDFORM.'.

    ms_result = zcl_aoc_unit_test=>check( it_code  = mt_code
                                          io_check = mo_check ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test2

  METHOD test001_03.
* ===========

    _code 'METHOD foo.'.
    _code '  DATA: lv_bar TYPE i.'.
    _code '  WRITE: / lv_bar.'.
    _code '  DATA: lv_moo TYPE i.'.
    _code '  WRITE: / lv_moo.'.
    _code 'ENDMETHOD.'.

    ms_result = zcl_aoc_unit_test=>check( it_code  = mt_code
                                          io_check = mo_check ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test1

  METHOD test001_04.
* ===========

    _code 'METHOD foo.'.
    _code '  DATA: lv_bar TYPE i.'.
    _code '  DATA: lv_moo TYPE i.'.
    _code '  WRITE: / lv_bar.'.
    _code '  WRITE: / lv_moo.'.
    _code 'ENDMETHOD.'.

    ms_result = zcl_aoc_unit_test=>check( it_code  = mt_code
                                          io_check = mo_check ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_04

  METHOD test001_05.
* ===========

    _code 'METHOD foo.'.
    _code '* comment'.
    _code '  DATA: lv_bar TYPE i.'.
    _code '  WRITE: / lv_bar.'.
    _code 'ENDMETHOD.'.

    ms_result = zcl_aoc_unit_test=>check( it_code  = mt_code
                                          io_check = mo_check ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_05

ENDCLASS.       "lcl_Test