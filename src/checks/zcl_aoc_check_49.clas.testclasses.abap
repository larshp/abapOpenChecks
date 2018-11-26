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
          mo_check  TYPE REF TO zcl_aoc_check_49.

    METHODS:
      setup,
      export_import FOR TESTING,
      test001_01 FOR TESTING,
      test001_02 FOR TESTING,
      test001_03 FOR TESTING,
      test001_04 FOR TESTING,
      test001_05 FOR TESTING,
      test001_06 FOR TESTING,
      test001_07 FOR TESTING,
      test001_08 FOR TESTING,
      test001_09 FOR TESTING,
      test010_01 FOR TESTING,
      test010_02 FOR TESTING,
      test010_03 FOR TESTING,
      test016_01 FOR TESTING,
      test016_02 FOR TESTING,
      test016_03 FOR TESTING,
      test016_04 FOR TESTING.

ENDCLASS.       "lcl_Test

*----------------------------------------------------------------------*
*       CLASS lcl_Test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_test IMPLEMENTATION.
* ==============================

  DEFINE _code.
    APPEND &1 TO mt_code.
  END-OF-DEFINITION.

  METHOD setup.
    CREATE OBJECT mo_check.
    zcl_aoc_unit_test=>set_check( mo_check ).
  ENDMETHOD.                    "setup

  METHOD export_import.
    zcl_aoc_unit_test=>export_import( mo_check ).
  ENDMETHOD.

  METHOD test001_01.
* ===========

    _code 'IF foo = bar.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test2

  METHOD test001_02.
* ===========

    _code 'IF  foo = bar.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_03.
* ===========

    _code 'WRITE ''IF  ''.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_04.
* ===========

    _code 'TYPES: BEGIN OF ty_foo,'.
    _code 'if  TYPE i,'.
    _code 'END OF ty_foo.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_05.
* ===========

    _code '  IF  foo = bar.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_06.
* ===========

    _code 'lo_alv->set_screen_status('.
    _code '  report        = sy-repid'.
    _code '  set_functions = lo_alv->c_functions_all ).'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_07.
* ===========

    _code 'DATA  lv_foo TYPE c.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '012'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_08.
* ===========

    _code 'DATA: lv_foo TYPE c.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_09.
* ===========

    _code 'CALL FUNCTION ''RS_VARIANT_EXISTS'''.
    _code '  EXPORTING'.
    _code '    report  = lv_report'.
    _code '    variant = lv_variant'.
    _code '  IMPORTING'.
    _code '    r_c     = lv_subrc.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test010_01.

    _code 'REPORT  zfoobar.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '010'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test010_02.

    _code 'REPORT zfoobar.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test010_03.

    _code 'REPORT.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test016_01.

    _code 'call_method(  ).'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '016'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test016_02.

    _code 'call_method( ).'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test016_03.

    _code 'call_method(   ).'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '016'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test016_04.

    _code 'call_method( ''foo(  )'' ).'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

ENDCLASS.
