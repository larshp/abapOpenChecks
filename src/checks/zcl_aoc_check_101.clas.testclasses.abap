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
          mo_check  TYPE REF TO zcl_aoc_check_101.

    METHODS:
      setup,
      export_import FOR TESTING,
      test001_01 FOR TESTING,
      test001_02 FOR TESTING,
      test001_03 FOR TESTING RAISING cx_static_check,
      test001_04 FOR TESTING RAISING cx_static_check,
      test001_05 FOR TESTING RAISING cx_static_check,
      test001_06 FOR TESTING RAISING cx_static_check,
      test002_01 FOR TESTING RAISING cx_static_check,
      test002_02 FOR TESTING RAISING cx_static_check,
      test002_03 FOR TESTING RAISING cx_static_check,
      test002_04 FOR TESTING RAISING cx_static_check,
      test002_05 FOR TESTING RAISING cx_static_check,
      test003_01 FOR TESTING RAISING cx_static_check,
      test003_02 FOR TESTING RAISING cx_static_check,
      test003_03 FOR TESTING RAISING cx_static_check,
      test003_04 FOR TESTING RAISING cx_static_check,
      test003_05 FOR TESTING RAISING cx_static_check,
      test003_06 FOR TESTING RAISING cx_static_check.

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
  ENDMETHOD.                                               "setup

  METHOD export_import.
    zcl_aoc_unit_test=>export_import( mo_check ).
  ENDMETHOD.

  METHOD test001_01.
* ===========

    _code 'IF foo IS NOT INITIAL.'.
    _code '  foo = foo + 1.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                                               "test1

  METHOD test001_02.
* ===========

    cl_abap_unit_assert=>assert_initial( ms_result ).
    _code 'IF NOT foo IS INITIAL.'.
    _code '  foo = foo + 1.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                                               "test2

  METHOD test001_03.
* ===========

    cl_abap_unit_assert=>assert_initial( ms_result ).
    _code 'IF foo IS NOT INITIAL AND NOT bar IS INITIAL.'.
    _code '  foo = foo + 1.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                                               "test2

  METHOD test001_04.
* ===========

    _code 'IF NOT ( foo IS INITIAL ).'.                    "Not handled by check
    _code '  foo = foo + 1.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.


  METHOD test001_05.
* ===========

    _code 'IF ( NOT foo IS INITIAL ).'.
    _code '  foo = foo + 1.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).
  ENDMETHOD.


  METHOD test001_06.
* ===========

    _code 'IF lines( foo ) > 1.'.                          "Not handled by check
    _code '  bar = bar + 1.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.

  METHOD test002_01.
* ===========

    _code 'IF NOT line_exists( foo[ 1 ] ).'.
    _code '  WRITE 1.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.

  METHOD test002_02.
* ===========

    _code 'IF NOT line_exists( foo[ bar = 1 ] ).'.
    _code '  WRITE 1.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.


  METHOD test002_03.
* ===========

    _code 'IF NOT foo[ bar = 1 ]-var = 1.'.
    _code '  WRITE 1.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD test002_04.
* ===========

    _code 'IF matches( val = <ls_token>-str regex = `^\w*\[$` ).'.
    _code '  WRITE 1.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.

  METHOD test002_05.
* ===========

    _code 'if not line_exists( foo['.
    _code '    var1 = 1'.
    _code '    var2 = 2 ] ).'.
    _code '  select * from mara into table lt_mara'.
    _code '    where matnr = ls_matnr.'.
    _code ' endif.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.


  METHOD test003_01.
* ===========

    _code 'if not lcl_program=>some_check( lv_param ).'.
    _code '  "some code'.
    _code 'endif.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.


  METHOD test003_02.
* ===========

    _code 'if not lcl_program=>some_check( iv_param = lv_param ).'.
    _code '  "some code'.
    _code 'endif.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.

  METHOD test003_03.
* ===========

    _code 'if not lcl_program->some_check( lv_param ).'.
    _code '  "some code'.
    _code 'endif.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.


  METHOD test003_04.
* ===========

    _code 'if not lcl_program->some_check( iv_param = lv_param ).'.
    _code '  "some code'.
    _code 'endif.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.

  METHOD test003_05.
* ===========

    _code 'if not lcl_program=>some_check( ).'.
    _code '  "some code'.
    _code 'endif.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.


  METHOD test003_06.
* ===========

    _code 'if not lcl_program->some_check( ).'.
    _code '  "some code'.
    _code 'endif.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.
ENDCLASS.       "lcl_Test
