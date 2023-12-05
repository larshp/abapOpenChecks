CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcl_aoc_check_34 DEFINITION LOCAL FRIENDS ltcl_test.
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
          mo_check  TYPE REF TO zcl_aoc_check_34.

    METHODS:
      setup,
      export_import FOR TESTING,
      test001_01 FOR TESTING,
      test001_02 FOR TESTING,
      test001_03 FOR TESTING,
      test001_04 FOR TESTING RAISING cx_static_check,
      test001_05 FOR TESTING RAISING cx_static_check,
      test001_06 FOR TESTING RAISING cx_static_check.

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

    _code 'CASE lv_foo.'.
    _code '  WHEN ''bar''.'.
    _code '    WRITE: ''hello''.'.
    _code '    WRITE: ''hello''.'.
    _code '    WRITE: ''hello''.'.
    _code '    WRITE: ''helloo''.'.
    _code '    WRITE: ''hello''.'.
    _code '    WRITE: ''hello''.'.
    _code '    WRITE: ''hellob''.'.
    _code '    WRITE: ''hello''.'.
    _code '    WRITE: ''hello''.'.
    _code '    WRITE: ''helloo''.'.
    _code '    WRITE: ''hello''.'.
    _code '    WRITE: ''hello''.'.
    _code '    WRITE: ''hello''.'.
    _code '    WRITE: ''hello''.'.
    _code '    WRITE: ''hello''.'.
    _code '    WRITE: ''hello''.'.
    _code '    WRITE: ''helloa''.'.
    _code '    WRITE: ''helloo''.'.
    _code '    WRITE: ''hello''.'.
    _code '    WRITE: ''hello''.'.
    _code '    WRITE: ''hello''.'.
    _code '    WRITE: ''hello''.'.
    _code 'ENDCASE.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test1

  METHOD test001_02.
* ===========

    _code 'WRITE: ''foo''.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test2

  METHOD test001_03.
* ===========

    _code 'CASE lv_foo.'.
    _code '  WHEN ''bar''.'.
    _code '    WRITE: ''hello''.'.
    _code 'ENDCASE.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_04.
* ===========

    mo_check->mv_incl_comments = abap_false.
    mo_check->mv_lines = 20.

    _code 'CASE lv_foo.'.
    _code '  WHEN ''bar''.'.
    DO 10 TIMES.
      _code ' ".'.
      _code '*.'.
    ENDDO.
    _code 'ENDCASE.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).


  ENDMETHOD.

  METHOD test001_05.
* ===========

    mo_check->mv_incl_comments = abap_true.
    mo_check->mv_lines = 20.

    _code 'CASE lv_foo.'.
    _code '  WHEN ''bar''.'.
    DO 10 TIMES.
      _code ' ".'.
      _code '*.'.
    ENDDO.
    _code 'ENDCASE.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_not_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_06.
* ===========
    mo_check->mv_incl_comments = abap_false.
    mo_check->mv_lines = 5.

    _code 'CASE lv_foo.'.
    _code '  WHEN ''bar''.'.
    _code '    WRITE: ''hello''.'.
    _code '    WRITE: ''hello''.'.
    _code '*.'.
    _code '    WRITE: ''hello''.'.
    _code '    WRITE: ''hello''.'.
    _code 'ENDCASE.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.
ENDCLASS.       "lcl_Test
