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
          mo_check  TYPE REF TO zcl_aoc_check_06.

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
      test001_10 FOR TESTING,
      test001_11 FOR TESTING,
      test001_12 FOR TESTING,
      test001_13 FOR TESTING,
      test001_14 FOR TESTING,
      test001_15 FOR TESTING,
      test001_16 FOR TESTING.

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

    _code 'write ''foo bar''.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test1

  METHOD test001_02.
* ===========

    _code 'WRITE ''foobar''. '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test2

  METHOD test001_03.
* ===========

    _code 'DEFINE _foo.        '.
    _code '  write: ''bar''.   '.
    _code 'END-OF-DEFINITION.  '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    IF sy-saprl >= '750'.
* 750 pretty prints code inside macro definitions
      cl_abap_unit_assert=>assert_equals( exp = '001'
                                          act = ms_result-code ).
    ELSE.
      cl_abap_unit_assert=>assert_initial( ms_result ).
    ENDIF.

  ENDMETHOD.                    "test001_03

  METHOD test001_04.
* ===========

    _code 'DATA: lv_a TYPE i,    '.
    _code '      lv_b TYPE i.    '.
    _code '                      '.
    _code 'WRITE: / ''foobar''.  '.
    _code '.                     '.
    _code 'lv_a = lv_b.          '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_04

  METHOD test001_05.
* ===========

    _code 'gt_tests[ iv_row ]-result = icon_incomplete.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_05

  METHOD test001_06.
* ===========

    IF sy-saprl < '740'.
      RETURN.
    ENDIF.

    _code 'DATA(lv_icon) = icon_incomplete.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_06

  METHOD test001_07.
* ===========

    IF sy-saprl < '740'.
      RETURN.
    ENDIF.

    _code 'data(lv_icon) = icon_incomplete.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test001_07

  METHOD test001_08.
* ===========

    _code 'lv_i = 2.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_08

  METHOD test001_09.
* ===========

    _code 'METHOD FOO.'.
    _code 'ENDMETHOD.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test001_09

  METHOD test001_10.
* ===========

    _code 'lv_long_variable_name = 2.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_10

  METHOD test001_11.
* ===========

    _code 'DEFINE _foo.        '.
    _code '  write: ''bar''.   '.
    _code 'END-OF-DEFINITION.  '.
    _code 'write: ''bar''.     '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test001_11

  METHOD test001_12.
* ===========

    _code ''. " empty

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_12

  METHOD test001_13.
* ===========

    _code 'some thing some'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_13

  METHOD test001_14.
* ===========

    _code 'FUNCTION-POOL SOME_FUNCTION'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_14

  METHOD test001_15.
* ===========

    _code 'FUNCTION SOME_FUNCTION'.
    _code 'data(var) = ''value'''.
    _code 'ENDFUNCTION.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test001_15

  METHOD test001_16.
*  ===========
    _code ''.
    _code 'CLASS lcl_test IMPLEMENTATION'.
    _code 'ENDCLASS.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.                    " test001_16
ENDCLASS.       "lcl_Test
