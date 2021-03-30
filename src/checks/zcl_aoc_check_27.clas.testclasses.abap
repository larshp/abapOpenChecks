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
          mo_check  TYPE REF TO zcl_aoc_check_27.

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
      test002_01 FOR TESTING,
      test002_02 FOR TESTING,
      test002_03 FOR TESTING,
      test002_04 FOR TESTING,
      test003_01 FOR TESTING,
      test003_02 FOR TESTING,
      test003_03 FOR TESTING,
      test003_04 FOR TESTING,
      test003_05 FOR TESTING,
      test003_06 FOR TESTING,
      test003_07 FOR TESTING,
      test003_08 FOR TESTING.

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

    _code 'FORM foo.'.
    _code '  lv_bar = lv_moo.'.
    _code '  RETURN.'.
    _code 'ENDFORM.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test1

  METHOD test001_02.
* ===========

    _code 'UPDATE ztable SET foo = ''bar''.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test2

  METHOD test001_03.
* ===========

    _code 'FORM foo.'.
    _code '  RETURN.'.
    _code 'ENDFORM.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test2

  METHOD test001_04.
* ===========

    _code 'FORM foo.'.
    _code '  IF lv_foo = lv_bar.'.
    _code '    RETURN.'.
    _code '  ENDIF.'.
    _code 'ENDFORM.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test001_04

  METHOD test001_05.
* ===========

    _code 'FORM foo.'.
    _code '  TRY.'.
    _code '    CATCH cx_static_check.'.
    _code '      RETURN.'.
    _code '  ENDTRY.'.
    _code 'ENDFORM.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test001_05

  METHOD test001_06.
* ===========

    _code 'FORM foo.'.
    _code '*  some comment'.
    _code '  RETURN.'.
    _code 'ENDFORM.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_06

  METHOD test001_07.
* ===========

    _code 'FORM foo.'.
    _code '  TRY.'.
    _code '    CATCH cx_static_check.'.
    _code '      RETURN.'.
    _code '* somme comment'.
    _code '  ENDTRY.'.
    _code 'ENDFORM.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test002_01.
* ===========

    _code 'FORM foo USING pv_foo.'.
    _code '  CLEAR pv_foo.'.
    _code 'ENDFORM.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test002_02.
* ===========

    _code 'FORM foo.'.
    _code '  DATA bar.'.
    _code '  CLEAR bar.'.
    _code 'ENDFORM.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '002'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test002_03.
* ===========


    _code 'DATA bar.'.
    _code 'FORM foo.'.
    _code '  moo = boo.'.
    _code '  CLEAR bar.'.
    _code 'ENDFORM.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test002_04.
* ===========

    _code 'FORM foo.'.
    _code '  DATA(bar) = `ABCD`.'.
    _code '  CLEAR bar.'.
    _code 'ENDFORM.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '002'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test003_01.
* ===========

    _code 'FORM foo.'.
    _code '  WRITE bar.'.
    _code '  CHECK foo = bar.'.
    _code 'ENDFORM.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '003'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test003_02.
    _code 'FORM foo.'.
    _code '  lv_bar = lv_moo.'.
    _code '  EXIT.'.
    _code 'ENDFORM.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '003'
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD test003_03.
    _code 'FORM foo.'.
    _code '  EXIT.'.
    _code 'ENDFORM.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.

  METHOD test003_04.
    _code 'FORM foo.'.
    _code '  IF lv_foo = lv_bar.'.
    _code '    EXIT.'.
    _code '  ENDIF.'.
    _code 'ENDFORM.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '003'
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD test003_05.
    _code 'FORM foo.'.
    _code '  TRY.'.
    _code '    CATCH cx_static_check.'.
    _code '      EXIT.'.
    _code '  ENDTRY.'.
    _code 'ENDFORM.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '003'
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD test003_06.
    _code 'FORM foo.'.
    _code '*  some comment'.
    _code '  EXIT.'.
    _code 'ENDFORM.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.

  METHOD test003_07.
    _code 'FORM foo.'.
    _code '  TRY.'.
    _code '    CATCH cx_static_check.'.
    _code '      EXIT.'.
    _code '* somme comment'.
    _code '  ENDTRY.'.
    _code 'ENDFORM.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '003'
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD test003_08.
    _code 'FORM foo.'.
    _code '  IF lv_foo = lv_bar.'.
    _code '    EXIT FROM STEP-LOOP.'.
    _code '  ENDIF.'.
    _code 'ENDFORM.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.

ENDCLASS.       "lcl_Test
