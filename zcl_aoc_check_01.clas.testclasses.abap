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
          mo_check  TYPE REF TO zcl_aoc_check_01.

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
      test001_12 FOR TESTING.

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
    zcl_aoc_unit_test=>export_import( ).
  ENDMETHOD.

  METHOD test001_01.
* ===========

    _code 'IF 1 = 2.          '.
    _code '  IF 3 = 4.        '.
    _code '    WRITE: ''foo''.'.
    _code '  ENDIF.           '.
    _code 'ENDIF.             '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test1

  METHOD test001_02.
* ===========

    _code 'IF 1 = 2 AND 3 = 4.'.
    _code '  WRITE: ''foo''.  '.
    _code 'ENDIF.             '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test2

  METHOD test001_03.
* ===========

    _code 'IF 1 = 2.          '.
    _code '  WRITE: ''bar''.  '.
    _code '  IF 3 = 4.        '.
    _code '    WRITE: ''foo''.'.
    _code '  ENDIF.           '.
    _code 'ENDIF.             '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_03

  METHOD test001_04.
* ===========

    _code 'IF 1 = 2.          '.
    _code '  WRITE: ''foo''.  '.
    _code 'ENDIF.             '.
    _code 'IF 3 = 4.          '.
    _code '  WRITE: ''bar''.  '.
    _code 'ENDIF.             '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_04

  METHOD test001_05.

    _code 'IF 1 = 2.          '.
    _code '  IF 3 = 4.        '.
    _code '    WRITE: ''foo''.'.
    _code '  ELSE.            '.
    _code '    WRITE: ''bar''.'.
    _code '  ENDIF.           '.
    _code 'ENDIF.             '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_05

  METHOD test001_06.

    _code 'IF 1 = 2.          '.
    _code '  IF 3 = 4.        '.
    _code '    WRITE: ''foo''.'.
    _code '  ENDIF.           '.
    _code '  IF 5 = 7.        '.
    _code '    WRITE: ''bar''.'.
    _code '  ENDIF.           '.
    _code 'ENDIF.             '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_06

  METHOD test001_07.

    _code 'IF 1 = 2.            '.
    _code '  WRITE: ''foo''.    '.
    _code '  IF 1 = 2.          '.
    _code '    IF 3 = 4.        '.
    _code '      WRITE: ''foo''.'.
    _code '    ENDIF.           '.
    _code '  ENDIF.             '.
    _code '  WRITE: ''foo''.    '.
    _code 'ENDIF.               '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test001_07

  METHOD test001_08.

    _code 'IF 1 = 2.          '.
    _code '  WRITE: ''foo''.  '.
    _code 'ELSE.              '.
    _code '  IF 3 = 4.        '.
    _code '    WRITE: ''foo''.'.
    _code '  ELSE.            '.
    _code '    WRITE: ''foo''.'.
    _code '  ENDIF.           '.
    _code 'ENDIF.             '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_09.

    _code 'IF 1 = 2.          '.
    _code '  WRITE: ''foo''.  '.
    _code 'ELSEIF 3 = 4.      '.
    _code '  WRITE: ''foo''.  '.
    _code 'ELSE.              '.
    _code '  WRITE: ''foo''.  '.
    _code 'ENDIF.             '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_10.

    _code 'IF 1 = 2.          '.
    _code '  WRITE: ''foo''.  '.
    _code 'ELSE.              '.
    _code '  IF 3 = 4.        '.
    _code '    WRITE: ''foo''.'.
    _code '  ELSE.            '.
    _code '    WRITE: ''foo''.'.
    _code '  ENDIF.           '.
    _code '  WRITE: ''foo''.  '.
    _code 'ENDIF.             '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_11.

    _code 'LOOP AT lt_foo ASSIGNING <ls_foo>.'.
    _code '  IF 1 = 2.'.
    _code '    CONTINUE.'.
    _code '  ENDIF.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_12.

    _code 'IF 1 = 2.'.
    _code '  IF 3 = 4.'.
    _code '    WRITE: ''foo''.'.
    _code '  ENDIF.'.
    _code 'ELSEIF 5 = 6.'.
    _code '  IF 7 = 8.'.
    _code '    WRITE: ''foo''.'.
    _code '  ENDIF.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

ENDCLASS.       "lcl_Test