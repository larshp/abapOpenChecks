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
      test001_12 FOR TESTING,
      test001_13 FOR TESTING,
      test001_14 FOR TESTING,
      test001_15 FOR TESTING,
      test001_16 FOR TESTING,
      test001_17 FOR TESTING,
      test001_18 FOR TESTING,
      test001_19 FOR TESTING,
      test001_20 FOR TESTING,
      test001_21 FOR TESTING,
      test001_22 FOR TESTING,
      test001_23 FOR TESTING,
      test001_24 FOR TESTING,
      test001_25 FOR TESTING.

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

  METHOD test001_13.

    _code '  IF 1 = ''a''. '.
    _code '    " comment   '.
    _code '    IF 1 = 2.   '.
    _code '    ENDIF.      '.
    _code '  ENDIF.        '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_14.

    _code '  IF 1 = ''a''. '.
    _code '*     comment   '.
    _code '    IF 1 = 2.   '.
    _code '    ENDIF.      '.
    _code '  ENDIF.        '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_15.

    _code '  IF 1 = ''a''. '.
    _code '*     comment1  '.
    _code '*     comment2  '.
    _code '    IF 1 = 2.   '.
    _code '    ENDIF.      '.
    _code '  ENDIF.        '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_16.

    _code '  IF 1 = " comment'.
    _code '''a''. '.
    _code '    IF 1 = 2.   '.
    _code '    ENDIF.      '.
    _code '  ENDIF.        '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_17.

    _code 'IF sy-subrc <> 0.'.
    _code '  MESSAGE ''foobar'' TYPE ''E''.'.
    _code 'ENDIF.'.
    _code 'IF 1 = 2.'.
    _code '  " blah'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_18.

    _code '  IF 1 = ''a''. '.
    _code '    IF 1 = 2.   '.
    _code '      WRITE foo.'.
    _code '      WRITE bar.'.
    _code '    ENDIF.      '.
    _code '*     comment1  '.
    _code '  ENDIF.        '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_19.

    _code 'IF 1 = 2.      '.
    _code '  WRITE foo.   '.
    _code 'ELSE.          '.
    _code '  IF 1 = 2.    '.
    _code '    WRITE foo. '.
    _code '  ELSEIF 1 = 2.'.
    _code '    WRITE foo. '.
    _code '  ENDIF.       '.
    _code '*  comment     '.
    _code 'ENDIF.         '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_20.

    _code 'IF 1 = 2.'.
    _code 'ELSEIF lv_string IS INITIAL.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_21.

    _code 'IF ws_layo-cwidth_opt IS INITIAL.'.
    _code '  IF ls_fcat-col_opt IS NOT INITIAL.'.
    _code '    ls_fieldcatalog-is_optimized = abap_true.'.
    _code '  ENDIF.'.
    _code 'ELSE.'.
    _code '  ls_fieldcatalog-is_optimized = abap_true.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_22.

    _code 'IF iv_mode = blah.'.
    _code '  lv_count = lines( lt_dates ). " sdf'.
    _code '  IF lv_count = 1. " dsf'.
    _code '    EXIT.'.
    _code '  ELSEIF lv_count > 1. " sdf'.
    _code '    WRITE foo.'.
    _code '  ENDIF.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_23.

    _code 'IF iv_mode = blah.'.
    _code '  IF lv_count = 1. " dsf'.
    _code '    EXIT.'.
    _code '  ELSEIF lv_count > 1. " sdf'.
    _code '    WRITE foo.'.
    _code '  ENDIF.'.
    _code 'ELSE.'.
    _code '  lv_count = lines( lt_dates ). " sdf'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_24.

    _code 'DATA: iv_commit  TYPE zcommit_type,'.
    _code '      lv_message TYPE string,'.
    _code '      lv_dummy_message TYPE string ##NEEDED.'.
    _code ''.
    _code 'IF iv_commit IS INITIAL.'.
    _code '  lv_message = ''foobar'' ##NO_TEXT.'.
    _code 'ENDIF.'.
    _code ''.
    _code 'IF 2 = 2.'.
    _code '  WRITE iv_commit.'.
    _code '  IF sy-subrc EQ 0.'.
    _code '    WRITE iv_commit.'.
    _code '  ELSE.'.
    _code '    WRITE iv_commit.'.
    _code '    IF sy-subrc NE 0.'.
    _code '      " blah blah'.
    _code '      WRITE iv_commit.'.
    _code '    ENDIF.'.
    _code '  ENDIF.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_25.
    _code 'IF sy-subrc EQ 0.          '.
    _code '  IF val IS NOT INITIAL.        '.
    _code '    WRITE: ''foo''.'.
    _code '  ELSEIF count IS NOT INITIAL .            '.
    _code '    WRITE: ''bar''.'.
    _code '  ENDIF.           '.
    _code 'ENDIF.             '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.

ENDCLASS.       "lcl_Test
