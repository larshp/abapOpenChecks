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
          mo_check  TYPE REF TO zcl_aoc_check_22.

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
      test001_12 FOR TESTING,
      test001_13 FOR TESTING,
      test001_14 FOR TESTING,
      test001_15 FOR TESTING,
      test001_16 FOR TESTING,
      test001_17 FOR TESTING,
      test001_18 FOR TESTING,
      test001_19 FOR TESTING.

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

    _code 'IF lv_foo = lv_bar.'.
    _code '  lv_moo = abap_true.'.
    _code '  WRITE: / lv_moo.'.
    _code 'ELSE.'.
    _code '  lv_moo = abap_true.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test1

  METHOD test001_02.
* ===========

    _code 'lv_moo = abap_true.'.
    _code 'IF lv_foo = lv_bar.'.
    _code '  WRITE: / lv_moo.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.                    "test001_02

  METHOD test001_03.
* ===========

    _code 'IF lv_foo = lv_bar.'.
    _code '  WRITE: / lv_moo.'.
    _code '  lv_moo = abap_true.'.
    _code 'ELSE.'.
    _code '  lv_moo = abap_true.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test001_03

  METHOD test001_04.
* ===========

    _code 'IF lv_foo = lv_bar.'.
    _code '  WRITE: / lv_moo.'.
    _code '  lv_moo = abap_true.'.
    _code 'ELSEIF lv_moo = lv_boo.'.
    _code '  lv_moo = abap_false.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.                    "test001_04

  METHOD test001_05.
* ===========

    _code 'IF lv_foo = lv_bar.'.
    _code '  WRITE: / lv_moo.'.
    _code '  IF 1 = 2.'.
    _code '    WRITE: / lv_text.'.
    _code '  ENDIF.'.
    _code 'ELSE.'.
    _code '  IF 1 = 2.'.
    _code '    WRITE: / lv_bar.'.
    _code '  ENDIF.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.                    "test001_05

  METHOD test001_06.
* ===========

    _code '  IF 1 = 2.'.
    _code '    WRITE: / lv_text.'.
    _code '  ENDIF.'.
    _code '  IF 1 = 2.'.
    _code '    WRITE: / lv_bar.'.
    _code '  ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.                    "test001_06

  METHOD test001_07.
* ===========

    _code 'IF 1 = 2.'.
    _code '  WRITE: / lv_text.'.
    _code 'ENDIF.'.
    _code 'IF 1 = 2.'.
    _code '  WRITE: / lv_bar.'.
    _code 'ELSE.'.
    _code '  WRITE: / lv_bar.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test001_07

  METHOD test001_08.
* ===========

    _code 'IF 1 = 2.'.
    _code '  WRITE: / lv_text.'.
    _code 'ENDIF.'.
    _code 'IF 1 = 2.'.
    _code '  WRITE: / lv_bar.'.
    _code 'ELSEIF lv_boo = 5.'.
    _code '  WRITE: / lv_bar.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.                    "test001_08

  METHOD test001_09.
* ===========

    _code 'CASE lv_foo.'.
    _code '  WHEN ''a''.'.
    _code '    lv_moo = lv_boo.'.
    _code '  WHEN ''b''.'.
    _code '    lv_moo = lv_boo.'.
    _code 'ENDCASE.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.                    "test001_08

  METHOD test001_10.
* ===========

    _code 'CASE lv_foo.'.
    _code '  WHEN ''a''.'.
    _code '    lv_moo = lv_boo.'.
    _code '  WHEN OTHERS.'.
    _code '    lv_moo = lv_boo.'.
    _code 'ENDCASE.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test001_10

  METHOD test001_12.
* ===========

    _code 'IF lv_a = lv_b.'.
    _code '  lv_foo = lv_bar.'.
    _code 'ELSE.'.
    _code '  IF lv_c = ''12''.'.
    _code '    lv_foo = lv_bar.'.
    _code '  ELSE.'.
    _code '    lv_foo = lv_bar.'.
    _code '  ENDIF.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test001_12

  METHOD test001_13.
* ===========

    _code 'IF lv_a = lv_b.'.
    _code '  IF lv_c = ''14''.'.
    _code '    lv_foo = lv_bar.'.
    _code '  ENDIF.'.
    _code 'ELSE.'.
    _code '  IF lv_c = ''12''.'.
    _code '    lv_foo = lv_bar.'.
    _code '  ENDIF.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.                    "test001_13

  METHOD test001_14.
* ===========

    _code 'IF lv_a = lv_b.'.
    _code '  IF lv_c = ''14''.'.
    _code '    lv_asdf = lv_bar.'.
    _code '  ENDIF.'.
    _code 'ELSE.'.
    _code '  IF lv_c = ''14''.'.
    _code '    lv_foo = lv_bar.'.
    _code '  ENDIF.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test001_15.
* ===========

    _code 'DATA gv_c TYPE c.'.
    _code 'IF gv_c <> '''''.
    _code '  WRITE ''filled'' ##NO_TEXT.'.
    _code 'ELSE.'.
    _code '  WRITE ''else'' ##NO_TEXT.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test001_16.
* ===========

    _code 'CASE lv_bar.'.
    _code '  WHEN ''A''.'.
    _code '    WRITE ''sdf''.'.
    _code '    CASE lv_button.'.
    _code '      WHEN 1.'.
    _code '        WRITE ''hgf''.'.
    _code '      WHEN 2 OR 3.'.
    _code '        WRITE ''ytyu''.'.
    _code '      WHEN OTHERS.'.
    _code '        ASSERT 1 = 1 + 1.'.
    _code '    ENDCASE.'.
    _code 'ENDCASE.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test001_17.
* ===========

    _code ''. " empty

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test001_18.
* ===========
    _code 'CASE mv_mode.'.
    _code '  WHEN c_change.'.
    _code '    change_group( ).'.
    _code '    li_tr_mgr->commit( ).'.
    _code '  WHEN c_create.'.
    _code '    create_group( ).'.
    _code '    li_tr_mgr->commit( ).'.
    _code 'ENDCASE.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test001_19.
* ===========

    _code 'IF NOT mooo IS INITIAL.'.
    _code '  CLEAR diadr.'.
    _code '  IF sy-subrc <> 0.'.
    _code '    WRITE ''hello''.'.
    _code '  ELSEIF 3 = 2.'.
    _code '    WRITE ''world''.'.
    _code '  ELSE.'.
    _code '    MOVE foo TO bar.'.
    _code '  ENDIF.'.
    _code 'ELSE.'.
    _code '  CLEAR moo.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

ENDCLASS.       "lcl_Te
