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
          mo_check  TYPE REF TO zcl_aoc_check_19.

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

    _code 'DATA: lt_table TYPE foo_tt,'.
    _code '      ls_wa TYPE foo.'.
    _code 'READ TABLE lt_table INDEX 1 INTO ls_wa.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test1

  METHOD test001_02.
* ===========

    _code 'DATA: lt_table TYPE foo_tt,'.
    _code '      ls_wa TYPE foo.'.
    _code 'READ TABLE lt_table INTO ls_wa WITH KEY = ''foo''.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test001_02

  METHOD test001_03.
* ===========

    _code 'DATA: lt_table TYPE foo_tt,'.
    _code '      ls_wa TYPE foo.'.
    _code 'LOOP AT lt_table INTO ls_wa.'.
    _code 'ENDLOOP.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test001_03

  METHOD test001_04.
* ===========

    _code 'DATA: lt_table TYPE foo_tt.'.
    _code 'FIELD-SYMBOLS: <ls_wa> TYPE foo.'.
    _code 'READ TABLE lt_table INDEX 1 ASSIGNING <ls_wa>.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test001_04

  METHOD test001_05.
* ===========

    _code 'DATA: lt_table TYPE foo_tt.'.
    _code 'FIELD-SYMBOLS: <ls_wa> TYPE foo.'.
    _code 'READ TABLE lt_table ASSIGNING <ls_wa> WITH KEY = ''foo''.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test001_05

  METHOD test001_06.
* ===========

    _code 'DATA: lt_table TYPE foo_tt.'.
    _code 'FIELD-SYMBOLS: <ls_wa> TYPE foo.'.
    _code 'LOOP AT lt_table ASSIGNING <ls_wa>.'.
    _code 'ENDLOOP.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test001_06

  METHOD test001_07.
* ===========

    _code 'DATA: lt_table TYPE foo_tt.'.
    _code 'FIELD-SYMBOLS: <ls_wa> TYPE foo.'.
    _code 'APPEND INITIAL LINE TO lt_table ASSIGNING <ls_wa>.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test001_07

  METHOD test001_08.
* ===========

    _code 'DATA: lt_table TYPE string_tt,'.
    _code '      ls_wa LIKE LINE OF lt_table.'.
    _code 'READ TABLE lt_table INDEX 1 INTO ls_wa.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test1

  METHOD test001_09.
* ===========

    _code 'DATA: lt_table TYPE string_tt,'.
    _code '      ls_wa LIKE LINE OF lt_table.'.
    _code 'READ TABLE lt_table INTO ls_wa WITH KEY = ''foo''.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_02

  METHOD test001_10.
* ===========

    _code 'DATA: lt_table TYPE string_tt,'.
    _code '      ls_wa LIKE LINE OF lt_table.'.
    _code 'LOOP AT lt_table INTO ls_wa.'.
    _code 'ENDLOOP.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_03

  METHOD test001_11.
* ===========

    _code 'DATA: lt_table TYPE string_tt.'.
    _code 'FIELD-SYMBOLS: <ls_wa> LIKE LINE OF lt_table.'.
    _code 'READ TABLE lt_table INDEX 1 ASSIGNING <ls_wa>.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_04

  METHOD test001_12.
* ===========

    _code 'DATA: lt_table TYPE string_tt.'.
    _code 'FIELD-SYMBOLS: <ls_wa> LIKE LINE OF lt_table.'.
    _code 'READ TABLE lt_table ASSIGNING <ls_wa> WITH KEY = ''foo''.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_05

  METHOD test001_13.
* ===========

    _code 'DATA: lt_table TYPE string_tt.'.
    _code 'FIELD-SYMBOLS: <ls_wa> LIKE LINE OF lt_table.'.
    _code 'LOOP AT lt_table ASSIGNING <ls_wa>.'.
    _code 'ENDLOOP.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_06

  METHOD test001_14.
* ===========

    _code 'DATA: lt_table TYPE string_tt.'.
    _code 'FIELD-SYMBOLS: <ls_wa> LIKE LINE OF lt_table.'.
    _code 'APPEND INITIAL LINE TO lt_table ASSIGNING <ls_wa>.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_14

  METHOD test001_15.
* ===========

    _code 'FIELD-SYMBOLS:'.
    _code '  <ls_wa>  LIKE LINE OF lt_foo1,'.
    _code '  <ls_wa2> LIKE LINE OF lt_foo2.'.
    _code 'READ TABLE lt_list[] ASSIGNING <ls_wa> INDEX 1.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_14

  METHOD test001_16.
* ===========

    _code 'FIELD-SYMBOLS: <ls_wa> TYPE any.'.
    _code 'READ TABLE lt_list[] ASSIGNING <ls_wa> INDEX 1.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_16

  METHOD test001_17.
* ===========

    _code 'FIELD-SYMBOLS: <lv_wa> TYPE i.'.
    _code 'READ TABLE lt_list[] ASSIGNING <lv_wa> INDEX 1.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_18.
* ===========

    _code 'FIELD-SYMBOLS: <lo_wa> TYPE REF TO cl_foobar.'.
    _code 'READ TABLE lt_list[] ASSIGNING <lo_wa> INDEX 1.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_19.
* ===========

    _code 'DATA: ls_token TYPE stokesx.'.
    _code 'LOOP AT io_tokens->get_tokens( ) INTO ls_token.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

ENDCLASS.       "lcl_Test
