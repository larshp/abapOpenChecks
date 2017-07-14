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
          mo_check  TYPE REF TO zcl_aoc_check_25.

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
      test001_09 FOR TESTING.

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

    _code 'PARAMETERS: p_foo TYPE i OBLIGATORY.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test1

  METHOD test001_02.
* ===========

    _code 'PARAMETERS: p_foo TYPE c OBLIGATORY.'.
    _code 'WRITE p_foo.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test2

  METHOD test001_03.
* ===========

    _code 'SELECT-OPTIONS: s_foo FOR zbar-moo.'.
    _code 'lt_moo = s_foo[].'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_03

  METHOD test001_04.
* ===========

    _code 'SELECT-OPTIONS: s_foo2 FOR zmoo-field1 MEMORY ID zmem,'.
    _code '                s_foo1 FOR zmoo-field2.'.
    _code 'lt_moo = s_foo1[].'.
    _code 'lt_boo = s_foo2[].'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_03

  METHOD test001_05.
* ===========

    _code 'SELECT-OPTIONS: s_foo2 FOR zmoo-field1 MEMORY ID zmem,'.
    _code '                s_foo1 FOR zmoo-field2.'.
    _code 'gt_result = zcl_class=>method( it_foo1 = s_foo1[]'.
    _code '                               it_foo2 = s_foo2[] ).'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_04

  METHOD test001_06.
* ===========

    _code 'PARAMETERS : p_per(3) TYPE c OBLIGATORY.'.
    _code 'WRITE p_per+2.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_06

  METHOD test001_07.
* ===========

    _code 'PARAMETERS : p_per(3) TYPE c OBLIGATORY.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test001_07

  METHOD test001_08.
* ===========

    _code 'SELECT-OPTIONS: s_obj FOR tadir-object.'.
    _code 'SELECT * FROM tadir INTO TABLE @DATA(lt_tadir)'.
    _code '  WHERE object IN @s_obj.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_09.
* ===========

    _code 'PARAMETERS: p_form TYPE text10.'.
    _code 'PERFORM (p_form) IN PROGRAM zfoobar.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

ENDCLASS.       "lcl_Test

CLASS ltcl_strip DEFINITION DEFERRED.
CLASS zcl_aoc_check_25 DEFINITION LOCAL FRIENDS ltcl_strip.

CLASS ltcl_strip DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.

    DATA: mo_check TYPE REF TO zcl_aoc_check_25.

    METHODS:
      setup,
      test01 FOR TESTING,
      test02 FOR TESTING,
      test03 FOR TESTING,
      test04 FOR TESTING,
      test05 FOR TESTING,
      test06 FOR TESTING.

ENDCLASS.       "ltcl_Strip

CLASS ltcl_strip IMPLEMENTATION.

  METHOD setup.
    CREATE OBJECT mo_check.
  ENDMETHOD.

  METHOD test01.
    DATA: lv_result TYPE string.

    lv_result = mo_check->strip( 'FOO' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'FOO' ).
  ENDMETHOD.

  METHOD test02.
    DATA: lv_result TYPE string.

    lv_result = mo_check->strip( 'FOO+2' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'FOO' ).
  ENDMETHOD.

  METHOD test03.
    DATA: lv_result TYPE string.

    lv_result = mo_check->strip( '@FOO' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'FOO' ).
  ENDMETHOD.

  METHOD test04.
    DATA: lv_result TYPE string.

    lv_result = mo_check->strip( 'FOO[]' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'FOO' ).
  ENDMETHOD.

  METHOD test05.
    DATA: lv_result TYPE string.

    lv_result = mo_check->strip( '(FOO)' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'FOO' ).
  ENDMETHOD.

  METHOD test06.
    DATA: lv_result TYPE string.

    lv_result = mo_check->strip( 'F' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'F' ).
  ENDMETHOD.

ENDCLASS.
