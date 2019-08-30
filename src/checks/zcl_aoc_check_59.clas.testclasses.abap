*----------------------------------------------------------------------*
*       CLASS lcl_Test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
* ================

    DATA: mt_code   TYPE string_table,
          ms_result TYPE scirest_ad,
          mo_check  TYPE REF TO zcl_aoc_check_59.

    METHODS:
      setup,
      export_import FOR TESTING,
      test001_01 FOR TESTING,
      test001_02 FOR TESTING,
      test001_03 FOR TESTING,
      test001_04 FOR TESTING,
      test001_05 FOR TESTING,
      test002_01 FOR TESTING,
      test002_02 FOR TESTING,
      test002_03 FOR TESTING,
      test002_04 FOR TESTING,
      test002_05 FOR TESTING,
      test002_06 FOR TESTING,
      test002_07 FOR TESTING,
      test002_08 FOR TESTING,
      test003_01 FOR TESTING,
      test003_02 FOR TESTING,
      test004_01 FOR TESTING,
      test004_02 FOR TESTING.

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
    zcl_aoc_unit_test=>set_object_type( 'CLAS' ).
  ENDMETHOD.

  METHOD export_import.
    zcl_aoc_unit_test=>export_import( mo_check ).
  ENDMETHOD.

  METHOD test001_01.

    _code 'IF foo = bar.'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test001_02.

    _code 'WRITE foobar.'.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test001_03.

    _code 'IF moo boo.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals(
      act = ms_result-code
      exp = '001' ).

  ENDMETHOD.

  METHOD test001_04.

    _code 'IF moo Z boo.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test001_05.

    _code 'ASSERT FIELDS <lv_field1> <lv_field2> CONDITION <lv_check_val> IS NOT INITIAL.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test002_01.

    _code 'IF 2 = 3.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test002_02.

    _code 'IF ( 2 = 3 ).'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals(
      act = ms_result-code
      exp = '002' ).

  ENDMETHOD.

  METHOD test002_03.

    _code 'IF 2 = 3 AND ( a = b ).'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals(
      act = ms_result-code
      exp = '002' ).

  ENDMETHOD.

  METHOD test002_04.

    _code 'IF ( 2 = 3 AND a = b ).'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals(
      act = ms_result-code
      exp = '002' ).

  ENDMETHOD.

  METHOD test002_05.

    _code 'IF 2 = 3 AND foo = boo.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test002_06.

    _code 'IF ( 2 = 3 ) AND a = b.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals(
      act = ms_result-code
      exp = '002' ).

  ENDMETHOD.

  METHOD test002_07.

    _code 'IF foo = bar AND ( moo = boo OR baa = laa ).'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test002_08.

    _code 'if moo = boo and foo = loo and ( mode z <mask> ).'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals(
      act = ms_result-code
      exp = '002' ).

  ENDMETHOD.

  METHOD test003_01.

    _code 'IF foo = bar AND moo = boo OR baa = laa.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals(
      act = ms_result-code
      exp = '003' ).

  ENDMETHOD.

  METHOD test003_02.

    _code 'IF ( foo = bar AND moo = boo ) OR baa = laa.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test004_01.

    _code 'IF SY-DATUM + 1 > + SY-DATUM.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test004_02.

    _code 'IF SY-DATUM + 1 > - SY-DATUM.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

ENDCLASS.       "lcl_Test
