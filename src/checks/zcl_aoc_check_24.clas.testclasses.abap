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
          mo_check  TYPE REF TO zcl_aoc_check_24.

    METHODS:
      setup,
      export_import FOR TESTING,
      test001_01 FOR TESTING,
      test001_02 FOR TESTING,
      test001_03 FOR TESTING.

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

    _code 'CHECK 1 = 2.'.
    _code 'CHECK 2 = 2.'.
    _code 'CHECK 3 = 2.'.
    _code 'CHECK 4 = 2.'.
    _code 'CHECK 5 = 2.'.
    _code 'CHECK 6 = 2.'.
    _code 'CHECK 7 = 2.'.
    _code 'CHECK 8 = 2.'.
    _code 'CHECK 9 = 2.'.
    _code 'CHECK 10 = 2.'.

* add all code again
    APPEND LINES OF mt_code TO mt_code.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test1

  METHOD test001_02.
* ===========

    _code 'LOOP AT lt_table INTO lv_structure.'.
    _code '  CHECK 1 = 2.                     '.
    _code 'ENDLOOP.                           '.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test2

  METHOD test001_03.
* ===========

    _code 'METHOD zif_foo~bar'.
    _code 'RETURN.'.
    _code 'ENDMETHOD.'.
    _code 'METHOD zif_foo~moo'.
    _code 'RETURN.'.
    _code 'ENDMETHOD.'.
    _code 'METHOD zif_foo~boo'.
    _code 'RETURN.'.
    _code 'ENDMETHOD.'.
    _code 'METHOD zif_foo~bah'.
    _code 'RETURN.'.
    _code 'ENDMETHOD.'.

* add all code again
    APPEND LINES OF mt_code TO mt_code.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

ENDCLASS.       "lcl_Test
