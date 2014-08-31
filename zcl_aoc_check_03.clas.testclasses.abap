*----------------------------------------------------------------------*
*       CLASS lcl_Test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
* ================

    DATA: lt_code   TYPE string_table,
          ls_result TYPE scirest_ad,
          lo_check  TYPE REF TO zcl_aoc_check_03.

    METHODS: setup,
             test001_01 FOR TESTING,
             test001_02 FOR TESTING,
             test001_03 FOR TESTING.

ENDCLASS.       "lcl_Test

*----------------------------------------------------------------------*
*       CLASS lcl_Test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test IMPLEMENTATION.
* ==============================

  DEFINE _code.
    append &1 to lt_code.
  END-OF-DEFINITION.

  METHOD setup.
    CREATE OBJECT lo_check.
  ENDMETHOD.                    "setup

  METHOD test001_01.
* ===========

    _code 'TRY.             '.
    _code '  WRITE ''foo''. '.
    _code 'ENDTRY.          '.

    ls_result = zcl_aoc_unit_test=>check( it_code  = lt_code
                                          io_check = lo_check ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ls_result-code ).

  ENDMETHOD.                    "test1

  METHOD test001_02.
* ===========

    _code 'TRY.               '.
    _code '    WRITE ''foo''. '.
    _code '  CATCH cx_root.   '.
    _code '    WRITE ''bar''. '.
    _code 'ENDTRY.            '.

    ls_result = zcl_aoc_unit_test=>check( it_code  = lt_code
                                          io_check = lo_check ).

    cl_abap_unit_assert=>assert_initial( ls_result ).

  ENDMETHOD.                    "test2

  METHOD test001_03.
* ===========

    _code 'TRY.                 '.
    _code '  WRITE ''foo''.     '.
    _code '  TRY.               '.
    _code '      WRITE ''foo''. '.
    _code '    CATCH cx_root.   '.
    _code '      WRITE ''bar''. '.
    _code '  ENDTRY.            '.
    _code 'ENDTRY.              '.

    ls_result = zcl_aoc_unit_test=>check( it_code  = lt_code
                                          io_check = lo_check ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ls_result-code ).

  ENDMETHOD.

ENDCLASS.       "lcl_Test