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
          lo_check  TYPE REF TO zcl_aoc_check_02.

    METHODS: setup,
             test001_01 FOR TESTING,
             test001_02 FOR TESTING,
             test001_03 FOR TESTING,
             test001_04 FOR TESTING.

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

    _code 'EXIT.'.

    ls_result = zcl_aoc_unit_test=>check( it_code  = lt_code
                                          io_check = lo_check ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ls_result-code ).

  ENDMETHOD.                    "test1

  METHOD test001_02.
* ===========

    _code 'LOOP AT lt_table INTO lv_structure.'.
    _code '  EXIT.                            '.
    _code 'ENDLOOP.                           '.

    ls_result = zcl_aoc_unit_test=>check( it_code  = lt_code
                                          io_check = lo_check ).

    cl_abap_unit_assert=>assert_initial( ls_result ).

  ENDMETHOD.                    "test2

  METHOD test001_03.
* ===========

    _code 'DO.      '.
    _code '  EXIT.  '.
    _code 'ENDDO.   '.

    ls_result = zcl_aoc_unit_test=>check( it_code  = lt_code
                                          io_check = lo_check ).

    cl_abap_unit_assert=>assert_initial( ls_result ).

  ENDMETHOD.                    "test001_03

  METHOD test001_04.
* ===========

    _code 'WHILE 1 = 2'.
    _code '  EXIT.    '.
    _code 'ENDWHILE.  '.

    ls_result = zcl_aoc_unit_test=>check( it_code  = lt_code
                                          io_check = lo_check ).

    cl_abap_unit_assert=>assert_initial( ls_result ).

  ENDMETHOD.                    "test001_04

ENDCLASS.       "lcl_Test