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

    DATA: mt_code   TYPE string_table,
          ms_result TYPE scirest_ad,
          mo_check  TYPE REF TO zcl_aoc_check_25.

    METHODS: setup,
             test001_01 FOR TESTING,
             test001_02 FOR TESTING,
             test001_03 FOR TESTING,
             test001_04 FOR TESTING,
             test001_05 FOR TESTING.

ENDCLASS.       "lcl_Test

*----------------------------------------------------------------------*
*       CLASS lcl_Test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test IMPLEMENTATION.
* ==============================

  DEFINE _code.
    append &1 to mt_code.
  END-OF-DEFINITION.

  METHOD setup.
    CREATE OBJECT mo_check.
  ENDMETHOD.                    "setup

  METHOD test001_01.
* ===========

    _code 'PARAMETERS: p_foo TYPE i OBLIGATORY.'.

    ms_result = zcl_aoc_unit_test=>check( it_code  = mt_code
                                          io_check = mo_check ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test1

  METHOD test001_02.
* ===========

    _code 'PARAMETERS: p_foo TYPE c OBLIGATORY.'.
    _code 'WRITE p_foo.'.

    ms_result = zcl_aoc_unit_test=>check( it_code  = mt_code
                                          io_check = mo_check ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test2

  METHOD test001_03.
* ===========

    _code 'SELECT-OPTIONS: s_foo FOR zbar-moo.'.
    _code 'lt_moo = s_foo[].'.

    ms_result = zcl_aoc_unit_test=>check( it_code  = mt_code
                                          io_check = mo_check ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_03

  METHOD test001_04.
* ===========

    _code 'SELECT-OPTIONS: s_foo2 FOR zmoo-field1 MEMORY ID zmem,'.
    _code '                s_foo1 FOR zmoo-field2.'.
    _code 'lt_moo = s_foo1[].'.
    _code 'lt_boo = s_foo2[].'.

    ms_result = zcl_aoc_unit_test=>check( it_code  = mt_code
                                          io_check = mo_check ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_03

  METHOD test001_05.
* ===========

    _code 'SELECT-OPTIONS: s_foo2 FOR zmoo-field1 MEMORY ID zmem,'.
    _code '                s_foo1 FOR zmoo-field2.'.
    _code 'gt_result = zcl_class=>method( it_foo1 = s_foo1[]'.
    _code '                               it_foo2 = s_foo2[] ).'.

    ms_result = zcl_aoc_unit_test=>check( it_code  = mt_code
                                          io_check = mo_check ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_04

* stuff like:
*PARAMETERS : p_per(3) TYPE c OBLIGATORY.
* should use LENGTH 3 instead

ENDCLASS.       "lcl_Test