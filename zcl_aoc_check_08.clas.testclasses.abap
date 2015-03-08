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
          mo_check  TYPE REF TO zcl_aoc_check_08.

    METHODS: setup.

    METHODS:
      test001_01 FOR TESTING,
      test001_02 FOR TESTING.

    METHODS:
      test002_01 FOR TESTING,
      test002_02 FOR TESTING,
      test002_03 FOR TESTING.

    METHODS:
      test003_01 FOR TESTING,
      test003_02 FOR TESTING.

    METHODS:
      test004_01 FOR TESTING.

    METHODS:
      test005_01 FOR TESTING.

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

    _code 'REFRESH lt_foobar.'.

    ms_result = zcl_aoc_unit_test=>check( it_code  = mt_code
                                          io_check = mo_check ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test1

  METHOD test001_02.
* ===========

    _code 'CLEAR lt_foobar[].'.

    ms_result = zcl_aoc_unit_test=>check( it_code  = mt_code
                                          io_check = mo_check ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test2

  METHOD test002_01.
* ===========

    _code 'IF iv_input IS REQUESTED. '.
    _code '  WRITE ''foo''.          '.
    _code 'ENDIF.                    '.

    ms_result = zcl_aoc_unit_test=>check( it_code  = mt_code
                                          io_check = mo_check ).

    cl_abap_unit_assert=>assert_equals( exp = '002'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test1

  METHOD test002_02.
* ===========

    _code 'IF iv_input IS SUPPLIED.  '.
    _code '  WRITE ''foo''.          '.
    _code 'ENDIF.                    '.
    ms_result = zcl_aoc_unit_test=>check( it_code  = mt_code
                                          io_check = mo_check ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test2

  METHOD test002_03.
* ===========

    _code 'WRITE: / ''foo IS REQUESTED bar'''.
    ms_result = zcl_aoc_unit_test=>check( it_code  = mt_code
                                          io_check = mo_check ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test003_01.
* ===========

    _code 'LEAVE.'.

    ms_result = zcl_aoc_unit_test=>check( it_code  = mt_code
                                          io_check = mo_check ).

    cl_abap_unit_assert=>assert_equals( exp = '003'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test1

  METHOD test003_02.
* ===========

    _code 'LEAVE LIST-PROCESSING.'.
    ms_result = zcl_aoc_unit_test=>check( it_code  = mt_code
                                          io_check = mo_check ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test2

  METHOD test004_01.
* ===========

    _code 'COMPUTE lv_foo = lv_bar.'.

    ms_result = zcl_aoc_unit_test=>check( it_code  = mt_code
                                          io_check = mo_check ).

    cl_abap_unit_assert=>assert_equals( exp = '004'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test005_01.
* ===========

    _code 'MOVE lv_foo TO lv_bar.'.

    ms_result = zcl_aoc_unit_test=>check( it_code  = mt_code
                                          io_check = mo_check ).

    cl_abap_unit_assert=>assert_equals( exp = '005'
                                        act = ms_result-code ).

  ENDMETHOD.

ENDCLASS.       "lcl_Test