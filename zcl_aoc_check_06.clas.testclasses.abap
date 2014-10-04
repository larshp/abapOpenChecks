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
          mo_check  TYPE REF TO zcl_aoc_check_06.

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
    append &1 to mt_code.
  END-OF-DEFINITION.

  METHOD setup.
    CREATE OBJECT mo_check.
  ENDMETHOD.                    "setup

  METHOD test001_01.
* ===========

    _code 'write ''foo bar''.'.

    ms_result = zcl_aoc_unit_test=>check( it_code  = mt_code
                                          io_check = mo_check ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test1

  METHOD test001_02.
* ===========

    _code 'WRITE ''foobar''. '.

    ms_result = zcl_aoc_unit_test=>check( it_code  = mt_code
                                          io_check = mo_check ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test2

  METHOD test001_03.
* ===========

    _code 'DEFINE _foo.        '.
    _code '  write: ''bar''.   '.
    _code 'END-OF-DEFINITION.  '.

    ms_result = zcl_aoc_unit_test=>check( it_code  = mt_code
                                          io_check = mo_check ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test001_03

  METHOD test001_04.
* ===========

    _code 'DATA: lv_a TYPE i,    '.
    _code '      lv_b TYPE i.    '.
    _code '                      '.
    _code 'WRITE: / ''foobar''.  '.
    _code '.                     '.
    _code 'lv_a = lv_b.          '.

    ms_result = zcl_aoc_unit_test=>check( it_code  = mt_code
                                          io_check = mo_check ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

ENDCLASS.       "lcl_Test