*----------------------------------------------------------------------*
*       CLASS lcl_Test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ltcl_test definition for testing
  duration short
  risk level harmless
  final.

  private section.
* ================

    data: mt_code   type string_table,
          ms_result type scirest_ad,
          mo_check  type ref to zcl_aoc_check_101.

    methods:
      setup,
      export_import for testing,
      test001_01 for testing,
      test001_02 for testing,
      test001_03 for testing raising cx_static_check,
      test001_04 FOR TESTING RAISING cx_static_check,
      test001_05 FOR TESTING RAISING cx_static_check,
      test001_06 FOR TESTING RAISING cx_static_check.

endclass.       "lcl_Test

*----------------------------------------------------------------------*
*       CLASS lcl_Test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
class ltcl_test implementation.
* ==============================

  define _code.
    APPEND &1 TO mt_code.
  end-of-definition.

  method setup.
    create object mo_check.
    zcl_aoc_unit_test=>set_check( mo_check ).
  endmethod.                                               "setup

  method export_import.
    zcl_aoc_unit_test=>export_import( mo_check ).
  endmethod.

  method test001_01.
* ===========

    _code 'IF foo IS NOT INITIAL.'.
    _code '  foo = foo + 1.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  endmethod.                                               "test1

  method test001_02.
* ===========

    cl_abap_unit_assert=>assert_initial( ms_result ).
    _code 'IF NOT foo IS INITIAL'.
    _code '  foo = foo + 1.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  endmethod.                                               "test2

  method test001_03.
* ===========

    cl_abap_unit_assert=>assert_initial( ms_result ).
    _code 'IF foo IS NOT INITIAL AND NOT bar IS INITIAL'.
    _code '  foo = foo + 1.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  endmethod.                                               "test2

  method test001_04.
* ===========

    _code 'IF NOT ( foo IS INITIAL ).'.   "Not handled by check
    _code '  foo = foo + 1.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  endmethod.


  method test001_05.
* ===========

    _code 'IF ( NOT foo IS INITIAL ).'.   "Not handled by check
    _code '  foo = foo + 1.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).
  endmethod.


  method test001_06.
* ===========

    _code 'IF lines( foo ) > 1.'.   "Not handled by check
    _code '  bar = bar + 1.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).
  endmethod.
endclass.       "lcl_Test
