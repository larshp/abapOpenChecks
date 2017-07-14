*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

DEFINE _code.
  APPEND &1 TO mt_code.
END-OF-DEFINITION.

DEFINE _test.
  ms_result = zcl_aoc_parser=>run( it_code  = mt_code
                                   iv_debug = mv_debug ).
  cl_abap_unit_assert=>assert_equals( exp = &1
                                      act = ms_result-match ).
  IF ms_result-match = abap_true.
    cl_abap_unit_assert=>assert_not_initial( ms_result-tokens ).
  ELSE.
    cl_abap_unit_assert=>assert_initial( ms_result-tokens ).
  ENDIF.
END-OF-DEFINITION.
