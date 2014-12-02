*"* use this source file for any macro definitions you need
*"* in the implementation part of the class

  DEFINE _code.
    append &1 to mt_code.
  END-OF-DEFINITION.

  DEFINE _test.
    ms_result = zcl_aoc_parser=>run( it_code  = mt_code
                                    iv_debug = mv_debug ).
    cl_abap_unit_assert=>assert_equals( exp = &1
                                        act = ms_result-match ).
  END-OF-DEFINITION.