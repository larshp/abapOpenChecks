CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
    DATA: mt_code   TYPE string_table,
          ms_result TYPE scirest_ad,
          mo_check  TYPE REF TO zcl_aoc_check_99.

    METHODS:
      setup,
      export_import FOR TESTING,
      one_when_with_code FOR TESTING,
      two_cases_second_fails FOR TESTING,
      two_whens_first_blank FOR TESTING,
      two_whens_first_with_code FOR TESTING,
      two_whens_second_with_code FOR TESTING,
      two_whens_with_code FOR TESTING.

ENDCLASS.

CLASS ltcl_test IMPLEMENTATION.

  DEFINE _code.
    APPEND &1 TO mt_code.
  END-OF-DEFINITION.

  METHOD setup.
    CREATE OBJECT mo_check.
    zcl_aoc_unit_test=>set_check( mo_check ).
  ENDMETHOD.

  METHOD export_import.
    zcl_aoc_unit_test=>export_import( mo_check ).
  ENDMETHOD.

  METHOD one_when_with_code.
    " Only one WHEN with code => fail

    " given
    _code 'FORM foo.'.
    _code '  DATA lv_bar TYPE i.'.
    _code '  CASE lv_bar.'.
    _code '    WHEN 1.'.
    _code '      WRITE: / ''hello world''.'.
    _code '  ENDCASE.'.
    _code 'ENDFORM.'.

    " when
    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    " then
    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD two_cases_second_fails.
    " Two CASEs:
    " CASE 1: Two WHENs, both containing code => OK
    " CASE 2: Two WHENs, but only the first contains code => fail

    " given
    _code 'FORM foo.'.
    _code '  DATA lv_bar TYPE i.'.
    _code '  CASE lv_bar.'.
    _code '    WHEN 1.'.
    _code '      WRITE: / ''hello foo''.'.
    _code '    WHEN OTHERS.'.
    _code '      WRITE: / ''hello bar''.'.
    _code '  ENDCASE.'.
    _code ''.
    _code '  CASE lv_bar.'.
    _code '    WHEN 1.'.
    _code '      WRITE: / ''hello world''.'.
    _code '    WHEN OTHERS.'.
    _code '      " do nothing'.
    _code '  ENDCASE.'.
    _code 'ENDFORM.'.

    " when
    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    " then
    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD two_whens_first_blank.
    " Two WHENs, but the first only contains a blank line => fail

    " given
    _code 'FORM foo.'.
    _code '  DATA lv_bar TYPE i.'.
    _code '  CASE lv_bar.'.
    _code '    WHEN 1.'.
    _code ''.
    _code '    WHEN OTHERS.'.
    _code '      WRITE: / ''hello world''.'.
    _code '  ENDCASE.'.
    _code 'ENDFORM.'.

    " when
    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    " then
    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD two_whens_first_with_code.
    " Two WHENs, but only the first contains code => fail

    " given
    _code 'FORM foo.'.
    _code '  DATA lv_bar TYPE i.'.
    _code '  CASE lv_bar.'.
    _code '    WHEN 1.'.
    _code '      WRITE: / ''hello world''.'.
    _code '    WHEN OTHERS.'.
    _code '      " do nothing'.
    _code '  ENDCASE.'.
    _code 'ENDFORM.'.

    " when
    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    " then
    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD two_whens_second_with_code.
    " Two WHENs, but only the second contains code => fail

    " given
    _code 'FORM foo.'.
    _code '  DATA lv_bar TYPE i.'.
    _code '  CASE lv_bar.'.
    _code '    WHEN 1.'.
    _code '      " do nothing'.
    _code '    WHEN OTHERS.'.
    _code '      WRITE: / ''hello world''.'.
    _code '  ENDCASE.'.
    _code 'ENDFORM.'.

    " when
    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    " then
    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).
  ENDMETHOD.


  METHOD two_whens_with_code.
    " Two WHENs, both containing code => ok

    " given
    _code 'FORM foo.'.
    _code '  DATA lv_bar TYPE i.'.
    _code '  CASE lv_bar.'.
    _code '    WHEN 1.'.
    _code '      WRITE: / ''hello foo''.'.
    _code '    WHEN OTHERS.'.
    _code '      WRITE: / ''hello bar''.'.
    _code '  ENDCASE.'.
    _code 'ENDFORM.'.

    " when
    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    " then
    cl_abap_unit_assert=>assert_initial( ms_result-code ).
  ENDMETHOD.

ENDCLASS.
