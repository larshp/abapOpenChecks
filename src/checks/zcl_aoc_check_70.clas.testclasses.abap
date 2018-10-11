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
    DATA: mt_code   TYPE string_table,
          ms_result TYPE scirest_ad,
          mv_text   TYPE string,
          mo_check  TYPE REF TO zcl_aoc_check_70.

    METHODS:
      setup,
      message_handler FOR EVENT message OF zcl_aoc_check_70 IMPORTING p_param_1,
      export_import FOR TESTING,
      test001 FOR TESTING,
      test002 FOR TESTING,
      test003 FOR TESTING,
      test004 FOR TESTING,
      test005 FOR TESTING,
      test006 FOR TESTING,
      test007 FOR TESTING,
      test008 FOR TESTING,
      test009 FOR TESTING.

ENDCLASS.       "lcl_Test

*----------------------------------------------------------------------*
*       CLASS lcl_Test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_test IMPLEMENTATION.

  DEFINE _code.
    APPEND &1 TO mt_code.
  END-OF-DEFINITION.

  METHOD setup.
    CREATE OBJECT mo_check.
    SET HANDLER message_handler FOR mo_check.
    zcl_aoc_unit_test=>set_check( mo_check ).
  ENDMETHOD.                    "setup

  METHOD message_handler.
    mv_text = p_param_1.
  ENDMETHOD.

  METHOD export_import.
    zcl_aoc_unit_test=>export_import( mo_check ).
  ENDMETHOD.

  METHOD test001.
    _code 'WRITE ''foo''. "ToDo: Replace text'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = 'N'
                                        act = ms_result-kind ).
    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).
    cl_abap_unit_assert=>assert_equals( exp = 'ToDo: Replace text'
                                        act = mv_text ).
  ENDMETHOD.

  METHOD test002.
    _code 'WRITE ''foo''. " ToDo: Replace text'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = 'N'
                                        act = ms_result-kind ).
    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).
    cl_abap_unit_assert=>assert_equals( exp = 'ToDo: Replace text'
                                        act = mv_text ).
  ENDMETHOD.

  METHOD test003.
    _code 'WRITE ''foo'''.
    _code '* ToDo: insert bar'.
    _code '*WRITE ''foo'''.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = 'N'
                                        act = ms_result-kind ).
    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).
    cl_abap_unit_assert=>assert_equals( exp = 'ToDo: insert bar'
                                        act = mv_text ).
  ENDMETHOD.

  METHOD test004.
    _code 'LEAVE PROGRAM. "HACK: Leave program before crash'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = 'W'
                                        act = ms_result-kind ).
    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).
    cl_abap_unit_assert=>assert_equals( exp = 'HACK: Leave program before crash'
                                        act = mv_text ).
  ENDMETHOD.

  METHOD test005.
    _code 'DATA(LV_NUMBER) = 5 / 0. "FixMe: DIV/0'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = 'E'
                                        act = ms_result-kind ).
    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).
    cl_abap_unit_assert=>assert_equals( exp = 'FixMe: DIV/0'
                                        act = mv_text ).
  ENDMETHOD.

  METHOD test006.
    _code '* Some unrelated comment'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test007.
    _code 'IF NOT A_COMMENT IS INITIAL. ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test008.
    DATA:
      lv_attributes      TYPE xstring,
      lt_pattern_info    TYPE sci_t_regular_expressions,
      lt_pattern_warning TYPE sci_t_regular_expressions,
      lt_pattern_error   TYPE sci_t_regular_expressions.
    _code 'DATA(LV_NUMBER) = 5 / 0. " FixMe: DIV/0'.
    _code '* multiline comment'.
    _code 'DATA(LV_NUMBER2) = 5 / 0.'.

    lv_attributes = mo_check->get_attributes( ).
    IMPORT
      mt_pattern_info     = lt_pattern_info
      mt_pattern_warning  = lt_pattern_warning
      mt_pattern_error    = lt_pattern_error
      FROM DATA BUFFER lv_attributes.  "#EC CI_USE_WANTED "#EC CI_SUBRC
    EXPORT
      mv_multiline        = abap_true
      mt_pattern_info     = lt_pattern_info
      mt_pattern_warning  = lt_pattern_warning
      mt_pattern_error    = lt_pattern_error
      TO DATA BUFFER lv_attributes.
    mo_check->put_attributes( lv_attributes ).

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = 'E'
                                        act = ms_result-kind ).
    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).
    cl_abap_unit_assert=>assert_equals( exp = 'FixMe: DIV/0 multiline comment'
                                        act = mv_text ).
  ENDMETHOD.

  METHOD test009.
    DATA:
      lv_attributes      TYPE xstring,
      lt_pattern_info    TYPE sci_t_regular_expressions,
      lt_pattern_warning TYPE sci_t_regular_expressions,
      lt_pattern_error   TYPE sci_t_regular_expressions.
    _code 'DATA(LV_NUMBER) = 5 / 0. " FixMe: DIV/0'.
    _code '*'.
    _code '* NOT a multiline comment'.
    _code 'DATA(LV_NUMBER2) = 5 / 0.'.

    lv_attributes = mo_check->get_attributes( ).
    IMPORT
      mt_pattern_info     = lt_pattern_info
      mt_pattern_warning  = lt_pattern_warning
      mt_pattern_error    = lt_pattern_error
      FROM DATA BUFFER lv_attributes.  "#EC CI_USE_WANTED "#EC CI_SUBRC
    EXPORT
      mv_multiline        = abap_true
      mt_pattern_info     = lt_pattern_info
      mt_pattern_warning  = lt_pattern_warning
      mt_pattern_error    = lt_pattern_error
      TO DATA BUFFER lv_attributes.
    mo_check->put_attributes( lv_attributes ).

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = 'E'
                                        act = ms_result-kind ).
    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).
    cl_abap_unit_assert=>assert_equals( exp = 'FixMe: DIV/0'
                                        act = mv_text ).
  ENDMETHOD.

ENDCLASS.       "lcl_Test
