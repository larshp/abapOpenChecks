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
* ================

    DATA: mt_code   TYPE string_table,
          ms_result TYPE scirest_ad,
          mo_check  TYPE REF TO zcl_aoc_check_30.

    METHODS:
      setup,
      export_import FOR TESTING,
      test001_01 FOR TESTING,
      test001_02 FOR TESTING,
      test001_03 FOR TESTING,
      test001_04 FOR TESTING,
      test001_05 FOR TESTING,
      test001_06 FOR TESTING,
      test001_07 FOR TESTING,
      test001_08 FOR TESTING,
      test001_09 FOR TESTING,
      test001_10 FOR TESTING.

ENDCLASS.       "lcl_Test

*----------------------------------------------------------------------*
*       CLASS lcl_Test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_test IMPLEMENTATION.
* ==============================

  DEFINE _code.
    APPEND &1 TO mt_code.
  END-OF-DEFINITION.

  METHOD setup.
    CREATE OBJECT mo_check.
    zcl_aoc_unit_test=>set_check( mo_check ).
  ENDMETHOD.                    "setup

  METHOD export_import.
    zcl_aoc_unit_test=>export_import( mo_check ).
  ENDMETHOD.

  METHOD test001_01.
* ===========

    _code 'lx_error->to_fpm_error('.
    _code 'EXPORTING iv_ref_name = lv_field_name'.
    _code 'iv_ref_index = lv_row ).'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test1

  METHOD test001_02.
* ===========

    _code 'lx_error->to_fpm_error('.
    _code 'iv_ref_name = lv_field_name'.
    _code 'iv_ref_index = lv_row ).'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test2

  METHOD test001_03.
* ===========

    _code 'li_bcs->send_mail('.
    _code 'EXPORTING'.
    _code 'iv_subject_text = iv_subject_text'.
    _code 'iv_distr_list = iv_distr_list'.
    _code 'iv_recipient = iv_recipient'.
    _code 'it_body = lt_body_text ).'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_04.
* ===========

    _code 'foo( EXPORTING iv_bar = bar( CHANGING cv_maz = lv_baz ) ).'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_05.
* ===========

    _code 'foo( EXPORTING iv_bar = ''CHANGING'' ).'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_06.
* ===========

    _code 'foo( EXPORTING iv_bar = '')'' CHANGING cv_moo = lv_boo ).'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_07.
* ===========

    _code 'foo( iv_bar = bar( EXPORTING ci_moo = lv_boo ) ).'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_08.
* ===========

    _code '  cl_ci_inspection=>get_ref('.
    _code '    EXPORTING'.
    _code '      p_user          = '''''.
    _code '      p_name          = lv_name'.
    _code '    RECEIVING'.
    _code '      p_ref           = lo_ci'.
    _code '    EXCEPTIONS'.
    _code '      insp_not_exists = 1'.
    _code '      OTHERS          = 2 ).'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_09.
* ===========

    _code '  cl_ci_inspection=>get_ref('.
    _code '    EXPORTING'.
    _code '      p_user          = '''''.
    _code '      p_name          = lv_name'.
    _code '    EXCEPTIONS'.
    _code '      insp_not_exists = 1'.
    _code '      OTHERS          = 2 ).'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_10.
* ===========

    _code 'lv_foo = bar( EXPORTING ci_moo = lv_boo ).'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

ENDCLASS.       "lcl_Test
