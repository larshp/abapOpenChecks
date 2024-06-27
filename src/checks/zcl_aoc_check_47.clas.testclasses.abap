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
          mo_check  TYPE REF TO zcl_aoc_check_47.

    METHODS:
      setup,
      export_import FOR TESTING,
      test001_01 FOR TESTING,
      test001_02 FOR TESTING,
      test001_03 FOR TESTING,
      test001_04 FOR TESTING,
      test001_05 FOR TESTING,
      test001_06 FOR TESTING.

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
  ENDMETHOD.

  METHOD export_import.
    zcl_aoc_unit_test=>export_import( mo_check ).
  ENDMETHOD.

  METHOD test001_01.
* ===========

    _code 'CALL FUNCTION ''ZRFC_RFC'''.
    _code '  DESTINATION iv_rfc'.
    _code '  TABLES'.
    _code '    it_where              = lt_where'.
    _code '    et_data               = rt_data'.
    _code '  EXCEPTIONS'.
    _code '    system_failure        = 1 MESSAGE lv_msg'.
    _code '    communication_failure = 2 MESSAGE lv_msg'.
    _code '    resource_failure      = 3.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.                    "test1

  METHOD test001_02.
* ===========

    _code 'CALL FUNCTION ''ZRFC_RFC'''.
    _code '  DESTINATION iv_rfc'.
    _code '  TABLES'.
    _code '    it_where              = lt_where'.
    _code '    et_data               = rt_data'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_03.
* ===========

    _code 'CALL FUNCTION ''ZRFC_RFC'''.
    _code '  DESTINATION iv_rfc'.
    _code '  TABLES'.
    _code '    it_where              = lt_where'.
    _code '    et_data               = rt_data'.
    _code '  EXCEPTIONS'.
    _code '    system_failure        = 1'.
    _code '    communication_failure = 2'.
    _code '    resource_failure      = 3.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_04.
* ===========

    _code 'CALL FUNCTION ''ZRFC_RFC'''.
    _code '  DESTINATION ''NONE'''.
    _code '  TABLES'.
    _code '    it_where              = lt_where'.
    _code '    et_data               = rt_data'.
    _code '  EXCEPTIONS'.
    _code '    system_failure        = 1'.
    _code '    communication_failure = 2'.
    _code '    resource_failure      = 3.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_05.

    _code 'CALL FUNCTION ''GET_PRINT_PARAMETERS'''.
    _code '  EXPORTING'.
    _code '    copies      = 1'.
    _code '    list_name   = ''ABC'''.
    _code '    list_text   = ''text'''.
    _code '    destination = iv_tddest.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_06.

    _code 'NEW-PAGE PRINT ON'.
    _code '         DESTINATION             block1ds'.
    _code '         LIST NAME               TEXT-110'.
    _code '         COVER TEXT              TEXT-110'.
    _code '         IMMEDIATELY             block1sf'.
    _code '         KEEP IN SPOOL           on'.
    _code '         DATASET EXPIRATION      con_spdays'.
    _code '         NEW LIST IDENTIFICATION on'.
    _code '         LINE-COUNT              sy-linct'.
    _code '         LINE-SIZE               sy-linsz'.
    _code '         NO DIALOG.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

ENDCLASS.       "lcl_Test
