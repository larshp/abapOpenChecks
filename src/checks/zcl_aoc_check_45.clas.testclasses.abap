CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcl_aoc_check_45 DEFINITION LOCAL FRIENDS ltcl_test.

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
          mo_check  TYPE REF TO zcl_aoc_check_45.

    METHODS:
      setup,
      export_import FOR TESTING,
      test001_01 FOR TESTING,
      test001_02 FOR TESTING,
      test001_03 FOR TESTING,
      test002_01 FOR TESTING,
      test002_02 FOR TESTING,
      test002_03 FOR TESTING,
      test002_04 FOR TESTING,
      test002_05 FOR TESTING,
      test002_06 FOR TESTING,
      test003_01 FOR TESTING,
      test003_02 FOR TESTING,
      test003_03 FOR TESTING,
      test003_04 FOR TESTING,
      test003_05 FOR TESTING,
      test004_01 FOR TESTING,
      test004_02 FOR TESTING,
      test005_01 FOR TESTING,
      test005_02 FOR TESTING,
      test005_03 FOR TESTING,
      test010_01 FOR TESTING,
      test011_01 FOR TESTING RAISING cx_static_check.

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

    _code 'DESCRIBE TABLE lt_fcat.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test1

  METHOD test001_02.
* ===========

    _code 'DESCRIBE TABLE lt_fcat LINES lv_lines.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_03.
* ===========

    _code 'lv_lines = lines( lt_fcat ).'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test002_01.
* ===========

    IF mo_check->support_740sp02( ) = abap_false.
      RETURN.
    ENDIF.

    _code 'DATA(lo_new) = NEW cl_gui_frontend_services( ).'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test002_02.
* ===========

    IF mo_check->support_740sp02( ) = abap_false.
      RETURN.
    ENDIF.

    _code 'CREATE OBJECT lo_new.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '002'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test002_03.

    IF mo_check->support_740sp02( ) = abap_false.
      RETURN.
    ENDIF.

    _code 'CREATE OBJECT rr_result TYPE (ls_foo-command_class).'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test002_04.

    IF mo_check->support_740sp02( ) = abap_false.
      RETURN.
    ENDIF.

    _code 'CREATE OBJECT lo_new EXCEPTIONS foo = 4.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test002_05.

    IF mo_check->support_740sp02( ) = abap_false.
      RETURN.
    ENDIF.

    _code 'CREATE OBJECT lo_new EXCEPTIONS OTHERS = 4.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test002_06.

    IF mo_check->support_740sp02( ) = abap_false.
      RETURN.
    ENDIF.

    _code 'CREATE OBJECT lo_new AREA HANDLE lo_handle.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test003_01.

    IF mo_check->support_740sp02( ) = abap_false.
      RETURN.
    ENDIF.

    _code 'DATA bar LIKE LINE OF foo.'.
    _code 'LOOP AT foo INTO bar.'.
    _code 'ENDLOOP.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '003'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test003_02.

    IF mo_check->support_740sp02( ) = abap_false.
      RETURN.
    ENDIF.

    _code 'LOOP AT foo INTO DATA(bar).'.
    _code 'ENDLOOP.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).


    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test003_03.

    IF mo_check->support_740sp02( ) = abap_false.
      RETURN.
    ENDIF.

    _code 'DATA bar LIKE LINE OF foo.'.
    _code 'bar = 2.'.
    _code 'LOOP AT foo INTO bar.'.
    _code 'ENDLOOP.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test003_04.

    IF mo_check->support_740sp02( ) = abap_false.
      RETURN.
    ENDIF.

    _code 'DATA bar LIKE LINE OF foo.'.
    _code 'FORM foo.'.
    _code '  LOOP AT foo INTO bar.'.
    _code '  ENDLOOP.'.
    _code 'ENDFORM.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test003_05.

    IF mo_check->support_740sp02( ) = abap_false.
      RETURN.
    ENDIF.

    _code '  LOOP AT tab.'.
    _code '  ENDLOOP.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test004_01.

    _code 'CONDENSE lv_foo.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '004'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test004_02.

    _code 'lv_foo = condense( lv_foo ).'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test005_01.

    _code 'CONCATENATE LINES OF foo INTO bar.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '005'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test005_02.

    _code 'CONCATENATE LINES OF lt_data INTO lv_xstr IN BYTE MODE.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test005_03.

    _code 'CONCATENATE LINES OF lt_data INTO lv_xstr RESPECTING BLANKS.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test010_01.

    _code 'GET REFERENCE OF i_data INTO lo_data.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '010'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test011_01.

    DATA lv_expected TYPE sci_errc.

    IF mo_check->support_740sp08( ) = abap_true.
      lv_expected = '013'.
    ELSE.
      lv_expected = '011'.
    ENDIF.


    _code 'MOVE-CORRESPONDING foo TO bar.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = lv_expected
                                        act = ms_result-code ).

  ENDMETHOD.

ENDCLASS.       "lcl_Test
