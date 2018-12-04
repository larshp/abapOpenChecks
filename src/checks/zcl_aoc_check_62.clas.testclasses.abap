*----------------------------------------------------------------------*
*       CLASS lcl_Test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
* ================

    DATA: mt_code   TYPE string_table,
          ms_result TYPE scirest_ad,
          mo_check  TYPE REF TO zcl_aoc_check_62.

    METHODS:
      setup,
      export_import FOR TESTING,
      test001_01 FOR TESTING,
      test001_02 FOR TESTING,
      test001_03 FOR TESTING,
      test001_04 FOR TESTING,
      test001_05 FOR TESTING,
      test002_01 FOR TESTING,
      test002_02 FOR TESTING,
      test002_03 FOR TESTING,
      test003_01 FOR TESTING,
      test003_02 FOR TESTING,
      test003_03 FOR TESTING.

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

    _code 'LOOP AT lt_tab ASSIGNING <fs> WHERE foo = ''ABC''.'.
    _code '  DELETE lt_tab FROM <fs>.'.
    _code 'ENDLOOP.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test1

  METHOD test001_02.
* ===========

    _code 'DELETE lt_tab WHERE foo = ''ABC''.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test001_03.
* ===========

    _code 'LOOP AT lt_tab INTO bar WHERE foo = ''ABC''.'.
    _code '  DELETE lt_tab FROM bar.'.
    _code 'ENDLOOP.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test1

  METHOD test001_04.
* ===========

    _code 'LOOP AT lt_tab INTO DATA(bar) WHERE foo = ''ABC''.'.
    _code '  DELETE lt_tab FROM bar.'.
    _code 'ENDLOOP.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_05.
* ===========

    _code 'LOOP AT lt_tab ASSIGNING FIELD-SYMBOL(<bar>) WHERE foo = ''ABC''.'.
    _code '  DELETE lt_tab FROM <bar>.'.
    _code 'ENDLOOP.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test002_01.

    _code 'LOOP AT lt_tab.'.
    _code '  CONTINUE.'.
    _code 'ENDLOOP.'.
    _code 'WRITE hello.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '002'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test002_02.

    _code 'LOOP AT lt_tab.'.
    _code 'ENDLOOP.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test002_03.

    _code 'WRITE hello.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test003_01.

    _code 'IF lt_table IS NOT INITIAL.'.
    _code 'LOOP AT lt_table INTO ls_data.'.
    _code 'ENDLOOP.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '003'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test003_02.

    _code 'IF lines( lt_messages ) > 0.'.
    _code 'LOOP AT lt_messages INTO ls_message WHERE foo = ''A''.'.
    _code 'ENDLOOP.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '003'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test003_03.

    _code 'LOOP AT lt_table INTO ls_data.'.
    _code 'ENDLOOP.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

ENDCLASS.
