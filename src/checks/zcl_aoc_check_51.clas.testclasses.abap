CLASS ltcl_test DEFINITION DEFERRED.
CLASS zcl_aoc_check_51 DEFINITION LOCAL FRIENDS ltcl_test.

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
          mo_check  TYPE REF TO zcl_aoc_check_51.

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

    _code 'SELECT SINGLE bname FROM usr02 INTO lv_bname.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.                    "test1

  METHOD test001_02.
* ===========

    IF mo_check->supported( ) = abap_false.
      RETURN.
    ENDIF.

    _code 'SELECT SINGLE bname FROM usr02 INTO @lv_bname.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test001_03.
* ===========

    IF mo_check->supported( ) = abap_false.
      RETURN.
    ENDIF.

    _code 'SELECT SINGLE foo bar INTO (@<ls_data>-foo, @<ls_data>-bar) FROM zfoo.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test001_04.
* ===========

    IF mo_check->supported( ) = abap_false.
      RETURN.
    ENDIF.

    _code 'SELECT * FROM usr02 INTO TABLE lt_data.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_05.
* ===========

    IF mo_check->supported( ) = abap_false.
      RETURN.
    ENDIF.

    _code 'SELECT * FROM usr02 INTO TABLE @lt_data.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test001_06.
* ===========

    IF mo_check->supported( ) = abap_false.
      RETURN.
    ENDIF.

    _code 'SELECT SINGLE foo bar INTO (<ls_data>-foo, <ls_data>-bar) FROM zfoo.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_07.
* ===========

    IF mo_check->supported( ) = abap_false.
      RETURN.
    ENDIF.

    _code 'SELECTION-SCREEN END OF BLOCK b1.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test001_08.
* ===========

    IF mo_check->supported( ) = abap_false.
      RETURN.
    ENDIF.

    _code 'SELECT * FROM usr02 APPENDING CORRESPONDING FIELDS OF TABLE @lt_data.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test001_09.
* ===========

    IF mo_check->supported( ) = abap_false.
      RETURN.
    ENDIF.

    _code 'SELECT * FROM usr02 APPENDING TABLE @lt_data.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

  METHOD test001_10.
* ===========

    IF mo_check->supported( ) = abap_false.
      RETURN.
    ENDIF.

    _code 'SELECT SINGLE bname FROM usr02 INTO (@lv_bname).'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result-code ).

  ENDMETHOD.

ENDCLASS.       "lcl_Test
