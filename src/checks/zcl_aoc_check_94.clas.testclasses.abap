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
          mo_check  TYPE REF TO zcl_aoc_check_94.

    METHODS:
      setup,
      export_import FOR TESTING,
      test001_01 FOR TESTING,
      test001_02 FOR TESTING,
      test001_03 FOR TESTING,
      test002_01 FOR TESTING,
      test002_02 FOR TESTING,
      test002_03 FOR TESTING,
      test002_04 FOR TESTING.

ENDCLASS.
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
    zcl_aoc_unit_test=>set_check( mo_check ).
  ENDMETHOD.                    "setup

  METHOD export_import.
    zcl_aoc_unit_test=>export_import( mo_check ).
  ENDMETHOD.

  METHOD test001_01.
* ===========

    _code 'SELECT SINGLE mandt'.
    _code '  FROM mara'.
    _code '  WHERE matnr LIKE ''10*''.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '001'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test001_02.
* ===========

    _code 'SELECT SINGLE mandt'.
    _code '  FROM mara'.
    _code '  WHERE matnr LIKE ''10%''.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test001_03.
* ===========

    _code 'SELECT SINGLE mandt'.
    _code '  FROM mara'.
    _code '  WHERE matnr LIKE ''*10%''.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

  METHOD test002_01.
* ===========

    _code 'IF var CP ''Patt%''.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '002'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test002_02.
* ===========

    _code 'IF var NP ''Patt%''.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_equals( exp = '002'
                                        act = ms_result-code ).

  ENDMETHOD.

  METHOD test002_03.
* ===========

    _code 'IF var CP ''Patt*''.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.

  METHOD test002_04.
* ===========

    _code 'IF var NP ''Patt*''.'.
    _code 'ENDIF.'.

    ms_result = zcl_aoc_unit_test=>check( mt_code ).

    cl_abap_unit_assert=>assert_initial( ms_result ).

  ENDMETHOD.

ENDCLASS.
