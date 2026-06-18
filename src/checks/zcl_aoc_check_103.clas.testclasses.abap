*"* use this source file for your ABAP unit test classes
CLASS ltcl_test DEFINITION
  FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA mt_code TYPE string_table.
    DATA ms_result TYPE scirest_ad.
    DATA mo_check TYPE REF TO zcl_aoc_check_103.

    METHODS setup.

    METHODS assert_error_code
      IMPORTING
        iv_expected_error_code TYPE sci_errc.

    METHODS assert_no_error_code.
    METHODS execute_check.

    METHODS database_select_01 FOR TESTING.
    METHODS database_select_02 FOR TESTING.
    METHODS database_select_03 FOR TESTING.

ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.
  DEFINE _code.
    APPEND &1 TO mt_code.
  END-OF-DEFINITION.

  METHOD setup.
    mo_check = NEW #( ).
    zcl_aoc_unit_test=>set_check( mo_check ).
  ENDMETHOD.

  METHOD execute_check.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
  ENDMETHOD.

  METHOD assert_error_code.
    cl_abap_unit_assert=>assert_equals( exp = iv_expected_error_code
                                        act = ms_result-code ).
  ENDMETHOD.

  METHOD assert_no_error_code.
    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.



  METHOD database_select_03.
    " Given
    _code `SELECT *`.
    _code `  FROM demo_sumdist`.
    _code `  UP TO 1 ROWS`.
    _code `  INTO @DATA(lv_host)`.
    _code `  ORDER BY PRIMARY KEY.`.
    _code `ENDSELECT.`.

    " When
    execute_check( ).

    " Then
    assert_error_code( zcl_aoc_check_103=>gc_code-table_has_replacement_object ).
  ENDMETHOD.



  METHOD database_select_01.
    " Given
    _code `SELECT host`.
    _code `  FROM ztable`.
    _code `  UP TO 1 ROWS`.
    _code `  INTO @DATA(lv_host)`.
    _code `  WHERE sysname = @sy-sysid`.
    _code `  ORDER BY PRIMARY KEY.`.
    _code `ENDSELECT.`.

    " When
    execute_check( ).

    " Then
    assert_no_error_code( ).
  ENDMETHOD.

  METHOD database_select_02.
    " Given
    _code `DATA lv_host TYPE string.`.
    _code `SELECT host`.
    _code `  FROM ztable`.
    _code `  UP TO 1 ROWS`.
    _code `  INTO lv_host`.
    _code `  WHERE sysname = sy-sysid`.
    _code `  ORDER BY PRIMARY KEY.`.
    _code `ENDSELECT.`.

    " When
    execute_check( ).

    " Then
    assert_no_error_code( ).
  ENDMETHOD.


ENDCLASS.
