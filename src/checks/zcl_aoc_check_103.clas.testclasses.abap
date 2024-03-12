*"* use this source file for your ABAP unit test classes
class ltcl_test definition
  final
  for testing
  risk level harmless
  duration short.

  private section.
    data mt_code type string_table.
    data ms_result type scirest_ad.
    data mo_check type ref to zcl_aoc_check_103.

    methods setup.

    methods assert_error_code
      importing
        iv_expected_error_code type sci_errc.

    methods assert_no_error_code.
    methods execute_check.

    methods database_select_01 for testing.
    methods database_select_02 for testing.
    methods database_select_03 for testing.

endclass.


class ltcl_test implementation.
  define _code.
    append &1 to mt_code.
  end-of-definition.

  method setup.
    mo_check = new #( ).
    zcl_aoc_unit_test=>set_check( mo_check ).
  endmethod.

  method execute_check.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
  endmethod.

  method assert_error_code.
    cl_abap_unit_assert=>assert_equals( exp = iv_expected_error_code
                                        act = ms_result-code ).
  endmethod.

  method assert_no_error_code.
    cl_abap_unit_assert=>assert_initial( ms_result ).
  endmethod.



  method database_select_03.
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
  endmethod.



  method database_select_01.
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
  endmethod.

  method database_select_02.
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
  endmethod.


endclass.
