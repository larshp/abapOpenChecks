CLASS ltcl_super DEFINITION
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PROTECTED SECTION.
    DATA mt_code TYPE stringtab.
    DATA ms_result TYPE scirest_ad.
    DATA mo_check TYPE REF TO zcl_aoc_check_102.

    METHODS execute_check.

  PRIVATE SECTION.
    METHODS setup.
ENDCLASS.


CLASS ltcl_super IMPLEMENTATION.
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
ENDCLASS.


CLASS ltcl_main_test DEFINITION
  INHERITING FROM ltcl_super
  FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    METHODS assert_error_code
      IMPORTING
        iv_expected_error_code TYPE sci_errc.

    METHODS assert_no_error_code.

    METHODS export_import FOR TESTING.
    METHODS if_condition_01 FOR TESTING.
    METHODS first_letter_01 FOR TESTING.
    METHODS first_letter_02 FOR TESTING.
    METHODS first_letter_03 FOR TESTING.
    METHODS if_condition_02 FOR TESTING.
    METHODS if_condition_03 FOR TESTING.
    METHODS case_condition_01 FOR TESTING.
    METHODS case_condition_02 FOR TESTING.
    METHODS case_condition_03 FOR TESTING.
    METHODS elseif_condition FOR TESTING.
    METHODS constructor_expression_cond FOR TESTING.
    METHODS local_variable_assignment FOR TESTING.
    METHODS constructor_expression_switch FOR TESTING.
    METHODS within_macro FOR TESTING.
    METHODS method_call FOR TESTING.
    METHODS database_select_01 FOR TESTING.
    METHODS database_select_02 FOR TESTING.
    METHODS ignore_type_definition_01 FOR TESTING.
    METHODS ignore_type_definition_02 FOR TESTING.
    METHODS ignore_type_definition_03 FOR TESTING.
    METHODS ignore_type_definition_04 FOR TESTING.
    METHODS ignore_type_definition_05 FOR TESTING.
    METHODS ignore_type_definition_06 FOR TESTING.
    METHODS ignore_type_definition_07 FOR TESTING.
    METHODS ignore_type_definition_08 FOR TESTING.
    METHODS ignore_ranges FOR TESTING.
    METHODS default_value_01 FOR TESTING.
    METHODS default_value_02 FOR TESTING.
    METHODS concatenate FOR TESTING.
    METHODS overridden FOR TESTING.
    METHODS write FOR TESTING.
    METHODS message FOR TESTING.
    METHODS check_condition FOR TESTING.
    METHODS assert_condition FOR TESTING.
    METHODS call_function FOR TESTING.
ENDCLASS.


CLASS ltcl_main_test IMPLEMENTATION.
  METHOD export_import.
    zcl_aoc_unit_test=>export_import( mo_check ).
  ENDMETHOD.


  METHOD assert_error_code.
    cl_abap_unit_assert=>assert_equals( exp = iv_expected_error_code
                                        act = ms_result-code ).
  ENDMETHOD.


  METHOD assert_no_error_code.
    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.


  METHOD if_condition_01.
    " Given
    _code `IF sy-sysid = 'PRD'.`.
    _code `ENDIF.`.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-in_condition ).
  ENDMETHOD.


  METHOD first_letter_01.
    " Given
    _code `IF sy-sysid+0(1) = 'P'.`.
    _code `ENDIF.`.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-first_letter_used ).
  ENDMETHOD.


  METHOD first_letter_02.
    " Given
    _code `IF sy-sysid(1) = 'P'.`.
    _code `ENDIF.`.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-first_letter_used ).
  ENDMETHOD.


  METHOD first_letter_03.
    " Given
    _code `SELECT host`.
    _code `  FROM ztable`.
    _code `  UP TO 1 ROWS`.
    _code `  INTO @DATA(lv_host)`.
    _code `  WHERE sysname = @sy-sysid+0(1)`.
    _code `  ORDER BY PRIMARY KEY.`.
    _code `ENDSELECT.`.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-first_letter_used ).
  ENDMETHOD.


  METHOD if_condition_02.
    " Given
    _code `IF 1 = 2 OR sy-sysid = 'PRD'.`.
    _code `ENDIF.`.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-in_condition ).
  ENDMETHOD.


  METHOD case_condition_01.
    " Given
    _code `CASE sy-sysid.`.
    _code `  WHEN 'PRD'.`.
    _code `ENDCASE.`.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-in_condition ).
  ENDMETHOD.


  METHOD if_condition_03.
    " Given
    _code `IF 'PRD' = sy-sysid.`.
    _code `ENDIF.`.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-in_condition ).
  ENDMETHOD.


  METHOD case_condition_02.
    " Given
    _code `CASE 'PRD'.`.
    _code `  WHEN sy-sysid.`.
    _code `ENDCASE.`.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-in_condition ).
  ENDMETHOD.


  METHOD case_condition_03.
    " Given
    _code `CASE 'PRD'.`.
    _code `  WHEN sy-mandt OR sy-sysid.`.
    _code `ENDCASE.`.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-in_condition ).
  ENDMETHOD.


  METHOD elseif_condition.
    " Given
    _code `IF 1 = 2.`.
    _code `ELSEIF sy-sysid = 'PRD'.`.
    _code `ENDIF.`.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-in_condition ).
  ENDMETHOD.


  METHOD constructor_expression_cond.
    " Given
    _code `DATA lv_result TYPE abap_bool.`.
    _code `lv_result = COND #( WHEN sy-sysid = 'PRD' THEN abap_true ELSE abap_false ).`.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-usage_uncategorized ).
  ENDMETHOD.


  METHOD constructor_expression_switch.
    " Given
    _code `DATA lv_result TYPE abap_bool.`.
    _code `lv_result = SWITCH #( sy-sysid WHEN 'PRD' THEN abap_true ELSE abap_false ).`.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-usage_uncategorized ).
  ENDMETHOD.


  METHOD local_variable_assignment.
    " Given
    _code `DATA lv_result TYPE string.`.
    _code `lv_result = sy-sysid.`.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-assigned_to_variable ).
  ENDMETHOD.


  METHOD within_macro.
    " Given
    _code `DEFINE example.`.
    _code `  DATA lv_result TYPE string.`.
    _code `  lv_result = sy-sysid.`.
    _code `END-OF-DEFINITION.`.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-within_macro ).
  ENDMETHOD.


  METHOD method_call.
    " Given
    _code `method( iv_system = sy-sysid ).`.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-usage_uncategorized ).
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
    assert_error_code( gc_code-in_database_select ).
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
    assert_error_code( gc_code-in_database_select ).
  ENDMETHOD.


  METHOD ignore_type_definition_01.
    " Given
    _code `DATA lv_system TYPE sy-sysid.`.

    " When
    execute_check( ).

    " Then
    assert_no_error_code( ).
  ENDMETHOD.


  METHOD ignore_type_definition_02.
    " Given
    _code `DATA lv_system LIKE sy-sysid.`.

    " When
    execute_check( ).

    " Then
    assert_no_error_code( ).
  ENDMETHOD.


  METHOD ignore_type_definition_03.
    " Given
    _code `CLASS-DATA gv_system TYPE sy-sysid.`.

    " When
    execute_check( ).

    " Then
    assert_no_error_code( ).
  ENDMETHOD.


  METHOD ignore_type_definition_04.
    " Given
    _code `CLASS-DATA gv_system LIKE sy-sysid.`.

    " When
    execute_check( ).

    " Then
    assert_no_error_code( ).
  ENDMETHOD.


  METHOD ignore_type_definition_05.
    " Given
    _code `TYPES: BEGIN OF gy_structure,`.
    _code `         system TYPE sy-sysid,`.
    _code `       END OF gy_structure.`.

    " When
    execute_check( ).

    " Then
    assert_no_error_code( ).
  ENDMETHOD.


  METHOD ignore_type_definition_06.
    " Given
    _code `TYPES: BEGIN OF gy_structure,`.
    _code `         system LIKE sy-sysid,`.
    _code `       END OF gy_structure.`.

    " When
    execute_check( ).

    " Then
    assert_no_error_code( ).
  ENDMETHOD.


  METHOD ignore_type_definition_07.
    " Given
    _code `METHODS example`.
    _code `  IMPORTING`.
    _code `    iv_system TYPE sy-sysid.`.

    " When
    execute_check( ).

    " Then
    assert_no_error_code( ).
  ENDMETHOD.


  METHOD ignore_type_definition_08.
    " Given
    _code `CLASS-METHODS example`.
    _code `  IMPORTING`.
    _code `    iv_system TYPE sy-sysid.`.

    " When
    execute_check( ).

    " Then
    assert_no_error_code( ).
  ENDMETHOD.


  METHOD ignore_ranges.
    " Given
    _code `RANGES range FOR sy-sysid.`.

    " When
    execute_check( ).

    " Then
    assert_no_error_code( ).
  ENDMETHOD.


  METHOD default_value_01.
    " Given
    _code `METHODS example`.
    _code `  IMPORTING`.
    _code `    iv_system TYPE syst_sysid DEFAULT sy-sysid.`.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-as_default_value ).
  ENDMETHOD.


  METHOD default_value_02.
    " Given
    _code `CLASS-METHODS example`.
    _code `  IMPORTING`.
    _code `    iv_system TYPE syst_sysid DEFAULT sy-sysid.`.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-as_default_value ).
  ENDMETHOD.


  METHOD concatenate.
    " Given
    _code `DATA lv_system TYPE string.`.
    _code `CONCATENATE sy-sysid '' INTO lv_system.`.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-in_concatenate ).
  ENDMETHOD.


  METHOD overridden.
    " Given
    _code `sy-sysid = 'PRD'.`.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-overridden ).
  ENDMETHOD.


  METHOD write.
    " Given
    _code `WRITE sy-sysid.`.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-in_write ).
  ENDMETHOD.


  METHOD message.
    " Given
    _code `MESSAGE e000(z) WITH sy-sysid.`.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-in_message ).
  ENDMETHOD.


  METHOD check_condition.
    " Given
    _code `CHECK sy-sysid = 'PRD'.`.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-in_condition ).
  ENDMETHOD.


  METHOD assert_condition.
    " Given
    _code `ASSERT sy-sysid = 'PRD'.`.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-in_condition ).
  ENDMETHOD.


  METHOD call_function.
    " Given
    _code `CALL FUNCTION 'ZEXAMPLE'`.
    _code `  EXPORTING`.
    _code `    sysid = sy-sysid.`.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-in_function_module_call ).
  ENDMETHOD.
ENDCLASS.


"! Contains one method for each error code, plus two for the base type tests
CLASS ltcl_error_types DEFINITION
  INHERITING FROM ltcl_super
  FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    METHODS error_as_base_type FOR TESTING RAISING cx_static_check.
    METHODS base_type FOR TESTING RAISING cx_static_check.

    METHODS in_condition FOR TESTING RAISING cx_static_check.
    METHODS usage_uncategorized FOR TESTING RAISING cx_static_check.
    METHODS first_letter_used FOR TESTING RAISING cx_static_check.
    METHODS as_default_value FOR TESTING RAISING cx_static_check.
    METHODS in_concatenate FOR TESTING RAISING cx_static_check.
    METHODS overridden FOR TESTING RAISING cx_static_check.
    METHODS assigned_to_variable FOR TESTING RAISING cx_static_check.
    METHODS in_database_select FOR TESTING RAISING cx_static_check.
    METHODS in_write FOR TESTING RAISING cx_static_check.
    METHODS in_message FOR TESTING RAISING cx_static_check.
    METHODS within_macro FOR TESTING RAISING cx_static_check.
    METHODS in_function_module_call FOR TESTING RAISING cx_static_check.

    METHODS assert_error_type
      IMPORTING
        iv_exp_error_type TYPE sci_errty.
ENDCLASS.


CLASS ltcl_error_types IMPLEMENTATION.
  METHOD error_as_base_type.
    " Given: No error types have been changed
    _code `ASSERT sy-sysid = 'PRD'.`.

    " When
    execute_check( ).

    " Then
    assert_error_type( 'E' ).
  ENDMETHOD.


  METHOD base_type.
    DATA lv_attributes TYPE xstring.

    " Given
    _code `ASSERT sy-sysid = 'PRD'.`.

    " When
    EXPORT mv_errty = 'W'
           TO DATA BUFFER lv_attributes.
    mo_check->put_attributes( lv_attributes ).

    execute_check( ).

    " Then
    assert_error_type( 'W' ).
  ENDMETHOD.


  METHOD assert_error_type.
    cl_abap_unit_assert=>assert_equals( exp = iv_exp_error_type
                                        act = ms_result-kind ).
  ENDMETHOD.


  METHOD usage_uncategorized.
    DATA lv_attributes TYPE xstring.

    " Given
    _code `method( iv_system = sy-sysid ).`.

    " When
    EXPORT mv_errty                           = 'E'
           ms_error_types-usage_uncategorized = 'W'
           TO DATA BUFFER lv_attributes.
    mo_check->put_attributes( lv_attributes ).

    execute_check( ).

    " Then
    assert_error_type( 'W' ).
  ENDMETHOD.


  METHOD in_condition.
    DATA lv_attributes TYPE xstring.

    " Given
    _code `ASSERT sy-sysid = 'PRD'.`.

    " When
    EXPORT mv_errty                    = 'E'
           ms_error_types-in_condition = 'W'
           TO DATA BUFFER lv_attributes.
    mo_check->put_attributes( lv_attributes ).

    execute_check( ).

    " Then
    assert_error_type( 'W' ).
  ENDMETHOD.


  METHOD first_letter_used.
    DATA lv_attributes TYPE xstring.

    " Given
    _code `IF sy-sysid+0(1) = 'P'.`.
    _code `ENDIF.`.

    " When
    EXPORT mv_errty                         = 'E'
           ms_error_types-first_letter_used = 'W'
           TO DATA BUFFER lv_attributes.
    mo_check->put_attributes( lv_attributes ).

    execute_check( ).

    " Then
    assert_error_type( 'W' ).
  ENDMETHOD.


  METHOD as_default_value.
    DATA lv_attributes TYPE xstring.

    " Given
    _code `IF sy-sysid+0(1) = 'P'.`.
    _code `ENDIF.`.

    " When
    EXPORT mv_errty                         = 'E'
           ms_error_types-first_letter_used = 'W'
           TO DATA BUFFER lv_attributes.
    mo_check->put_attributes( lv_attributes ).

    execute_check( ).

    " Then
    assert_error_type( 'W' ).
  ENDMETHOD.


  METHOD in_concatenate.
    DATA lv_attributes TYPE xstring.

    " Given
    _code `DATA lv_system TYPE string.`.
    _code `CONCATENATE sy-sysid '' INTO lv_system.`.

    " When
    EXPORT mv_errty                      = 'E'
           ms_error_types-in_concatenate = 'W'
           TO DATA BUFFER lv_attributes.
    mo_check->put_attributes( lv_attributes ).

    execute_check( ).

    " Then
    assert_error_type( 'W' ).
  ENDMETHOD.


  METHOD overridden.
    DATA lv_attributes TYPE xstring.

    " Given
    _code `sy-sysid = 'PRD'.`.

    " When
    EXPORT mv_errty                  = 'E'
           ms_error_types-overridden = 'W'
           TO DATA BUFFER lv_attributes.
    mo_check->put_attributes( lv_attributes ).

    execute_check( ).

    " Then
    assert_error_type( 'W' ).
  ENDMETHOD.


  METHOD assigned_to_variable.
    DATA lv_attributes TYPE xstring.

    " Given
    _code `DATA lv_result TYPE string.`.
    _code `lv_result = sy-sysid.`.

    " When
    EXPORT mv_errty                            = 'E'
           ms_error_types-assigned_to_variable = 'W'
           TO DATA BUFFER lv_attributes.
    mo_check->put_attributes( lv_attributes ).

    execute_check( ).

    " Then
    assert_error_type( 'W' ).
  ENDMETHOD.


  METHOD in_database_select.
    DATA lv_attributes TYPE xstring.

    " Given
    _code `SELECT host`.
    _code `  FROM ztable`.
    _code `  UP TO 1 ROWS`.
    _code `  INTO @DATA(lv_host)`.
    _code `  WHERE sysname = @sy-sysid`.
    _code `  ORDER BY PRIMARY KEY.`.
    _code `ENDSELECT.`.

    " When
    EXPORT mv_errty                          = 'E'
           ms_error_types-in_database_select = 'W'
           TO DATA BUFFER lv_attributes.
    mo_check->put_attributes( lv_attributes ).

    execute_check( ).

    " Then
    assert_error_type( 'W' ).
  ENDMETHOD.


  METHOD in_write.
    DATA lv_attributes TYPE xstring.

    " Given
    _code `WRITE sy-sysid.`.

    " When
    EXPORT mv_errty                = 'E'
           ms_error_types-in_write = 'W'
           TO DATA BUFFER lv_attributes.
    mo_check->put_attributes( lv_attributes ).

    execute_check( ).

    " Then
    assert_error_type( 'W' ).
  ENDMETHOD.


  METHOD in_message.
    DATA lv_attributes TYPE xstring.

    " Given
    _code `MESSAGE e000(z) WITH sy-sysid.`.

    " When
    EXPORT mv_errty                  = 'E'
           ms_error_types-in_message = 'W'
           TO DATA BUFFER lv_attributes.
    mo_check->put_attributes( lv_attributes ).

    execute_check( ).

    " Then
    assert_error_type( 'W' ).
  ENDMETHOD.


  METHOD within_macro.
    DATA lv_attributes TYPE xstring.

    " Given
    _code `DEFINE example.`.
    _code `  DATA lv_result TYPE string.`.
    _code `  lv_result = sy-sysid.`.
    _code `END-OF-DEFINITION.`.

    " When
    EXPORT mv_errty                    = 'E'
           ms_error_types-within_macro = 'W'
           TO DATA BUFFER lv_attributes.
    mo_check->put_attributes( lv_attributes ).

    execute_check( ).

    " Then
    assert_error_type( 'W' ).
  ENDMETHOD.


  METHOD in_function_module_call.
    DATA lv_attributes TYPE xstring.

    " Given
    _code `CALL FUNCTION 'ZEXAMPLE'`.
    _code `  EXPORTING`.
    _code `    sysid = sy-sysid.`.

    " When
    EXPORT mv_errty                               = 'E'
           ms_error_types-in_function_module_call = 'W'
           TO DATA BUFFER lv_attributes.
    mo_check->put_attributes( lv_attributes ).

    execute_check( ).

    " Then
    assert_error_type( 'W' ).
  ENDMETHOD.
ENDCLASS.
