CLASS ltcl_test DEFINITION
  FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    CONSTANTS gc_given_sy_variable TYPE string VALUE `SYSID`.
    CONSTANTS gc_usage_kind LIKE zcl_aoc_sy_variable_analyzer=>gc_usage_kind
                            VALUE zcl_aoc_sy_variable_analyzer=>gc_usage_kind.

    DATA mt_given_code TYPE string_table.

    DATA mt_result TYPE zcl_aoc_sy_variable_analyzer=>ty_t_result.

    METHODS analyze_variables.

    METHODS assert_single_result
      IMPORTING
        is_expected_result TYPE zcl_aoc_sy_variable_analyzer=>ty_s_result.

    METHODS if_condition_01 FOR TESTING RAISING cx_static_check.

    METHODS if_condition_02 FOR TESTING RAISING cx_static_check.

    METHODS if_condition_03 FOR TESTING RAISING cx_static_check.

    METHODS case_condition_01 FOR TESTING RAISING cx_static_check.

    METHODS case_condition_02 FOR TESTING RAISING cx_static_check.

    METHODS case_condition_03 FOR TESTING RAISING cx_static_check.

    METHODS elseif_condition FOR TESTING RAISING cx_static_check.

    METHODS constructor_expression_cond FOR TESTING RAISING cx_static_check.

    METHODS constructor_expression_switch FOR TESTING RAISING cx_static_check.

    METHODS local_variable_assignment FOR TESTING RAISING cx_static_check.

    METHODS within_macro FOR TESTING RAISING cx_static_check.

    METHODS method_call FOR TESTING RAISING cx_static_check.

    METHODS database_select_01 FOR TESTING RAISING cx_static_check.

    METHODS database_select_02 FOR TESTING RAISING cx_static_check.

    METHODS type_definition_01 FOR TESTING RAISING cx_static_check.

    METHODS type_definition_02 FOR TESTING RAISING cx_static_check.

    METHODS type_definition_03 FOR TESTING RAISING cx_static_check.

    METHODS type_definition_04 FOR TESTING RAISING cx_static_check.

    METHODS type_definition_05 FOR TESTING RAISING cx_static_check.

    METHODS type_definition_06 FOR TESTING RAISING cx_static_check.

    METHODS type_definition_07 FOR TESTING RAISING cx_static_check.

    METHODS type_definition_08 FOR TESTING RAISING cx_static_check.

    METHODS ranges FOR TESTING RAISING cx_static_check.

    METHODS default_value_01 FOR TESTING RAISING cx_static_check.

    METHODS default_value_02 FOR TESTING RAISING cx_static_check.

    METHODS concatenate FOR TESTING RAISING cx_static_check.

    METHODS overridden FOR TESTING RAISING cx_static_check.

    METHODS write FOR TESTING RAISING cx_static_check.

    METHODS message FOR TESTING RAISING cx_static_check.

    METHODS check_condition FOR TESTING RAISING cx_static_check.

    METHODS assert_condition FOR TESTING RAISING cx_static_check.

    METHODS no_result FOR TESTING RAISING cx_static_check.

    METHODS multiple_results FOR TESTING RAISING cx_static_check.

    METHODS call_function FOR TESTING RAISING cx_static_check.

    METHODS move_01 FOR TESTING RAISING cx_static_check.

    METHODS move_02 FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.
  DEFINE _code.
    APPEND &1 TO mt_given_code.
  END-OF-DEFINITION.

  METHOD if_condition_01.
    " Given
    _code `IF sy-sysid = 'PRD'.`.
    _code `ENDIF.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 1
                                   token_index     = 2
                                   usage_kind      = gc_usage_kind-in_condition ) ).
  ENDMETHOD.

  METHOD analyze_variables.
    DATA(lo_scan) = zcl_aoc_unit_test=>create_scan( mt_given_code ).
    DATA(lo_cut) = NEW zcl_aoc_sy_variable_analyzer( lo_scan ).
    mt_result = lo_cut->analyze_variable_usage( gc_given_sy_variable ).
  ENDMETHOD.

  METHOD assert_single_result.
    cl_abap_unit_assert=>assert_equals( exp = 1
                                        act = lines( mt_result ) ).

    cl_abap_unit_assert=>assert_equals( exp = is_expected_result
                                        act = mt_result[ 1 ] ).
  ENDMETHOD.

  METHOD if_condition_02.
    " Given
    _code `IF 1 = 2 OR sy-sysid = 'PRD'.`.
    _code `ENDIF.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 1
                                   token_index     = 6
                                   usage_kind      = gc_usage_kind-in_condition ) ).
  ENDMETHOD.

  METHOD case_condition_01.
    " Given
    _code `CASE sy-sysid.`.
    _code `  WHEN 'PRD'.`.
    _code `ENDCASE.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 1
                                   token_index     = 2
                                   usage_kind      = gc_usage_kind-in_condition ) ).
  ENDMETHOD.

  METHOD if_condition_03.
    " Given
    _code `IF 'PRD' = sy-sysid.`.
    _code `ENDIF.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 1
                                   token_index     = 4
                                   usage_kind      = gc_usage_kind-in_condition ) ).
  ENDMETHOD.

  METHOD case_condition_02.
    " Given
    _code `CASE 'PRD'.`.
    _code `  WHEN sy-sysid.`.
    _code `ENDCASE.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 2
                                   token_index     = 4
                                   usage_kind      = gc_usage_kind-in_condition ) ).
  ENDMETHOD.

  METHOD case_condition_03.
    " Given
    _code `CASE 'PRD'.`.
    _code `  WHEN sy-mandt OR sy-sysid.`.
    _code `ENDCASE.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 2
                                   token_index     = 6
                                   usage_kind      = gc_usage_kind-in_condition ) ).
  ENDMETHOD.

  METHOD elseif_condition.
    " Given
    _code `IF 1 = 2.`.
    _code `ELSEIF sy-sysid = 'PRD'.`.
    _code `ENDIF.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 2
                                   token_index     = 6
                                   usage_kind      = gc_usage_kind-in_condition ) ).
  ENDMETHOD.

  METHOD constructor_expression_cond.
    " Given
    _code `DATA lv_result TYPE abap_bool.`.
    _code `lv_result = COND #( WHEN sy-sysid = 'PRD' THEN abap_true ELSE abap_false ).`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 2
                                   token_index     = 10
                                   usage_kind      = gc_usage_kind-usage_uncategorized ) ).
  ENDMETHOD.

  METHOD constructor_expression_switch.
    " Given
    _code `DATA lv_result TYPE abap_bool.`.
    _code `lv_result = SWITCH #( sy-sysid WHEN 'PRD' THEN abap_true ELSE abap_false ).`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 2
                                   token_index     = 9
                                   usage_kind      = gc_usage_kind-usage_uncategorized ) ).
  ENDMETHOD.

  METHOD local_variable_assignment.
    " Given
    _code `DATA lv_result TYPE string.`.
    _code `lv_result = sy-sysid.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 2
                                   token_index     = 7
                                   usage_kind      = gc_usage_kind-assigned_to_variable ) ).
  ENDMETHOD.

  METHOD within_macro.
    " Given
    _code `DEFINE example.`.
    _code `  DATA lv_result TYPE string.`.
    _code `  lv_result = sy-sysid.`.
    _code `END-OF-DEFINITION.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 2
                                   token_index     = 9
                                   usage_kind      = gc_usage_kind-within_macro ) ).
  ENDMETHOD.

  METHOD method_call.
    " Given
    _code `method( iv_system = sy-sysid ).`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 1
                                   token_index     = 4
                                   usage_kind      = gc_usage_kind-usage_uncategorized ) ).
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
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 1
                                   token_index     = 14
                                   usage_kind      = gc_usage_kind-in_database_select ) ).
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
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 2
                                   token_index     = 18
                                   usage_kind      = gc_usage_kind-in_database_select ) ).
  ENDMETHOD.

  METHOD type_definition_01.
    " Given
    _code `DATA lv_system TYPE sy-sysid.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 1
                                   token_index     = 4
                                   usage_kind      = gc_usage_kind-type_definition ) ).
  ENDMETHOD.

  METHOD type_definition_02.
    " Given
    _code `DATA lv_system LIKE sy-sysid.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 1
                                   token_index     = 4
                                   usage_kind      = gc_usage_kind-type_definition ) ).
  ENDMETHOD.

  METHOD type_definition_03.
    " Given
    _code `CLASS-DATA lv_system TYPE sy-sysid.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 1
                                   token_index     = 4
                                   usage_kind      = gc_usage_kind-type_definition ) ).
  ENDMETHOD.

  METHOD type_definition_04.
    " Given
    _code `CLASS-DATA lv_system LIKE sy-sysid.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 1
                                   token_index     = 4
                                   usage_kind      = gc_usage_kind-type_definition ) ).
  ENDMETHOD.

  METHOD type_definition_05.
    " Given
    _code `TYPES: BEGIN OF gy_structure,`.
    _code `         system TYPE sy-sysid,`.
    _code `       END OF gy_structure.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 2
                                   token_index     = 8
                                   usage_kind      = gc_usage_kind-type_definition ) ).
  ENDMETHOD.

  METHOD type_definition_06.
    " Given
    _code `TYPES: BEGIN OF gy_structure,`.
    _code `         system LIKE sy-sysid,`.
    _code `       END OF gy_structure.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 2
                                   token_index     = 8
                                   usage_kind      = gc_usage_kind-type_definition ) ).
  ENDMETHOD.

  METHOD type_definition_07.
    " Given
    _code `METHODS example`.
    _code `  IMPORTING`.
    _code `    iv_system TYPE sy-sysid.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 1
                                   token_index     = 6
                                   usage_kind      = gc_usage_kind-type_definition ) ).
  ENDMETHOD.

  METHOD type_definition_08.
    " Given
    _code `CLASS-METHODS example`.
    _code `  IMPORTING`.
    _code `    iv_system TYPE sy-sysid.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 1
                                   token_index     = 6
                                   usage_kind      = gc_usage_kind-type_definition ) ).
  ENDMETHOD.

  METHOD ranges.
    " Given
    _code `RANGES range FOR sy-sysid.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 1
                                   token_index     = 4
                                   usage_kind      = gc_usage_kind-type_definition ) ).
  ENDMETHOD.

  METHOD default_value_01.
    " Given
    _code `METHODS example`.
    _code `  IMPORTING`.
    _code `    iv_system TYPE syst_sysid DEFAULT sy-sysid.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 1
                                   token_index     = 8
                                   usage_kind      = gc_usage_kind-as_default_value ) ).
  ENDMETHOD.

  METHOD default_value_02.
    " Given
    _code `CLASS-METHODS example`.
    _code `  IMPORTING`.
    _code `    iv_system TYPE syst_sysid DEFAULT sy-sysid.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 1
                                   token_index     = 8
                                   usage_kind      = gc_usage_kind-as_default_value ) ).
  ENDMETHOD.

  METHOD concatenate.
    " Given
    _code `DATA lv_system TYPE string.`.
    _code `CONCATENATE sy-sysid '' INTO lv_system.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 2
                                   token_index     = 6
                                   usage_kind      = gc_usage_kind-in_concatenate ) ).
  ENDMETHOD.

  METHOD overridden.
    " Given
    _code `sy-sysid = 'PRD'.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 1
                                   token_index     = 1
                                   usage_kind      = gc_usage_kind-overridden ) ).
  ENDMETHOD.

  METHOD write.
    " Given
    _code `WRITE sy-sysid.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 1
                                   token_index     = 2
                                   usage_kind      = gc_usage_kind-in_write ) ).
  ENDMETHOD.

  METHOD message.
    " Given
    _code `MESSAGE e000(z) WITH sy-sysid.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 1
                                   token_index     = 4
                                   usage_kind      = gc_usage_kind-in_message ) ).
  ENDMETHOD.

  METHOD check_condition.
    " Given
    _code `CHECK sy-sysid = 'PRD'.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 1
                                   token_index     = 2
                                   usage_kind      = gc_usage_kind-in_condition ) ).
  ENDMETHOD.

  METHOD assert_condition.
    " Given
    _code `ASSERT sy-sysid = 'PRD'.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 1
                                   token_index     = 2
                                   usage_kind      = gc_usage_kind-in_condition ) ).
  ENDMETHOD.

  METHOD no_result.
    " Given
    _code `IF sy-mandt = '001'.`.
    _code `ENDIF.`.

    " When
    analyze_variables( ).

    " Then
    cl_abap_unit_assert=>assert_initial( mt_result ).
  ENDMETHOD.

  METHOD multiple_results.
    DATA lt_expected_result LIKE mt_result.

    " Given
    _code `IF sy-sysid = 'PRD'.`.
    _code `  WRITE sy-sysid.`.
    _code `ELSEIF sy-sysid = 'QAS`.
    _code `  sy-sysid = 'DEV'.`.
    _code `ENDIF.`.

    " When
    analyze_variables( ).

    " Then
    lt_expected_result = VALUE #( ( statement_index = 1
                                    token_index     = 2
                                    usage_kind      = gc_usage_kind-in_condition )
                                  ( statement_index = 2
                                    token_index     = 2
                                    usage_kind      = gc_usage_kind-in_write )
                                  ( statement_index = 3
                                    token_index     = 2
                                    usage_kind      = gc_usage_kind-in_condition )
                                  ( statement_index = 4
                                    token_index     = 1
                                    usage_kind      = gc_usage_kind-overridden ) ).
  ENDMETHOD.

  METHOD call_function.
    " Given
    _code `CALL FUNCTION 'ZEXAMPLE'`.
    _code `  EXPORTING`.
    _code `    sysid = sy-sysid.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 1
                                   token_index     = 7
                                   usage_kind      = gc_usage_kind-in_function_module_call ) ).
  ENDMETHOD.

  METHOD move_01.
    " Given
    _code `MOVE sy-sysid TO DATA(lv_sysid).`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 1
                                   token_index     = 2
                                   usage_kind      = gc_usage_kind-assigned_to_variable ) ).
  ENDMETHOD.

  METHOD move_02.
    " Given
    _code `MOVE 'DEV' TO sy-sysid.`.

    " When
    analyze_variables( ).

    " Then
    assert_single_result( VALUE #( statement_index = 1
                                   token_index     = 4
                                   usage_kind      = gc_usage_kind-overridden ) ).
  ENDMETHOD.

ENDCLASS.
