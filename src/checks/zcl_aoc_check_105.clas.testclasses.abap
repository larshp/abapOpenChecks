CONSTANTS: BEGIN OF gc_function_modules,
             not_blocked        TYPE funcname VALUE 'Z_NOT_BLOCKED',
             blocked            TYPE funcname VALUE 'Z_BLOCKED',
             triggers_rfc_error TYPE funcname VALUE 'ZTRIGGER_RFC_ERROR',
           END OF gc_function_modules.
CONSTANTS gc_rfc_blocklist TYPE devclass VALUE 'BLOCKLIST'.

CLASS ltcl_system_mock DEFINITION FOR TESTING.

  PUBLIC SECTION.
    INTERFACES zif_aoc_system PARTIALLY IMPLEMENTED.
ENDCLASS.


CLASS ltcl_x_not_supported DEFINITION
  INHERITING FROM cx_no_check FOR TESTING.
ENDCLASS.


CLASS ltcl_x_not_supported IMPLEMENTATION.
ENDCLASS.


CLASS ltcl_system_mock IMPLEMENTATION.
  METHOD zif_aoc_system~is_function_module_rfc_blocked.
    IF iv_blocklist_package <> gc_rfc_blocklist.
      RAISE EXCEPTION TYPE ltcl_x_not_supported.
    ENDIF.

    CASE iv_function_module_name.
      WHEN gc_function_modules-blocked.
        rv_result = abap_true.
      WHEN gc_function_modules-not_blocked.
        rv_result = abap_false.
      WHEN gc_function_modules-triggers_rfc_error.
        RAISE EXCEPTION TYPE zcx_aoc_rfc_error.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE ltcl_x_not_supported.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.


CLASS ltcl_test DEFINITION
  FINAL
  FOR TESTING
  RISK LEVEL HARMLESS
  DURATION SHORT.

  PRIVATE SECTION.
    DATA mt_code TYPE string_table.
    DATA ms_result TYPE scirest_ad.
    DATA mo_check TYPE REF TO zcl_aoc_check_105.

    METHODS setup.

    METHODS assert_found_on_blocklist.

    METHODS assert_no_error_code.
    METHODS execute_check.

    METHODS export_import FOR TESTING.
    METHODS without_destination FOR TESTING.
    METHODS not_confused_with_parameter_1 FOR TESTING RAISING cx_static_check.
    METHODS not_confused_with_parameter_2 FOR TESTING RAISING cx_static_check.
    METHODS not_confused_with_task_name FOR TESTING RAISING cx_static_check.
    METHODS ignore_dynamic_function_name FOR TESTING RAISING cx_static_check.
    METHODS handle_alternative_notation FOR TESTING RAISING cx_static_check.
    METHODS handle_rfc_error FOR TESTING RAISING cx_static_check.
    METHODS function_module_rfc_blocked FOR TESTING RAISING cx_static_check.
    METHODS function_module_not_blocked FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.
  METHOD setup.
    DATA lv_attributes TYPE xstring.

    mo_check = NEW #( NEW ltcl_system_mock( ) ).

    EXPORT mv_errty                 = 'E'
           mv_rfc_blocklist_package = gc_rfc_blocklist
           TO DATA BUFFER lv_attributes.

    mo_check->put_attributes( lv_attributes ).

    zcl_aoc_unit_test=>set_check( mo_check ).
  ENDMETHOD.


  METHOD execute_check.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
  ENDMETHOD.


  METHOD export_import.
    zcl_aoc_unit_test=>export_import( mo_check ).
  ENDMETHOD.


  METHOD assert_found_on_blocklist.
    cl_abap_unit_assert=>assert_equals( exp = gc_code-found_on_blocklist
                                        act = ms_result-code ).
  ENDMETHOD.


  METHOD assert_no_error_code.
    cl_abap_unit_assert=>assert_initial( ms_result ).
  ENDMETHOD.


  METHOD function_module_rfc_blocked.
    " Given
    INSERT |CALL FUNCTION '{ gc_function_modules-blocked }' DESTINATION 'RFC'.| INTO TABLE mt_code.

    " When
    execute_check( ).

    " Then
    assert_found_on_blocklist( ).
  ENDMETHOD.


  METHOD function_module_not_blocked.
    " Given
    INSERT |CALL FUNCTION '{ gc_function_modules-not_blocked }' DESTINATION 'RFC'.| INTO TABLE mt_code.

    " When
    execute_check( ).

    " Then
    assert_no_error_code( ).
  ENDMETHOD.


  METHOD without_destination.
    " Given
    INSERT |CALL FUNCTION '{ gc_function_modules-blocked }'.| INTO TABLE mt_code.

    " When
    execute_check( ).

    " Then: We don't care if DESTINATION was not used
    assert_no_error_code( ).
  ENDMETHOD.


  METHOD not_confused_with_parameter_1.
    " Given: The first parameter is named destination, but the function is not called via RFC
    INSERT |CALL FUNCTION '{ gc_function_modules-blocked }' EXPORTING destination = 'A'.| INTO TABLE mt_code.

    " When
    execute_check( ).

    " Then
    assert_no_error_code( ).
  ENDMETHOD.


  METHOD not_confused_with_parameter_2.
    " Given: The second parameter is named destination, but the function is not called via RFC
    INSERT |CALL FUNCTION '{ gc_function_modules-blocked }' EXPORTING a = 'A' destination = 'A'.|
           INTO TABLE mt_code.

    " When
    execute_check( ).

    " Then
    assert_no_error_code( ).
  ENDMETHOD.


  METHOD not_confused_with_task_name.
    " Given: A task is named destination, but the function is not called via RFC (far-fetched, but possible)
    INSERT |CALL FUNCTION '{ gc_function_modules-blocked }' STARTING NEW TASK destination.| INTO TABLE mt_code.

    " When
    execute_check( ).

    " Then
    assert_no_error_code( ).
  ENDMETHOD.


  METHOD ignore_dynamic_function_name.
    " Given
    INSERT |CALL FUNCTION variable_name DESTINATION 'RFC'.| INTO TABLE mt_code.

    " When
    execute_check( ).

    " Then
    assert_no_error_code( ).
  ENDMETHOD.


  METHOD handle_alternative_notation.
    " Given: Like EXISTING_FUNCTION_RFC_DISABLED, but ` is used instead of the typical '
    INSERT |CALL FUNCTION `{ gc_function_modules-blocked }` DESTINATION 'RFC'.| INTO TABLE mt_code.

    " When
    execute_check( ).

    " Then
    assert_found_on_blocklist( ).
  ENDMETHOD.


  METHOD handle_rfc_error.
    cl_abap_unit_assert=>skip( 'Implement this when ZCX_AOC_RFC_ERROR was changed to an unchecked exception' ).

    " Given: System mock is set up to raise an RFC error for this function module
    INSERT |CALL FUNCTION '{ gc_function_modules-triggers_rfc_error }' DESTINATION 'RFC'.| INTO TABLE mt_code.

    " When
    execute_check( ).
  ENDMETHOD.
ENDCLASS.
