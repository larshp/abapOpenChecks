CONSTANTS: BEGIN OF gc_function_modules,
             existing_rfc_enabled  TYPE funcname VALUE 'ZEXISTING_RFC_ENABLED',
             existing_rfc_disabled TYPE funcname VALUE 'ZEXISTING_RFC_DISABLED',
             not_existing          TYPE funcname VALUE 'ZNOTEXISTING',
             triggers_rfc_error    TYPE funcname VALUE 'ZTRIGGER_RFC_ERROR',
           END OF gc_function_modules.

CLASS ltcl_system_mock DEFINITION FOR TESTING.

  PUBLIC SECTION.
    INTERFACES zif_aoc_system.
ENDCLASS.


CLASS ltcl_x_not_supported DEFINITION
  INHERITING FROM cx_no_check FOR TESTING.
ENDCLASS.


CLASS ltcl_x_not_supported IMPLEMENTATION.
ENDCLASS.


CLASS ltcl_system_mock IMPLEMENTATION.
  METHOD zif_aoc_system~is_function_module_rfc_enabled.
    CASE iv_function_module_name.
      WHEN gc_function_modules-existing_rfc_enabled.
        rv_result = abap_true.
      WHEN gc_function_modules-existing_rfc_disabled.
        rv_result = abap_false.
      WHEN gc_function_modules-not_existing.
        RAISE EXCEPTION TYPE zcx_aoc_object_not_found.
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
    DATA mo_check TYPE REF TO zcl_aoc_check_104.

    METHODS setup.

    METHODS assert_error_code
      IMPORTING
        iv_expected_error_code TYPE sci_errc.

    METHODS assert_no_error_code.
    METHODS execute_check.

    METHODS export_import FOR TESTING.
    METHODS existing_function_rfc_enabled FOR TESTING.
    METHODS not_existing_function FOR TESTING.
    METHODS without_destination FOR TESTING.
    METHODS existing_function_rfc_disabled FOR TESTING RAISING cx_static_check.
    METHODS not_confused_with_parameter_1 FOR TESTING RAISING cx_static_check.
    METHODS not_confused_with_parameter_2 FOR TESTING RAISING cx_static_check.
    METHODS not_confused_with_task_name FOR TESTING RAISING cx_static_check.
    METHODS ignore_dynamic_function_name FOR TESTING RAISING cx_static_check.
    METHODS handle_alternative_notation FOR TESTING RAISING cx_static_check.
    METHODS handle_rfc_error FOR TESTING RAISING cx_static_check.
ENDCLASS.


CLASS ltcl_test IMPLEMENTATION.
  METHOD setup.
    mo_check = NEW #( NEW ltcl_system_mock( ) ).
    zcl_aoc_unit_test=>set_check( mo_check ).
  ENDMETHOD.


  METHOD execute_check.
    ms_result = zcl_aoc_unit_test=>check( mt_code ).
  ENDMETHOD.


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


  METHOD existing_function_rfc_enabled.
    " Given
    INSERT |CALL FUNCTION '{ gc_function_modules-existing_rfc_enabled }' DESTINATION 'RFC'.| INTO TABLE mt_code.

    " When
    execute_check( ).

    " Then
    assert_no_error_code( ).
  ENDMETHOD.


  METHOD existing_function_rfc_disabled.
    " Given
    INSERT |CALL FUNCTION '{ gc_function_modules-existing_rfc_disabled }' DESTINATION 'RFC'.| INTO TABLE mt_code.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-rfc_not_enabled ).
  ENDMETHOD.


  METHOD not_existing_function.
    " Given
    INSERT |CALL FUNCTION '{ gc_function_modules-not_existing }' DESTINATION 'RFC'.| INTO TABLE mt_code.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-function_module_does_not_exist ).
  ENDMETHOD.


  METHOD without_destination.
    " Given
    INSERT |CALL FUNCTION '{ gc_function_modules-not_existing }'.| INTO TABLE mt_code.

    " When
    execute_check( ).

    " Then: We don't care if DESTINATION was not used
    assert_no_error_code( ).
  ENDMETHOD.


  METHOD not_confused_with_parameter_1.
    " Given: The first parameter is named destination, but the function is not called via RFC
    INSERT |CALL FUNCTION '{ gc_function_modules-not_existing }' EXPORTING destination = 'A'.| INTO TABLE mt_code.

    " When
    execute_check( ).

    " Then
    assert_no_error_code( ).
  ENDMETHOD.


  METHOD not_confused_with_parameter_2.
    " Given: The second parameter is named destination, but the function is not called via RFC
    INSERT |CALL FUNCTION '{ gc_function_modules-not_existing }' EXPORTING a = 'A' destination = 'A'.|
           INTO TABLE mt_code.

    " When
    execute_check( ).

    " Then
    assert_no_error_code( ).
  ENDMETHOD.


  METHOD not_confused_with_task_name.
    " Given: A task is named destination, but the function is not called via RFC (far-fetched, but possible)
    INSERT |CALL FUNCTION '{ gc_function_modules-not_existing }' STARTING NEW TASK destination.| INTO TABLE mt_code.

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
    INSERT |CALL FUNCTION `{ gc_function_modules-existing_rfc_disabled }` DESTINATION 'RFC'.| INTO TABLE mt_code.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-rfc_not_enabled ).
  ENDMETHOD.

  METHOD handle_rfc_error.
    " Given: System mock is set up to raise an RFC error for this function module
    INSERT |CALL FUNCTION '{ gc_function_modules-triggers_rfc_error }' DESTINATION 'RFC'.| INTO TABLE mt_code.

    " When
    execute_check( ).

    " Then
    assert_error_code( gc_code-rfc_error ).
  ENDMETHOD.

ENDCLASS.
