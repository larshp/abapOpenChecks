"! <p class="shorttext synchronized">System</p>
CLASS zcl_aoc_system DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        iv_rfc_destination TYPE rfcdest.

    INTERFACES zif_aoc_system.

  PRIVATE SECTION.
    DATA mv_rfc_destination TYPE rfcdest.
ENDCLASS.


CLASS zcl_aoc_system IMPLEMENTATION.
  METHOD constructor.
    mv_rfc_destination = iv_rfc_destination.
  ENDMETHOD.


  METHOD zif_aoc_system~is_function_module_rfc_enabled.
    CALL FUNCTION 'ZAOC_IS_FUNCTION_MODULE_RFC'
      DESTINATION mv_rfc_destination
      EXPORTING
        iv_function_module_name = iv_function_module_name
      IMPORTING
        ev_is_rfc_enabled       = rv_result
      EXCEPTIONS
        not_found               = 1
        system_failure          = 2
        communication_failure   = 3.

    DATA(lv_subrc) = sy-subrc.

    CASE lv_subrc.
      WHEN 0.
        RETURN.
      WHEN 1.
        RAISE EXCEPTION TYPE zcx_aoc_object_not_found.
      WHEN OTHERS.
        RAISE EXCEPTION TYPE zcx_aoc_rfc_error.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
