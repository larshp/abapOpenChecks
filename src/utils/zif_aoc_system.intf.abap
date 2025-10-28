"! <p class="shorttext synchronized">System</p>
INTERFACE zif_aoc_system
  PUBLIC.
  METHODS is_function_module_rfc_enabled
    IMPORTING
      iv_function_module_name TYPE funcname
    RETURNING
      VALUE(rv_result)        TYPE abap_bool
    RAISING
      zcx_aoc_object_not_found
      zcx_aoc_rfc_error.

  METHODS is_function_module_rfc_blocked
    IMPORTING
      iv_function_module_name TYPE funcname
      iv_blocklist_package    TYPE devclass
    RETURNING
      VALUE(rv_result)        TYPE abap_bool
    RAISING
      zcx_aoc_rfc_error.

ENDINTERFACE.
