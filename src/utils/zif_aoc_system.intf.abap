INTERFACE zif_aoc_system
  PUBLIC .
  METHODS is_function_module_existing
    IMPORTING
      iv_function_module_name TYPE funcname
    RETURNING
      VALUE(rv_result)        TYPE abap_bool.
ENDINTERFACE.
