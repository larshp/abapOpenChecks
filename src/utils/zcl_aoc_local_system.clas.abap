"! <p class="shorttext synchronized">Local system</p>
CLASS zcl_aoc_local_system DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_aoc_system.
ENDCLASS.


CLASS zcl_aoc_local_system IMPLEMENTATION.
  METHOD zif_aoc_system~is_function_module_existing.
    SELECT SINGLE @abap_true
      FROM tfdir
      WHERE funcname = @iv_function_module_name
      INTO @rv_result.
  ENDMETHOD.
ENDCLASS.
