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
    DATA lv_dummy TYPE funcname ##NEEDED.
    SELECT SINGLE funcname
      FROM tfdir
      INTO lv_dummy
      WHERE funcname = iv_function_module_name.

    IF sy-subrc = 0.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
