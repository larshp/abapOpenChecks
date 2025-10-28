"! <p class="shorttext synchronized">Helper class for function modules</p>
CLASS zcl_aoc_function_module_helper DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS is_destination_in_rfc_call
      IMPORTING
        io_scan          TYPE REF TO zcl_aoc_scan
        is_statement     TYPE sstmnt
        iv_token_index   TYPE syst_tabix
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS is_dynamic_func_module_name
      IMPORTING
        iv_name          TYPE string
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aoc_function_module_helper IMPLEMENTATION.
  METHOD is_destination_in_rfc_call.
    TRY.
        IF io_scan->tokens[ is_statement-from ]-str <> zcl_aoc_scan=>gc_keyword-call
            OR io_scan->tokens[ is_statement-from + 1 ]-str <> zcl_aoc_scan=>gc_keyword-function.
          " The token is not in a CALL FUNCTION statement
          RETURN.
        ENDIF.

        " DESTINATION is expected to be the 4th token.
        " But if the addition STARTING NEW TASK is used, then it can be the 8th token
        IF iv_token_index <> is_statement-from + 3.
          IF iv_token_index <> is_statement-from + 7.
            RETURN.
          ENDIF.

          IF io_scan->tokens[ is_statement-from + 3 ]-str <> zcl_aoc_scan=>gc_keyword-starting.
            " The token is not the DESTINATION keyword
            RETURN.
          ENDIF.
        ENDIF.
      CATCH cx_sy_itab_line_not_found.
        RETURN.
    ENDTRY.

    " All previous checks have passed
    rv_result = abap_true.
  ENDMETHOD.


  METHOD is_dynamic_func_module_name.
    IF iv_name(1) <> '`' AND iv_name(1) <> `'`.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
