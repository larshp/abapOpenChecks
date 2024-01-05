CLASS lcl_check_helper IMPLEMENTATION.
  METHOD constructor.
    mo_scan = io_scan.
  ENDMETHOD.

  METHOD determine_error_code.
    IF is_using_only_first_letter( is_token ) = abap_true.
      rv_error_code = gc_code-first_letter_used.
      RETURN.
    ENDIF.

    IF is_used_in_macro( is_statement ) = abap_true.
      rv_error_code = gc_code-within_macro.
      RETURN.
    ENDIF.

    ASSIGN mo_scan->tokens[ is_statement-from ] TO FIELD-SYMBOL(<ls_first_token_of_statement>).

    CASE <ls_first_token_of_statement>-str.
      WHEN zcl_aoc_scan=>gc_keyword-types
          OR zcl_aoc_scan=>gc_keyword-ranges.
        " Ignore
        RETURN.
      WHEN zcl_aoc_scan=>gc_keyword-concatenate.
        rv_error_code = gc_code-in_concatenate.
      WHEN is_token-str.
        rv_error_code = gc_code-overridden.
      WHEN zcl_aoc_scan=>gc_keyword-select.
        rv_error_code = gc_code-in_database_select.
      WHEN zcl_aoc_scan=>gc_keyword-write.
        rv_error_code = gc_code-in_write.
      WHEN zcl_aoc_scan=>gc_keyword-message.
        rv_error_code = gc_code-in_message.
      WHEN zcl_aoc_scan=>gc_keyword-if
          OR zcl_aoc_scan=>gc_keyword-elseif
          OR zcl_aoc_scan=>gc_keyword-case
          OR zcl_aoc_scan=>gc_keyword-when
          OR zcl_aoc_scan=>gc_keyword-check
          OR zcl_aoc_scan=>gc_keyword-assert.
        rv_error_code = gc_code-in_condition.
      WHEN zcl_aoc_scan=>gc_keyword-methods
          OR zcl_aoc_scan=>gc_keyword-class_methods
          OR zcl_aoc_scan=>gc_keyword-data
          OR zcl_aoc_scan=>gc_keyword-class_data.
        ASSIGN mo_scan->tokens[ iv_index_token - 1 ] TO FIELD-SYMBOL(<ls_token_before_sysid>).

        CASE <ls_token_before_sysid>-str.
          WHEN zcl_aoc_scan=>gc_keyword-default.
            rv_error_code = gc_code-as_default_value.
          WHEN zcl_aoc_scan=>gc_keyword-like
              OR zcl_aoc_scan=>gc_keyword-type.
            " Ignore
            RETURN.
        ENDCASE.
      WHEN OTHERS.
        READ TABLE mo_scan->tokens ASSIGNING <ls_token_before_sysid> INDEX iv_index_token - 1.

        IF <ls_token_before_sysid>-str = '='
           AND iv_index_token - is_statement-from = 2.
          rv_error_code = gc_code-assigned_to_variable.
        ELSE.
          rv_error_code = gc_code-usage_uncategorized.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD is_using_only_first_letter.
    rv_result = xsdbool( is_token-str = 'SY-SYSID+0(1)'
                         OR is_token-str = 'SY-SYSID(1)' ).
  ENDMETHOD.

  METHOD is_used_in_macro.
    ASSIGN mo_scan->tokens[ is_statement-to + 1 ] TO FIELD-SYMBOL(<ls_token_after_statement>).

    IF sy-subrc = 0
       AND <ls_token_after_statement>-str = zcl_aoc_scan=>gc_keyword-end_of_definition.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
