"! <p class="shorttext synchronized">Analyzer for system variables (SY structure)</p>
CLASS zcl_aoc_sy_variable_analyzer DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPES ty_v_usage_kind TYPE i.

    TYPES: BEGIN OF ty_s_result,
             statement_index TYPE sy-tabix,
             token_index     TYPE sy-tabix,
             usage_kind      TYPE ty_v_usage_kind,
           END OF ty_s_result.

    TYPES ty_t_result TYPE TABLE OF ty_s_result WITH EMPTY KEY.

    CONSTANTS:
      BEGIN OF gc_usage_kind,
        usage_uncategorized     TYPE ty_v_usage_kind VALUE 1,
        in_condition            TYPE ty_v_usage_kind VALUE 2,
        type_definition         TYPE ty_v_usage_kind VALUE 3,
        as_default_value        TYPE ty_v_usage_kind VALUE 4,
        in_concatenate          TYPE ty_v_usage_kind VALUE 5,
        overridden              TYPE ty_v_usage_kind VALUE 6,
        assigned_to_variable    TYPE ty_v_usage_kind VALUE 7,
        in_database_select      TYPE ty_v_usage_kind VALUE 8,
        in_write                TYPE ty_v_usage_kind VALUE 9,
        in_message              TYPE ty_v_usage_kind VALUE 10,
        within_macro            TYPE ty_v_usage_kind VALUE 11,
        in_function_module_call TYPE ty_v_usage_kind VALUE 12,
      END OF gc_usage_kind.

    METHODS constructor
      IMPORTING
        io_scan TYPE REF TO zcl_aoc_scan.

    METHODS analyze_variable_usage
      IMPORTING
        iv_sy_variable_name TYPE stokesx-str
      RETURNING
        VALUE(rt_result)    TYPE ty_t_result.

  PRIVATE SECTION.
    DATA mo_scan TYPE REF TO zcl_aoc_scan.

    METHODS determine_usage_kind
      IMPORTING
        is_token             TYPE stokesx
        iv_index_token       TYPE sy-tabix
        is_statement         TYPE sstmnt
      RETURNING
        VALUE(rv_usage_kind) TYPE ty_v_usage_kind.

    METHODS is_used_in_macro
      IMPORTING
        is_statement     TYPE sstmnt
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
ENDCLASS.


CLASS zcl_aoc_sy_variable_analyzer IMPLEMENTATION.
  METHOD constructor.
    mo_scan = io_scan.
  ENDMETHOD.

  METHOD analyze_variable_usage.
    LOOP AT mo_scan->statements ASSIGNING FIELD-SYMBOL(<ls_statement>).
      DATA(lv_statement_index) = sy-tabix.

      LOOP AT mo_scan->tokens ASSIGNING FIELD-SYMBOL(<ls_token>)
           FROM <ls_statement>-from TO <ls_statement>-to
           WHERE str CP |SY-{ iv_sy_variable_name }*|
              OR str CP |@SY-{ iv_sy_variable_name }*|.

        DATA(lv_token_index) = sy-tabix.

        DATA(lv_usage_kind) = determine_usage_kind( is_token       = <ls_token>
                                                    iv_index_token = lv_token_index
                                                    is_statement   = <ls_statement> ).

        INSERT VALUE #( statement_index = lv_statement_index
                        token_index     = lv_token_index
                        usage_kind      = lv_usage_kind ) INTO TABLE rt_result.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.

  METHOD determine_usage_kind.
    IF is_used_in_macro( is_statement ) = abap_true.
      " In macros, all actual statements are detected as a single statement.
      " That's way too complicated, so we just leave it at this...
      rv_usage_kind = gc_usage_kind-within_macro.
      RETURN.
    ENDIF.

    ASSIGN mo_scan->tokens[ iv_index_token - 1 ] TO FIELD-SYMBOL(<ls_token_before_sy_var>).
    ASSIGN mo_scan->tokens[ is_statement-from ] TO FIELD-SYMBOL(<ls_first_token_of_statement>).

    CASE <ls_first_token_of_statement>-str.
      WHEN zcl_aoc_scan=>gc_keyword-types
          OR zcl_aoc_scan=>gc_keyword-ranges.
        rv_usage_kind = gc_usage_kind-type_definition.
      WHEN zcl_aoc_scan=>gc_keyword-concatenate.
        rv_usage_kind = gc_usage_kind-in_concatenate.
      WHEN is_token-str.
        rv_usage_kind = gc_usage_kind-overridden.
      WHEN zcl_aoc_scan=>gc_keyword-select.
        rv_usage_kind = gc_usage_kind-in_database_select.
      WHEN zcl_aoc_scan=>gc_keyword-write.
        rv_usage_kind = gc_usage_kind-in_write.
      WHEN zcl_aoc_scan=>gc_keyword-message.
        rv_usage_kind = gc_usage_kind-in_message.
      WHEN zcl_aoc_scan=>gc_keyword-if
          OR zcl_aoc_scan=>gc_keyword-elseif
          OR zcl_aoc_scan=>gc_keyword-case
          OR zcl_aoc_scan=>gc_keyword-when
          OR zcl_aoc_scan=>gc_keyword-check
          OR zcl_aoc_scan=>gc_keyword-assert.
        rv_usage_kind = gc_usage_kind-in_condition.
      WHEN zcl_aoc_scan=>gc_keyword-methods
          OR zcl_aoc_scan=>gc_keyword-class_methods
          OR zcl_aoc_scan=>gc_keyword-data
          OR zcl_aoc_scan=>gc_keyword-class_data.

        CASE <ls_token_before_sy_var>-str.
          WHEN zcl_aoc_scan=>gc_keyword-default.
            rv_usage_kind = gc_usage_kind-as_default_value.
          WHEN OTHERS.
            rv_usage_kind = gc_usage_kind-type_definition.
        ENDCASE.
      WHEN zcl_aoc_scan=>gc_keyword-call.
        ASSIGN mo_scan->tokens[ is_statement-from + 1 ] TO FIELD-SYMBOL(<ls_second_token_of_statement>).
        IF <ls_second_token_of_statement>-str = zcl_aoc_scan=>gc_keyword-function.
          rv_usage_kind = gc_usage_kind-in_function_module_call.
        ELSE.
          rv_usage_kind = gc_usage_kind-usage_uncategorized.
        ENDIF.
      WHEN zcl_aoc_scan=>gc_keyword-move.
        IF <ls_token_before_sy_var>-str = zcl_aoc_scan=>gc_keyword-to.
          rv_usage_kind = gc_usage_kind-overridden.
        ELSE.
          ASSIGN mo_scan->tokens[ iv_index_token + 1 ] TO FIELD-SYMBOL(<ls_token_after_sy_var>).

          IF <ls_token_after_sy_var>-str = zcl_aoc_scan=>gc_keyword-to.
            rv_usage_kind = gc_usage_kind-assigned_to_variable.
          ELSE.
            rv_usage_kind = gc_usage_kind-usage_uncategorized.
          ENDIF.
        ENDIF.
      WHEN OTHERS.
        IF <ls_token_before_sy_var>-str            = '='
            AND iv_index_token - is_statement-from = 2.
          rv_usage_kind = gc_usage_kind-assigned_to_variable.
        ELSE.
          rv_usage_kind = gc_usage_kind-usage_uncategorized.
        ENDIF.
    ENDCASE.
  ENDMETHOD.

  METHOD is_used_in_macro.
    ASSIGN mo_scan->tokens[ is_statement-to + 1 ] TO FIELD-SYMBOL(<ls_token_after_statement>).

    IF sy-subrc = 0
        AND <ls_token_after_statement>-str = zcl_aoc_scan=>gc_keyword-end_of_definition.
      rv_result = abap_true.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
