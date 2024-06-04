"! <p class="shorttext synchronized">102 - Use of SY-SYSID</p>
CLASS zcl_aoc_check_102 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  FINAL.

  PUBLIC SECTION.
    METHODS constructor.

    METHODS check REDEFINITION.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_s_error_code_map,
        usage_kind TYPE zcl_aoc_sy_variable_analyzer=>ty_v_usage_kind,
        error_code TYPE sci_errc,
        text       TYPE ty_scimessage_text,
      END OF ty_s_error_code_map.

    DATA mt_error_code_map TYPE TABLE OF ty_s_error_code_map.

    METHODS is_using_only_first_letter
      IMPORTING
        is_token         TYPE stokesx
      RETURNING
        VALUE(rv_result) TYPE abap_bool.
ENDCLASS.


CLASS zcl_aoc_check_102 IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    version = '002'.
    position = '102'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    mt_error_code_map = VALUE #( ( usage_kind = zcl_aoc_sy_variable_analyzer=>gc_usage_kind-usage_uncategorized
                                   error_code = gc_code-usage_uncategorized
                                   text       = TEXT-001 )
                                 ( usage_kind = zcl_aoc_sy_variable_analyzer=>gc_usage_kind-in_condition
                                   error_code = gc_code-in_condition
                                   text       = TEXT-002 )
                                 ( usage_kind = zcl_aoc_sy_variable_analyzer=>gc_usage_kind-as_default_value
                                   error_code = gc_code-as_default_value
                                   text       = TEXT-004 )
                                 ( usage_kind = zcl_aoc_sy_variable_analyzer=>gc_usage_kind-in_concatenate
                                   error_code = gc_code-in_concatenate
                                   text       = TEXT-005 )
                                 ( usage_kind = zcl_aoc_sy_variable_analyzer=>gc_usage_kind-overridden
                                   error_code = gc_code-overridden
                                   text       = TEXT-006 )
                                 ( usage_kind = zcl_aoc_sy_variable_analyzer=>gc_usage_kind-assigned_to_variable
                                   error_code = gc_code-assigned_to_variable
                                   text       = TEXT-007 )
                                 ( usage_kind = zcl_aoc_sy_variable_analyzer=>gc_usage_kind-in_database_select
                                   error_code = gc_code-in_database_select
                                   text       = TEXT-008 )
                                 ( usage_kind = zcl_aoc_sy_variable_analyzer=>gc_usage_kind-in_write
                                   error_code = gc_code-in_write
                                   text       = TEXT-009 )
                                 ( usage_kind = zcl_aoc_sy_variable_analyzer=>gc_usage_kind-in_message
                                   error_code = gc_code-in_message
                                   text       = TEXT-010 )
                                 ( usage_kind = zcl_aoc_sy_variable_analyzer=>gc_usage_kind-within_macro
                                   error_code = gc_code-within_macro
                                   text       = TEXT-011 )
                                 ( usage_kind = zcl_aoc_sy_variable_analyzer=>gc_usage_kind-in_function_module_call
                                   error_code = gc_code-in_function_module_call
                                   text       = TEXT-012 ) ).

    LOOP AT mt_error_code_map ASSIGNING FIELD-SYMBOL(<ls_error_code_map>).
      insert_scimessage( iv_code = <ls_error_code_map>-error_code
                         iv_text = <ls_error_code_map>-text ).
    ENDLOOP.

    insert_scimessage( iv_code = gc_code-first_letter_used
                       iv_text = TEXT-003 ).
  ENDMETHOD.

  METHOD check.
    " abapOpenChecks
    " https://github.com/larshp/abapOpenChecks
    " MIT License

    DATA(lo_variable_analyzer) = NEW zcl_aoc_sy_variable_analyzer( io_scan ).

    DATA(lt_variable_usage) = lo_variable_analyzer->analyze_variable_usage( 'SYSID' ).

    LOOP AT lt_variable_usage ASSIGNING FIELD-SYMBOL(<ls_variable_usage>).
      DATA(lv_error_code) = VALUE sci_errc( ).

      ASSIGN mt_error_code_map[ usage_kind = <ls_variable_usage>-usage_kind ] TO FIELD-SYMBOL(<ls_error_code_map>).

      IF sy-subrc <> 0.
        " Ignore this usage kind
        CONTINUE.
      ENDIF.

      lv_error_code = <ls_error_code_map>-error_code.

      ASSIGN io_scan->statements[ <ls_variable_usage>-statement_index ] TO FIELD-SYMBOL(<ls_statement>).
      ASSIGN io_scan->tokens[ <ls_variable_usage>-token_index ] TO FIELD-SYMBOL(<ls_token>).

      IF is_using_only_first_letter( <ls_token> ) = abap_true.
        " This is more interesting than where it is used
        lv_error_code = gc_code-first_letter_used.
      ENDIF.

      DATA(lv_include) = io_scan->get_include( <ls_statement>-level ).

      inform( p_sub_obj_name = lv_include
              p_line         = <ls_token>-row
              p_kind         = mv_errty
              p_test         = myname
              p_code         = lv_error_code ).

    ENDLOOP.
  ENDMETHOD.

  METHOD is_using_only_first_letter.
    rv_result = SWITCH #( is_token-str
                          WHEN `SY-SYSID+0(1)` OR `SY-SYSID(1)` OR `@SY-SYSID+0(1)` OR `@SY-SYSID(1)`
                          THEN abap_true
                          ELSE abap_false ).
  ENDMETHOD.
ENDCLASS.
