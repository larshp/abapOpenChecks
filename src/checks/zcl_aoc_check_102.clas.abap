"! <p class="shorttext synchronized">102 - Use of SY-SYSID</p>
CLASS zcl_aoc_check_102 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  FINAL.

  PUBLIC SECTION.
    METHODS constructor.

    METHODS check REDEFINITION.

    METHODS put_attributes REDEFINITION.

    METHODS if_ci_test~query_attributes REDEFINITION.

    METHODS get_attributes REDEFINITION.

  PROTECTED SECTION.
    METHODS inform REDEFINITION.

  PRIVATE SECTION.
    TYPES:
      BEGIN OF ty_s_usage_kind_map,
        usage_kind TYPE zcl_aoc_sy_variable_analyzer=>ty_v_usage_kind,
        error_code TYPE sci_errc,
        text       TYPE ty_scimessage_text,
      END OF ty_s_usage_kind_map.

    DATA mt_usage_kind_map TYPE TABLE OF ty_s_usage_kind_map.

    DATA: BEGIN OF ms_error_types,
            usage_uncategorized     TYPE sci_errty,
            in_condition            TYPE sci_errty,
            first_letter_used       TYPE sci_errty,
            as_default_value        TYPE sci_errty,
            in_concatenate          TYPE sci_errty,
            overridden              TYPE sci_errty,
            assigned_to_variable    TYPE sci_errty,
            in_database_select      TYPE sci_errty,
            in_write                TYPE sci_errty,
            in_message              TYPE sci_errty,
            within_macro            TYPE sci_errty,
            in_function_module_call TYPE sci_errty,
          END OF ms_error_types.

    METHODS is_using_only_first_letter
      IMPORTING
        is_token         TYPE stokesx
      RETURNING
        VALUE(rv_result) TYPE abap_bool.

    METHODS get_shortened_text
      IMPORTING
        iv_error_code    TYPE sci_errc
      RETURNING
        VALUE(rv_result) TYPE char30.
ENDCLASS.


CLASS zcl_aoc_check_102 IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    version = '003'.
    position = '102'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_checksum( ).
    enable_rfc( ).

    mt_usage_kind_map = VALUE #( ( usage_kind = zcl_aoc_sy_variable_analyzer=>gc_usage_kind-usage_uncategorized
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

    LOOP AT mt_usage_kind_map ASSIGNING FIELD-SYMBOL(<ls_error_code_map>).
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

      ASSIGN mt_usage_kind_map[ usage_kind = <ls_variable_usage>-usage_kind ] TO FIELD-SYMBOL(<ls_usage_kind_map>).

      IF sy-subrc <> 0.
        " Ignore this usage kind
        CONTINUE.
      ENDIF.

      lv_error_code = <ls_usage_kind_map>-error_code.

      ASSIGN io_scan->statements[ <ls_variable_usage>-statement_index ] TO FIELD-SYMBOL(<ls_statement>).
      ASSIGN io_scan->tokens[ <ls_variable_usage>-token_index ] TO FIELD-SYMBOL(<ls_token>).

      IF is_using_only_first_letter( <ls_token> ) = abap_true.
        " This is more interesting than where it is used
        lv_error_code = gc_code-first_letter_used.
      ENDIF.

      DATA(lv_include) = io_scan->get_include( <ls_statement>-level ).

      inform( p_position     = <ls_variable_usage>-statement_index
              p_sub_obj_name = lv_include
              p_line         = <ls_token>-row
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


  METHOD if_ci_test~query_attributes.
    DATA(lv_text_usage_uncategorized) = get_shortened_text( gc_code-usage_uncategorized ).
    DATA(lv_text_in_condition) = get_shortened_text( gc_code-in_condition ).
    DATA(lv_text_first_letter_used) = get_shortened_text( gc_code-first_letter_used ).
    DATA(lv_text_as_default_value) = get_shortened_text( gc_code-as_default_value ).
    DATA(lv_text_in_concatenate) = get_shortened_text( gc_code-in_concatenate ).
    DATA(lv_text_overridden) = get_shortened_text( gc_code-overridden ).
    DATA(lv_text_assigned_to_variable) = get_shortened_text( gc_code-assigned_to_variable ).
    DATA(lv_text_in_database_select) = get_shortened_text( gc_code-in_database_select ).
    DATA(lv_text_in_write) = get_shortened_text( gc_code-in_write ).
    DATA(lv_text_in_message) = get_shortened_text( gc_code-in_message ).
    DATA(lv_text_within_macro) = get_shortened_text( gc_code-within_macro ).
    DATA(lv_text_in_function_call) = get_shortened_text( gc_code-in_function_module_call ).

    zzaoc_top.

    zzaoc_fill_att mv_errty                               'Base Error Type' ''.
    zzaoc_fill_att ms_error_types-usage_uncategorized     lv_text_usage_uncategorized  ''.
    zzaoc_fill_att ms_error_types-in_condition            lv_text_in_condition         ''.
    zzaoc_fill_att ms_error_types-first_letter_used       lv_text_first_letter_used    ''.
    zzaoc_fill_att ms_error_types-as_default_value        lv_text_as_default_value     ''.
    zzaoc_fill_att ms_error_types-in_concatenate          lv_text_in_concatenate       ''.
    zzaoc_fill_att ms_error_types-overridden              lv_text_overridden           ''.
    zzaoc_fill_att ms_error_types-assigned_to_variable    lv_text_assigned_to_variable ''.
    zzaoc_fill_att ms_error_types-in_database_select      lv_text_in_database_select   ''.
    zzaoc_fill_att ms_error_types-in_write                lv_text_in_write             ''.
    zzaoc_fill_att ms_error_types-in_message              lv_text_in_message           ''.
    zzaoc_fill_att ms_error_types-within_macro            lv_text_within_macro         ''.
    zzaoc_fill_att ms_error_types-in_function_module_call lv_text_in_function_call     ''.

    zzaoc_popup.
  ENDMETHOD.


  METHOD put_attributes.
    IMPORT mv_errty                               = mv_errty
           ms_error_types-usage_uncategorized     = ms_error_types-usage_uncategorized
           ms_error_types-in_condition            = ms_error_types-in_condition
           ms_error_types-first_letter_used       = ms_error_types-first_letter_used
           ms_error_types-as_default_value        = ms_error_types-as_default_value
           ms_error_types-in_concatenate          = ms_error_types-in_concatenate
           ms_error_types-overridden              = ms_error_types-overridden
           ms_error_types-assigned_to_variable    = ms_error_types-assigned_to_variable
           ms_error_types-in_database_select      = ms_error_types-in_database_select
           ms_error_types-in_write                = ms_error_types-in_write
           ms_error_types-in_message              = ms_error_types-in_message
           ms_error_types-within_macro            = ms_error_types-within_macro
           ms_error_types-in_function_module_call = ms_error_types-in_function_module_call
           FROM DATA BUFFER p_attributes.

    ASSERT sy-subrc = 0.
  ENDMETHOD.


  METHOD get_attributes.
    EXPORT mv_errty                               = mv_errty
           ms_error_types-usage_uncategorized     = ms_error_types-usage_uncategorized
           ms_error_types-in_condition            = ms_error_types-in_condition
           ms_error_types-first_letter_used       = ms_error_types-first_letter_used
           ms_error_types-as_default_value        = ms_error_types-as_default_value
           ms_error_types-in_concatenate          = ms_error_types-in_concatenate
           ms_error_types-overridden              = ms_error_types-overridden
           ms_error_types-assigned_to_variable    = ms_error_types-assigned_to_variable
           ms_error_types-in_database_select      = ms_error_types-in_database_select
           ms_error_types-in_write                = ms_error_types-in_write
           ms_error_types-in_message              = ms_error_types-in_message
           ms_error_types-within_macro            = ms_error_types-within_macro
           ms_error_types-in_function_module_call = ms_error_types-in_function_module_call
           TO DATA BUFFER p_attributes.
  ENDMETHOD.


  METHOD inform.
    DATA(lv_error_type_override) = SWITCH #( p_code
                                             WHEN gc_code-usage_uncategorized
                                               THEN ms_error_types-usage_uncategorized
                                             WHEN gc_code-in_condition
                                               THEN ms_error_types-in_condition
                                             WHEN gc_code-first_letter_used
                                               THEN ms_error_types-first_letter_used
                                             WHEN gc_code-as_default_value
                                               THEN ms_error_types-as_default_value
                                             WHEN gc_code-in_concatenate
                                               THEN ms_error_types-in_concatenate
                                             WHEN gc_code-overridden
                                               THEN ms_error_types-overridden
                                             WHEN gc_code-assigned_to_variable
                                               THEN ms_error_types-assigned_to_variable
                                             WHEN gc_code-in_database_select
                                               THEN ms_error_types-in_database_select
                                             WHEN gc_code-in_write
                                               THEN ms_error_types-in_write
                                             WHEN gc_code-in_message
                                               THEN ms_error_types-in_message
                                             WHEN gc_code-within_macro
                                               THEN ms_error_types-within_macro
                                             WHEN gc_code-in_function_module_call
                                               THEN ms_error_types-in_function_module_call ).

    DATA(lv_error_type) = COND #( WHEN lv_error_type_override IS NOT INITIAL
                                  THEN lv_error_type_override
                                  ELSE mv_errty ).

    super->inform( p_sub_obj_type = p_sub_obj_type
                   p_sub_obj_name = p_sub_obj_name
                   p_position     = p_position
                   p_line         = p_line
                   p_column       = p_column
                   p_errcnt       = p_errcnt
                   p_kind         = lv_error_type
                   p_test         = p_test
                   p_code         = p_code
                   p_suppress     = p_suppress
                   p_param_1      = p_param_1
                   p_param_2      = p_param_2
                   p_param_3      = p_param_3
                   p_param_4      = p_param_4
                   p_inclspec     = p_inclspec
                   p_detail       = p_detail
                   p_checksum_1   = p_checksum_1 ).
  ENDMETHOD.


  METHOD get_shortened_text.
    DATA(lv_text) = scimessages[ code = iv_error_code ]-text.
    ASSERT sy-subrc = 0.

    rv_result = COND #( WHEN strlen( lv_text ) > 30
                        THEN |{ lv_text(27) }...|
                        ELSE lv_text ).
  ENDMETHOD.
ENDCLASS.
