"! <p class="shorttext synchronized">102 - Use of system ID</p>
CLASS zcl_aoc_check_102 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  FINAL.

  PUBLIC SECTION.
    METHODS constructor.

    METHODS check REDEFINITION.
ENDCLASS.


CLASS zcl_aoc_check_102 IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    version = '001'.
    position = '102'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    insert_scimessage( iv_code = gc_code-usage_uncategorized
                       iv_text = TEXT-001 ).
    insert_scimessage( iv_code = gc_code-in_condition
                       iv_text = TEXT-002 ).
    insert_scimessage( iv_code = gc_code-first_letter_used
                       iv_text = TEXT-003 ).
    insert_scimessage( iv_code = gc_code-as_default_value
                       iv_text = TEXT-004 ).
    insert_scimessage( iv_code = gc_code-in_concatenate
                       iv_text = TEXT-005 ).
    insert_scimessage( iv_code = gc_code-overridden
                       iv_text = TEXT-006 ).
    insert_scimessage( iv_code = gc_code-assigned_to_variable
                       iv_text = TEXT-007 ).
    insert_scimessage( iv_code = gc_code-in_database_select
                       iv_text = TEXT-008 ).
    insert_scimessage( iv_code = gc_code-in_write
                       iv_text = TEXT-009 ).
    insert_scimessage( iv_code = gc_code-in_message
                       iv_text = TEXT-010 ).
    insert_scimessage( iv_code = gc_code-within_macro
                       iv_text = TEXT-011 ).
  ENDMETHOD.

  METHOD check.
    " abapOpenChecks
    " https://github.com/larshp/abapOpenChecks
    " MIT License

    DATA(lo_helper) = NEW lcl_check_helper( io_scan ).

    LOOP AT io_scan->statements ASSIGNING FIELD-SYMBOL(<ls_statement>).
      LOOP AT io_scan->tokens ASSIGNING FIELD-SYMBOL(<ls_token>)
           FROM <ls_statement>-from TO <ls_statement>-to
           WHERE str CP 'SY-SYSID*'
              OR str CP '@SY-SYSID*'.

        DATA(lv_index_token) = sy-tabix.

        DATA(lv_error_code) = lo_helper->determine_error_code( is_token       = <ls_token>
                                                               iv_index_token = lv_index_token
                                                               is_statement   = <ls_statement> ).

        IF lv_error_code IS INITIAL.
          " No error
          CONTINUE.
        ENDIF.

        DATA(lv_include) = io_scan->get_include( <ls_statement>-level ).

        inform( p_sub_obj_name = lv_include
                p_line         = <ls_token>-row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = lv_error_code ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
