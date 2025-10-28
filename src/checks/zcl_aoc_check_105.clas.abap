"! <p class="shorttext synchronized">105 - RFC blocklist</p>
CLASS zcl_aoc_check_105 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_system TYPE REF TO zif_aoc_system OPTIONAL.

    METHODS check REDEFINITION.

    METHODS put_attributes REDEFINITION.

    METHODS if_ci_test~query_attributes REDEFINITION.

    METHODS get_attributes REDEFINITION.

  PRIVATE SECTION.
    DATA mo_system TYPE REF TO zif_aoc_system.
    DATA mo_function_module_helper TYPE REF TO zcl_aoc_function_module_helper.
    DATA mv_rfc_blocklist_package TYPE devclass.
ENDCLASS.


CLASS zcl_aoc_check_105 IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    enable_rfc( ).

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    DATA(lv_destination) = zcl_aoc_super=>get_destination( ).

    mo_system = COND #( WHEN io_system IS BOUND
                        THEN io_system
                        ELSE NEW zcl_aoc_system( lv_destination ) ).
    mo_function_module_helper = NEW #( ).

    version = '001'.
    position = '105'.

    insert_scimessage( iv_code = gc_code-found_on_blocklist
                       iv_text = TEXT-001 ).
  ENDMETHOD.


  METHOD check.
    LOOP AT io_scan->statements ASSIGNING FIELD-SYMBOL(<ls_statement>).
      LOOP AT io_scan->tokens
           TRANSPORTING NO FIELDS
           FROM <ls_statement>-from TO <ls_statement>-to
           WHERE str = zcl_aoc_scan=>gc_keyword-destination.

        DATA(lv_token_index) = sy-tabix.

        DATA(lv_is_destination_in_rfc_call) = mo_function_module_helper->is_destination_in_rfc_call(
                                                  io_scan        = io_scan
                                                  is_statement   = <ls_statement>
                                                  iv_token_index = lv_token_index ).

        IF lv_is_destination_in_rfc_call = abap_false.
          CONTINUE.
        ENDIF.

        ASSIGN io_scan->tokens[ <ls_statement>-from + 2 ] TO FIELD-SYMBOL(<ls_token_function_name>).

        IF mo_function_module_helper->is_dynamic_func_module_name( <ls_token_function_name>-str ) = abap_true.
          CONTINUE.
        ENDIF.

        DATA(lv_function_module_name_length) = strlen( <ls_token_function_name>-str ) - 2.
        DATA(lv_function_module_name) = <ls_token_function_name>-str+1(lv_function_module_name_length).

        " TODO: Convert ZCX_AOC_RFC_ERROR to an unchecked exception.
        "       We want the runtime error to cause a check error instead of a regular finding.
        DATA(lv_is_rfc_blocked) = mo_system->is_function_module_rfc_blocked(
                                      iv_function_module_name = CONV #( lv_function_module_name )
                                      iv_blocklist_package    = mv_rfc_blocklist_package ).

        IF lv_is_rfc_blocked = abap_true.
          DATA(lv_include) = io_scan->get_include( <ls_statement>-level ).

          inform( p_sub_obj_name = lv_include
                  p_line         = <ls_token_function_name>-row
                  p_kind         = mv_errty
                  p_test         = myname
                  p_code         = gc_code-found_on_blocklist
                  p_param_1      = lv_function_module_name ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


  METHOD get_attributes.
    EXPORT mv_errty                 = mv_errty
           mv_rfc_blocklist_package = mv_rfc_blocklist_package
           TO DATA BUFFER p_attributes.
  ENDMETHOD.


  METHOD if_ci_test~query_attributes.
    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.
    zzaoc_fill_att mv_rfc_blocklist_package 'RFC Blocklist Package' ''.

    zzaoc_popup.

    IF mv_rfc_blocklist_package IS INITIAL.
      attributes_ok = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD put_attributes.
    IMPORT mv_errty                 = mv_errty
           mv_rfc_blocklist_package = mv_rfc_blocklist_package
           FROM DATA BUFFER p_attributes.
    ASSERT sy-subrc = 0.
  ENDMETHOD.
ENDCLASS.
