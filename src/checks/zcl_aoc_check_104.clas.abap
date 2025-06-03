"! <p class="shorttext synchronized">104 - Local existence of function modules called via RFC</p>
CLASS zcl_aoc_check_104 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  FINAL.

  PUBLIC SECTION.
    METHODS constructor
      IMPORTING
        io_system TYPE REF TO zif_aoc_system OPTIONAL.

    METHODS check REDEFINITION.

  PRIVATE SECTION.
    DATA mo_system TYPE REF TO zif_aoc_system.

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
ENDCLASS.


CLASS zcl_aoc_check_104 IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    enable_rfc( ).

    DATA(lv_destination) = zcl_aoc_super=>get_destination( ).

    mo_system = COND #( WHEN io_system IS BOUND
                        THEN io_system
                        ELSE NEW zcl_aoc_system( lv_destination ) ).

    version = '001'.
    position = '104'.

    insert_scimessage( iv_code = gc_code-function_module_does_not_exist
                       iv_text = TEXT-001 ).

    insert_scimessage( iv_code = gc_code-rfc_not_enabled
                       iv_text = TEXT-002 ).

    insert_scimessage( iv_code = gc_code-rfc_error
                       iv_text = TEXT-003 ).
  ENDMETHOD.


  METHOD check.
    LOOP AT io_scan->statements ASSIGNING FIELD-SYMBOL(<ls_statement>).
      LOOP AT io_scan->tokens
           TRANSPORTING NO FIELDS
           FROM <ls_statement>-from TO <ls_statement>-to
           WHERE str = zcl_aoc_scan=>gc_keyword-destination.

        DATA(lv_token_index) = sy-tabix.

        IF is_destination_in_rfc_call( io_scan        = io_scan
                                       is_statement   = <ls_statement>
                                       iv_token_index = lv_token_index ) = abap_false.
          CONTINUE.
        ENDIF.

        ASSIGN io_scan->tokens[ <ls_statement>-from + 2 ] TO FIELD-SYMBOL(<ls_token_function_name>).

        IF is_dynamic_func_module_name( <ls_token_function_name>-str ) = abap_true.
          CONTINUE.
        ENDIF.

        DATA(lv_function_module_name_length) = strlen( <ls_token_function_name>-str ) - 2.
        DATA(lv_function_module_name) = <ls_token_function_name>-str+1(lv_function_module_name_length).

        DATA(lv_error_code) = VALUE sci_errc( ).

        TRY.
            IF mo_system->is_function_module_rfc_enabled( CONV #( lv_function_module_name ) ) = abap_false.
              lv_error_code = gc_code-rfc_not_enabled.
            ELSE.
              CONTINUE.
            ENDIF.
          CATCH zcx_aoc_object_not_found.
            lv_error_code = gc_code-function_module_does_not_exist.
          CATCH zcx_aoc_rfc_error.
            lv_error_code = gc_code-rfc_error.
        ENDTRY.

        DATA(lv_include) = io_scan->get_include( <ls_statement>-level ).

        inform( p_sub_obj_name = lv_include
                p_line         = <ls_token_function_name>-row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = lv_error_code
                p_param_1      = lv_function_module_name ).
      ENDLOOP.
    ENDLOOP.
  ENDMETHOD.


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
