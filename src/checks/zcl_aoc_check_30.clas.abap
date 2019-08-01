CLASS zcl_aoc_check_30 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS check
        REDEFINITION.
    METHODS get_message_text
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_30 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    TYPES: BEGIN OF ty_stack,
             exporting  TYPE abap_bool,
             importing  TYPE abap_bool,
             changing   TYPE abap_bool,
             receiv     TYPE abap_bool,
             exceptions TYPE abap_bool,
             row        TYPE token_row,
           END OF ty_stack.

    DATA: lv_i       TYPE i,
          lt_stack   TYPE TABLE OF ty_stack,
          ls_stack   LIKE LINE OF lt_stack,
          lv_include TYPE sobj_name.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF io_scan->statements,
                   <ls_token>     LIKE LINE OF io_scan->tokens.


    LOOP AT io_scan->statements ASSIGNING <ls_statement>
        WHERE type = scan_stmnt_type-standard
        OR type = scan_stmnt_type-compute_direct
        OR type = scan_stmnt_type-method_direct.

      LOOP AT io_scan->tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from TO <ls_statement>-to
          WHERE type = scan_token_type-identifier.

        lv_i = strlen( <ls_token>-str ) - 1.
        IF <ls_token>-str+lv_i(1) = '('.
* push
          APPEND ls_stack TO lt_stack.
          CLEAR ls_stack.
        ELSEIF <ls_token>-str(1) = ')'.
          IF ls_stack-exporting = abap_true
              AND ls_stack-importing = abap_false
              AND ls_stack-receiv = abap_false
              AND ls_stack-exceptions = abap_false
              AND ls_stack-changing = abap_false.
            lv_include = io_scan->get_include( <ls_statement>-level ).
            inform( p_sub_obj_type = c_type_include
                    p_sub_obj_name = lv_include
                    p_line         = ls_stack-row
                    p_kind         = mv_errty
                    p_test         = myname
                    p_code         = '001' ).
          ENDIF.

* pop
          lv_i = lines( lt_stack ).
          ASSERT lv_i > 0.
          READ TABLE lt_stack INDEX lv_i INTO ls_stack.
          ASSERT sy-subrc = 0.
          DELETE lt_stack INDEX lv_i.
        ELSEIF <ls_token>-str = 'EXPORTING'.
          ls_stack-exporting = abap_true.
          ls_stack-row = <ls_token>-row.
        ELSEIF <ls_token>-str = 'IMPORTING'.
          ls_stack-importing = abap_true.
        ELSEIF <ls_token>-str = 'RECEIVING'.
          ls_stack-receiv = abap_true.
        ELSEIF <ls_token>-str = 'EXCEPTIONS'.
          ls_stack-exceptions = abap_true.
        ELSEIF <ls_token>-str = 'CHANGING'.
          ls_stack-changing = abap_true.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '030'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    mv_errty = c_error.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'EXPORTING can be omitted'.                "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.                    "GET_MESSAGE_TEXT
ENDCLASS.
