CLASS zcl_aoc_check_85 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS check_compute
      IMPORTING
        !io_scan                 TYPE REF TO zcl_aoc_scan
        !is_statement            TYPE sstmnt
        !io_compiler             TYPE REF TO cl_abap_compiler
      CHANGING
        VALUE(cv_save_to_change) TYPE abap_bool .
    METHODS get_statement
      IMPORTING
        !it_tokens          TYPE stokesx_tab
        !is_statement       TYPE sstmnt
      RETURNING
        VALUE(rv_statement) TYPE string .
ENDCLASS.



CLASS ZCL_AOC_CHECK_85 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA:
      lo_compiler       TYPE REF TO cl_abap_compiler,
      lv_save_to_change TYPE abap_bool,
      lv_used_define    TYPE abap_bool,
      lv_define         TYPE abap_bool,
      ls_statement      LIKE LINE OF io_scan->statements,
      lv_keyword        TYPE string.


    IF trdir-fixpt = abap_true.
      RETURN.
    ENDIF.
    lv_save_to_change = abap_true.

    lo_compiler = cl_abap_compiler=>create( program_name ).

    LOOP AT io_scan->statements INTO ls_statement.
      lv_keyword = io_scan->statement_keyword( sy-tabix ).
      IF lv_define = abap_true.
        IF lv_keyword = 'END-OF-DEFINITION'.
          lv_define = abap_false.
        ENDIF.
        CONTINUE.
      ENDIF.

      CASE lv_keyword.
        WHEN 'DEFINE'.
          lv_save_to_change = abap_false. "define is black magic
          lv_define = abap_true.
        WHEN 'COMPUTE'.
          check_compute(
            EXPORTING
              io_scan      = io_scan
              is_statement = ls_statement
              io_compiler  = lo_compiler
            CHANGING
              cv_save_to_change = lv_save_to_change ).
      ENDCASE.
    ENDLOOP.

    IF lv_save_to_change = abap_true.
      inform( p_test = myname
              p_kind = mv_errty
              p_code = '001' ).
    ELSEIF lv_used_define = abap_true.
      inform( p_test = myname
              p_kind = 'N'
              p_code = '003' ).
    ENDIF.

  ENDMETHOD.


  METHOD check_compute.
    DATA:
      lv_statement       TYPE string,
      lv_inform          TYPE abap_bool,
      lv_column          TYPE i,
      lv_full            TYPE string,
      lt_data            TYPE STANDARD TABLE OF REF TO cl_abap_comp_data,
      lo_data            TYPE REF TO cl_abap_comp_data,
      lo_data_assignment TYPE REF TO cl_abap_comp_data.

    FIELD-SYMBOLS:
      <ls_token> LIKE LINE OF io_scan->tokens.

    "concate statement for debugging purpose and inform
    lv_statement = get_statement(
      it_tokens     = io_scan->tokens
      is_statement  = is_statement ).

    "check for multiplication and division
    LOOP AT io_scan->tokens ASSIGNING <ls_token> FROM is_statement-from TO is_statement-to
        WHERE str = '*'
           OR str = '/'.
      lv_inform = abap_true.
      EXIT.
    ENDLOOP.

    IF lv_inform = abap_false.
      "collect all types of compute statement
      LOOP AT io_scan->tokens ASSIGNING <ls_token> FROM is_statement-from TO is_statement-to
          WHERE str <> '='.
        lv_column = <ls_token>-col + <ls_token>-len1 - 1. "end of token
        io_compiler->get_full_name_for_position(
          EXPORTING
            p_line    = <ls_token>-row
            p_column  = lv_column
            p_include = io_scan->get_include( is_statement-level )
          IMPORTING
            p_full_name = lv_full
          EXCEPTIONS
            OTHERS = 4 ).
        IF sy-subrc = 0.
          TRY.
              lo_data ?= io_compiler->get_symbol_entry( lv_full ).
              APPEND lo_data TO lt_data.
            CATCH cx_sy_move_cast_error.
              "ignore - only take simple types
          ENDTRY.
        ENDIF.
      ENDLOOP.

      "check type compatibility
      READ TABLE lt_data INTO lo_data_assignment INDEX 1.
      IF sy-subrc = 0.
        IF lo_data_assignment->type->length = -1.
          "it's a parameter
          lv_inform = abap_true.
        ELSEIF lo_data_assignment->type->atyp = 'P'.
          LOOP AT lt_data FROM 2 INTO lo_data.
            IF lo_data->type->atyp = 'P'.
              "packed number
              IF lo_data->type->decimals <> lo_data_assignment->type->decimals.
                lv_inform = abap_true.
                EXIT.
              ENDIF.
            ELSEIF lo_data->type->atyp = 'I' OR lo_data->type->atyp = 'C' OR lo_data->type->atyp = 'g'.
              "integer or char or string
              lv_inform = abap_true.
              EXIT.
            ENDIF.
          ENDLOOP.
          IF sy-subrc <> 0.
            "some strange assignment, e.g. string template
            lv_inform = abap_true.
          ENDIF.
        ENDIF.
      ENDIF.
    ENDIF.

    IF lv_inform = abap_true.
      cv_save_to_change = abap_false.
      "get token for line/col
      LOOP AT io_scan->tokens ASSIGNING <ls_token> FROM is_statement-from TO is_statement-to.
        EXIT.
      ENDLOOP.

      inform(
        p_sub_obj_name = io_scan->get_include( is_statement-level )
        p_line         = <ls_token>-row
        p_column       = <ls_token>-col
        p_kind         = 'N'
        p_test         = myname
        p_code         = '002'
        p_param_1      = lv_statement ).
    ENDIF.
  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version     = '001'.
    position    = '085'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Fixed point arithmetic can be safely activated'(m01) ).

    insert_scimessage(
        iv_code = '002'
        iv_text = 'Statement blocks activation of fixed point arithmetic: &1'(m02) ).

    insert_scimessage(
        iv_code = '003'
        iv_text = TEXT-m03 ).

  ENDMETHOD.


  METHOD get_statement.
    FIELD-SYMBOLS:
      <ls_token> LIKE LINE OF it_tokens.

    LOOP AT it_tokens ASSIGNING <ls_token> FROM is_statement-from TO is_statement-to.
      IF rv_statement IS INITIAL.
        rv_statement = <ls_token>-str.
      ELSE.
        CONCATENATE rv_statement <ls_token>-str INTO rv_statement SEPARATED BY space.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.
ENDCLASS.
