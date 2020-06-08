CLASS zcl_aoc_check_03 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS check
        REDEFINITION.
  PROTECTED SECTION.

    METHODS check_nested
      IMPORTING
        !io_scan TYPE REF TO zcl_aoc_scan .
    METHODS check_no_catch
      IMPORTING
        !io_scan TYPE REF TO zcl_aoc_scan .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aoc_check_03 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    check_no_catch( io_scan ).

    check_nested( io_scan ).

  ENDMETHOD.


  METHOD check_nested.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_position  TYPE i,
          lv_index     TYPE i,
          lv_include   TYPE program,
          lv_error     TYPE abap_bool,
          lv_exception TYPE string.

    FIELD-SYMBOLS: <ls_token>     LIKE LINE OF io_scan->tokens,
                   <ls_statement> LIKE LINE OF io_scan->statements.


    LOOP AT io_scan->statements ASSIGNING <ls_statement>
        WHERE type <> io_scan->gc_statement-comment
        AND type <> io_scan->gc_statement-empty
        AND type <> io_scan->gc_statement-comment_in_stmnt
        AND type <> io_scan->gc_statement-pragma.

      lv_position = sy-tabix.

      READ TABLE io_scan->tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
      IF sy-subrc <> 0.
        CLEAR lv_exception.
        CONTINUE.
      ENDIF.

      IF <ls_token>-str = 'CATCH'.
        lv_index = <ls_statement>-from + 1.

        READ TABLE io_scan->tokens ASSIGNING <ls_token> INDEX lv_index.
        IF sy-subrc <> 0.
          CLEAR lv_exception.
          CONTINUE.
        ENDIF.

        IF lv_exception = <ls_token>-str.
          lv_error = abap_true.
        ELSE.
          lv_error = abap_false.
        ENDIF.
        lv_exception = <ls_token>-str.
      ELSEIF <ls_token>-str = 'ENDTRY'.
        IF lv_error = abap_true AND NOT lv_exception IS INITIAL.
          lv_include = io_scan->get_include( <ls_statement>-level ).
          inform( p_sub_obj_name = lv_include
                  p_position     = lv_position
                  p_line         = <ls_token>-row
                  p_kind         = mv_errty
                  p_test         = myname
                  p_code         = '002' ).
        ENDIF.
        lv_error = abap_false.
        CONTINUE.
      ELSE.
        CLEAR lv_exception.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "check_nested


  METHOD check_no_catch.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_include TYPE program,
          lv_found   TYPE abap_bool,
          lv_index   LIKE sy-tabix.

    FIELD-SYMBOLS: <ls_structure> LIKE LINE OF io_scan->structures,
                   <ls_statement> LIKE LINE OF io_scan->statements,
                   <ls_token>     LIKE LINE OF io_scan->tokens.


    LOOP AT io_scan->structures ASSIGNING <ls_structure>
        WHERE stmnt_type = zcl_aoc_scan=>gc_structure_statement-try.
      lv_index = sy-tabix.

      lv_found = abap_false.

      READ TABLE io_scan->structures
        WITH KEY stmnt_type = zcl_aoc_scan=>gc_structure_statement-catch back = lv_index
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        lv_found = abap_true.
      ENDIF.

      READ TABLE io_scan->structures
        WITH KEY stmnt_type = zcl_aoc_scan=>gc_structure_statement-cleanup back = lv_index
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        lv_found = abap_true.
      ENDIF.

      IF lv_found = abap_false.

        READ TABLE io_scan->statements ASSIGNING <ls_statement> INDEX <ls_structure>-stmnt_from.
        ASSERT sy-subrc = 0.

        READ TABLE io_scan->tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
        ASSERT sy-subrc = 0.

        lv_include = io_scan->get_include( <ls_statement>-level ).

        inform( p_sub_obj_name = lv_include
                p_position     = <ls_structure>-stmnt_from
                p_line         = <ls_token>-row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = '001' ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "check_no_catch


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '003'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).
    enable_checksum( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'TRY without CATCH'(m01) ).
    insert_scimessage(
        iv_code = '002'
        iv_text = 'Nesting with same exception'(m02) ).

  ENDMETHOD.
ENDCLASS.
