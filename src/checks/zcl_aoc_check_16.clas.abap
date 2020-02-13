CLASS zcl_aoc_check_16 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_16 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_include TYPE sobj_name,
          lv_ok      TYPE token_row.

    FIELD-SYMBOLS: <ls_token>     LIKE LINE OF io_scan->tokens,
                   <ls_statement> LIKE LINE OF io_scan->statements.


    LOOP AT io_scan->statements ASSIGNING <ls_statement>
        WHERE type <> io_scan->gc_statement-empty
        AND type <> io_scan->gc_statement-macro_definition
        AND type <> io_scan->gc_statement-comment
        AND type <> io_scan->gc_statement-native_sql
        AND type <> io_scan->gc_statement-pragma
        AND type <> io_scan->gc_statement-comment_in_stmnt.

      LOOP AT io_scan->tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from TO <ls_statement>-to
          WHERE type <> io_scan->gc_token-comment
          AND str <> ')'.
        lv_ok = <ls_token>-row.
      ENDLOOP.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF lv_ok <> <ls_statement>-trow.
        READ TABLE io_scan->tokens WITH KEY row = <ls_statement>-trow
          type = io_scan->gc_token-pragma TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
* allow if line contains pragma
          CONTINUE.
        ENDIF.

        lv_include = io_scan->get_include( <ls_statement>-level ).
        inform( p_sub_obj_name = lv_include
                p_line         = <ls_statement>-trow
                p_kind         = mv_errty
                p_test         = myname
                p_code         = '001' ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '016'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Line contains only "." or ")."'(m01) ).

  ENDMETHOD.
ENDCLASS.
