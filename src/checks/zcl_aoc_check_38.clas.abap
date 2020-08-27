CLASS zcl_aoc_check_38 DEFINITION
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



CLASS ZCL_AOC_CHECK_38 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_include         TYPE sobj_name,
          lv_statement_count TYPE i,
          lv_up_to_n_rows    TYPE abap_bool.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF io_scan->statements,
                   <ls_token>     LIKE LINE OF io_scan->tokens.


    LOOP AT io_scan->statements ASSIGNING <ls_statement>.

      IF <ls_statement>-type = io_scan->gc_statement-standard
          OR <ls_statement>-type = io_scan->gc_statement-method_direct.

        LOOP AT io_scan->tokens ASSIGNING <ls_token>
            FROM <ls_statement>-from TO <ls_statement>-to
            WHERE str = 'UP' OR str = 'TO' OR str = 'ROWS'.
          lv_statement_count = lv_statement_count + 1.
        ENDLOOP.

        IF lv_statement_count = 3.
          lv_up_to_n_rows = abap_true.
        ENDIF.
        CLEAR lv_statement_count.

      ENDIF.

      READ TABLE io_scan->tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF <ls_token>-str = 'ENDSELECT'.

        IF lv_up_to_n_rows = abap_false.

          lv_include = io_scan->get_include( <ls_statement>-level ).

          inform( p_sub_obj_name = lv_include
                  p_line         = <ls_token>-row
                  p_kind         = mv_errty
                  p_test         = myname
                  p_code         = '001' ).
        ENDIF.

        CLEAR lv_up_to_n_rows.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '038'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Avoid use of SELECT-ENDSELECT'(m01) ).

  ENDMETHOD.
ENDCLASS.
