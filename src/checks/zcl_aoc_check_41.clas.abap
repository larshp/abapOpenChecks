CLASS zcl_aoc_check_41 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
    METHODS get_attributes
        REDEFINITION .
    METHODS if_ci_test~query_attributes
        REDEFINITION .
    METHODS put_attributes
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA mv_ignore TYPE flag.
ENDCLASS.



CLASS ZCL_AOC_CHECK_41 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_include TYPE sobj_name,
          lv_comment TYPE abap_bool,
          lv_len     TYPE i,
          lv_prev    TYPE i.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF io_scan->statements,
                   <ls_token>     LIKE LINE OF io_scan->tokens,
                   <ls_scomment>  LIKE LINE OF io_scan->statements,
                   <ls_tcomment>  LIKE LINE OF io_scan->tokens.


    LOOP AT io_scan->statements ASSIGNING <ls_statement>
        WHERE type <> io_scan->gc_statement-empty
        AND type <> io_scan->gc_statement-comment
        AND type <> io_scan->gc_statement-comment_in_stmnt
        AND type <> io_scan->gc_statement-macro_definition
        AND type <> io_scan->gc_statement-pragma
        AND colonrow = 0.

      CLEAR lv_prev.

      LOOP AT io_scan->tokens ASSIGNING <ls_token> FROM <ls_statement>-from TO <ls_statement>-to.
        lv_len = strlen( <ls_token>-str ) - 1.
        IF <ls_token>-str(1) = '(' AND <ls_token>-str+lv_len(1) = ')'.
* special case see unit test 05, SAP is fun
          EXIT.
        ENDIF.

        IF lv_prev IS INITIAL
            OR lv_prev = <ls_token>-row
            OR lv_prev = <ls_token>-row - 1.
          lv_prev = <ls_token>-row.
          CONTINUE.
        ENDIF.

        IF <ls_token>-str = 'GLOBAL'.
* global friends in classes are auto generated with newlines
          EXIT.
        ENDIF.

        lv_comment = abap_false.
        LOOP AT io_scan->statements ASSIGNING <ls_scomment>
            WHERE type = io_scan->gc_statement-comment_in_stmnt
            AND level = <ls_statement>-level.
          LOOP AT io_scan->tokens ASSIGNING <ls_tcomment>
              FROM <ls_statement>-from TO <ls_statement>-to.
            IF <ls_tcomment>-row = lv_prev.
              lv_comment = abap_true.
            ENDIF.
          ENDLOOP.
        ENDLOOP.

        IF mv_ignore = abap_false OR lv_comment = abap_false.
          lv_include = io_scan->get_include( <ls_statement>-level ).
          inform( p_sub_obj_name = lv_include
                  p_line         = <ls_token>-row
                  p_kind         = mv_errty
                  p_test         = myname
                  p_code         = '001' ).
          EXIT.
        ENDIF.

        lv_prev = <ls_token>-row.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '041'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.
    mv_ignore      = abap_false.

    enable_rfc( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Empty line in statement'(m01) ).

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mv_ignore = mv_ignore
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_ignore 'Ignore comments' ''.          "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mv_ignore = mv_ignore
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
