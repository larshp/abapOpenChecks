CLASS zcl_aoc_check_81 DEFINITION
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



CLASS ZCL_AOC_CHECK_81 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_level TYPE i,
          lv_first TYPE abap_bool,
          lv_code  TYPE sci_errc,
          ls_next  LIKE LINE OF io_scan->tokens.

    FIELD-SYMBOLS: <ls_level>     LIKE LINE OF io_scan->levels,
                   <ls_token>     LIKE LINE OF io_scan->tokens,
                   <ls_statement> LIKE LINE OF io_scan->statements.


    LOOP AT io_scan->levels ASSIGNING <ls_level>.
      lv_level = sy-tabix.

      LOOP AT io_scan->statements ASSIGNING <ls_statement> WHERE level = lv_level
          AND type <> io_scan->gc_statement-empty
          AND type <> io_scan->gc_statement-macro_definition
          AND type <> io_scan->gc_statement-comment
          AND type <> io_scan->gc_statement-native_sql
          AND type <> io_scan->gc_statement-pragma
          AND type <> io_scan->gc_statement-comment_in_stmnt.

        lv_first = abap_true.
        LOOP AT io_scan->tokens ASSIGNING <ls_token> FROM <ls_statement>-from TO <ls_statement>-to - 1.
          IF lv_first = abap_true
              AND ( <ls_token>-str = 'SELECT'
              OR <ls_token>-str = 'UPDATE'
              OR <ls_token>-str = 'INSERT' ).
            EXIT.
          ENDIF.
          lv_first = abap_false.

          READ TABLE io_scan->tokens INTO ls_next INDEX sy-tabix + 1.
          ASSERT sy-subrc = 0.

          CLEAR lv_code.
          IF <ls_token>-str(1) = '''' AND <ls_token>-col + <ls_token>-len1 = ls_next-col.
            lv_code = '001'.
          ELSEIF ls_next-str(1) = '''' AND <ls_token>-col + <ls_token>-len1 = ls_next-col.
            lv_code = '002'.
          ENDIF.

          IF NOT lv_code IS INITIAL.
            inform( p_sub_obj_name = <ls_level>-name
                    p_line         = <ls_token>-row
                    p_column       = <ls_token>-col + <ls_token>-len1
                    p_kind         = mv_errty
                    p_test         = myname
                    p_code         = lv_code ).
          ENDIF.

        ENDLOOP.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version  = '001'.
    position = '081'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Missing space after '''(m01) ).

    insert_scimessage(
        iv_code = '002'
        iv_text = 'Missing space before '''(m02) ).

  ENDMETHOD.
ENDCLASS.
