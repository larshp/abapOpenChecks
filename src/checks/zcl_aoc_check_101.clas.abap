CLASS zcl_aoc_check_101 DEFINITION
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



CLASS ZCL_AOC_CHECK_101 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    FIELD-SYMBOLS <ls_structure> LIKE LINE OF io_scan->structures.
    FIELD-SYMBOLS <ls_statement> LIKE LINE OF io_scan->statements.
    FIELD-SYMBOLS <ls_token>     LIKE LINE OF io_scan->tokens.

    DATA l_condition_start_token_index TYPE i.

    LOOP AT io_scan->structures ASSIGNING <ls_structure>
        WHERE stmnt_type = zcl_aoc_scan=>gc_structure_statement-if
        OR stmnt_type = zcl_aoc_scan=>gc_structure_statement-elseif.

      LOOP AT io_scan->statements ASSIGNING <ls_statement>
          FROM <ls_structure>-stmnt_from TO <ls_structure>-stmnt_to
          WHERE type <> io_scan->gc_statement-empty
          AND type <> io_scan->gc_statement-comment
          AND type <> io_scan->gc_statement-comment_in_stmnt
          AND type <> io_scan->gc_statement-macro_definition
          AND type <> io_scan->gc_statement-pragma.


        LOOP AT io_scan->tokens ASSIGNING <ls_token>
            FROM <ls_statement>-from TO <ls_statement>-to.

          IF <ls_token>-str = 'IF' OR <ls_token>-str = 'ELSEIF'
              OR <ls_token>-str = 'AND' OR <ls_token>-str = 'OR' OR <ls_token>-str = '('.
            l_condition_start_token_index = sy-tabix.
            CONTINUE.
          ELSEIF l_condition_start_token_index IS INITIAL.
            CONTINUE.
          ENDIF.

          IF sy-tabix = l_condition_start_token_index + 1.
            IF <ls_token>-str <> 'NOT'.
              CLEAR l_condition_start_token_index.
            ENDIF.
            CONTINUE.
          ENDIF.

          inform( p_sub_obj_name = io_scan->get_include( <ls_statement>-level )
                  p_line         = <ls_token>-row
                  p_kind         = mv_errty
                  p_test         = myname
                  p_code         = '001' ).

        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version  = '001'.
    position = '101'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Prefer IS NOT to NOT IS'(m01) ).

  ENDMETHOD.
ENDCLASS.
