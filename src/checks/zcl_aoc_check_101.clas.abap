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
    CONSTANTS mc_function_open_regex    TYPE string VALUE `^\w*\($`.
    CONSTANTS mc_function_close_string  TYPE string VALUE `)`.
    CONSTANTS mc_table_expr_open_regex  TYPE string VALUE `^\w*\[$`.
    CONSTANTS mc_table_expr_close_regex TYPE string VALUE `^]-?\w*$`.

    DATA mt_relation_operator_range TYPE RANGE OF string.

ENDCLASS.



CLASS zcl_aoc_check_101 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    FIELD-SYMBOLS <ls_structure> LIKE LINE OF io_scan->structures.
    FIELD-SYMBOLS <ls_statement> LIKE LINE OF io_scan->statements.
    FIELD-SYMBOLS <ls_token>     LIKE LINE OF io_scan->tokens.

    DATA lv_condition_start_token_index TYPE i.
    DATA lv_has_relational_operator     TYPE abap_bool.
    DATA lv_is_in_function              TYPE abap_bool.
    DATA lv_is_in_table_expression      TYPE abap_bool.

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

        CLEAR: lv_condition_start_token_index, lv_has_relational_operator,
               lv_is_in_table_expression, lv_is_in_function.

        LOOP AT io_scan->tokens ASSIGNING <ls_token>
            FROM <ls_statement>-from TO <ls_statement>-to.

          IF <ls_token>-str = 'IF' OR <ls_token>-str = 'ELSEIF'
              OR <ls_token>-str = 'AND' OR <ls_token>-str = 'OR' OR <ls_token>-str = '('.
            lv_condition_start_token_index = sy-tabix.
            CONTINUE.
          ELSEIF lv_condition_start_token_index IS INITIAL.
            CONTINUE.
          ENDIF.

          IF sy-tabix = lv_condition_start_token_index + 1.
            IF <ls_token>-str <> 'NOT'.
              CLEAR lv_condition_start_token_index.
            ENDIF.
            CONTINUE.
          ENDIF.

          IF lv_is_in_function = abap_true.
            IF <ls_token>-str = mc_function_close_string.
              CLEAR lv_is_in_function.
            ENDIF.
            CONTINUE.
          ELSEIF boolc( matches( val = <ls_token>-str regex = mc_function_open_regex ) ) = abap_true.
            lv_is_in_function = abap_true.
            CONTINUE.
          ENDIF.

          IF lv_is_in_table_expression = abap_true.
            IF boolc( matches( val = <ls_token>-str regex = mc_table_expr_close_regex ) ) = abap_true.
              CLEAR lv_is_in_table_expression.
            ENDIF.
            CONTINUE.
          ELSEIF boolc( matches( val = <ls_token>-str regex = mc_table_expr_open_regex ) ) = abap_true.
            lv_is_in_table_expression = abap_true.
            CONTINUE.
          ENDIF.

          IF <ls_token>-str IN mt_relation_operator_range.
            lv_has_relational_operator = abap_true.
          ENDIF.
        ENDLOOP.

        IF lv_has_relational_operator = abap_true.
          inform( p_sub_obj_name = io_scan->get_include( <ls_statement>-level )
                  p_line         = <ls_token>-row
                  p_kind         = mv_errty
                  p_test         = myname
                  p_code         = '001' ).
        ENDIF.
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

    "https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abenlogexp_comp.htm
    APPEND VALUE #( sign = 'I' option = 'EQ' low = '=' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'EQ' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = '<>' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'NE' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = '>' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'GT' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = '<' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'LT' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = '>=' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'GE' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = '<=' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'LE' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'CO' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'CN' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'CA' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'NA' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'CS' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'NS' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'CP' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'NP' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'BYTE-CO' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'BYTE-CN' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'BYTE-CA' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'BYTE-NA' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'BYTE-CS' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'BYTE-NS' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'O' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'Z' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'M' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'BETWEEN' ) TO mt_relation_operator_range.
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'IN' ) TO mt_relation_operator_range.

    "IS for all the Predicate expressions :
    "https://help.sap.com/doc/abapdocu_750_index_htm/7.50/en-US/abenpredicate_expressions.htm
    APPEND VALUE #( sign = 'I' option = 'EQ' low = 'IS' ) TO mt_relation_operator_range.

  ENDMETHOD.
ENDCLASS.
