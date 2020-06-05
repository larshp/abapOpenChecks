CLASS zcl_aoc_check_21 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.

    METHODS check
        REDEFINITION.
  PROTECTED SECTION.

    METHODS build_statement
      IMPORTING
        !is_statement       TYPE sstmnt
        !it_tokens          TYPE stokesx_tab
      RETURNING
        VALUE(rv_statement) TYPE string.
    METHODS find_parameters
      IMPORTING
        !iv_statement        TYPE string
      RETURNING
        VALUE(rt_parameters) TYPE string_table.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aoc_check_21 IMPLEMENTATION.


  METHOD build_statement.

    FIELD-SYMBOLS: <ls_token> LIKE LINE OF it_tokens.


    LOOP AT it_tokens ASSIGNING <ls_token> FROM is_statement-from TO is_statement-to.
      IF rv_statement IS INITIAL.
        rv_statement = <ls_token>-str.
      ELSE.
        CONCATENATE rv_statement <ls_token>-str
          INTO rv_statement SEPARATED BY space.
      ENDIF.
    ENDLOOP.
    CONCATENATE rv_statement '.' INTO rv_statement.

  ENDMETHOD.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_parameters TYPE string_table,
          lv_parameter  LIKE LINE OF lt_parameters,
          lv_statement  TYPE string,
          ls_form_stmnt LIKE LINE OF io_scan->statements,
          lv_form       TYPE abap_bool.

    FIELD-SYMBOLS: <ls_structure> LIKE LINE OF io_scan->structures,
                   <ls_token>     LIKE LINE OF io_scan->tokens,
                   <ls_statement> LIKE LINE OF io_scan->statements.


    LOOP AT io_scan->structures ASSIGNING <ls_structure>
        WHERE type = io_scan->gc_structure-routine
        AND stmnt_type = zcl_aoc_scan=>gc_structure_statement-form.

      lv_form = abap_true.

      LOOP AT io_scan->statements ASSIGNING <ls_statement>
          FROM <ls_structure>-stmnt_from
          TO <ls_structure>-stmnt_to
          WHERE type <> io_scan->gc_statement-empty
          AND type <> io_scan->gc_statement-comment
          AND type <> io_scan->gc_statement-comment_in_stmnt.

        lv_statement = build_statement(
            is_statement = <ls_statement>
            it_tokens    = io_scan->tokens ).

        IF lv_form = abap_true.
          ls_form_stmnt = <ls_statement>.
          lt_parameters = find_parameters( lv_statement ).
          lv_form = abap_false.
          CONTINUE.
        ENDIF.

        LOOP AT lt_parameters INTO lv_parameter.
          IF lv_statement CS lv_parameter.
            DELETE lt_parameters INDEX sy-tabix.
          ENDIF.
        ENDLOOP.

      ENDLOOP.

      LOOP AT lt_parameters INTO lv_parameter.
        READ TABLE io_scan->tokens INDEX ls_form_stmnt-from ASSIGNING <ls_token>.
        ASSERT sy-subrc = 0.

        inform( p_sub_obj_name = io_scan->get_include( ls_form_stmnt-level )
                p_position     = <ls_structure>-stmnt_from
                p_line         = <ls_token>-row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = '001'
                p_param_1      = lv_parameter ).
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '021'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).
    enable_checksum( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Unused FORM parameter &1'(m01) ).

  ENDMETHOD.


  METHOD find_parameters.

    DATA: lt_code   TYPE TABLE OF string,
          ls_result TYPE zcl_aoc_parser=>ty_result.

    FIELD-SYMBOLS: <ls_res_tok> LIKE LINE OF ls_result-tokens.


    APPEND iv_statement TO lt_code.
    ls_result = zcl_aoc_parser=>run(
        it_code = lt_code
        iv_rule = 'FORM' ).

    LOOP AT ls_result-tokens ASSIGNING <ls_res_tok>
        WHERE value = zcl_aoc_parser=>c_role-fielddefid
        AND type = zcl_aoc_parser=>c_type-role.
      APPEND <ls_res_tok>-code TO rt_parameters.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
