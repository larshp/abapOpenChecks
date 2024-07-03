CLASS zcl_aoc_check_47 DEFINITION
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



CLASS ZCL_AOC_CHECK_47 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_include   TYPE sobj_name,
          lv_count     TYPE i,
          lv_fourth    TYPE string,
          lv_statement TYPE string.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF io_scan->statements,
                   <ls_token>     LIKE LINE OF io_scan->tokens.


    LOOP AT io_scan->statements ASSIGNING <ls_statement>
        WHERE type <> io_scan->gc_statement-empty
        AND type <> io_scan->gc_statement-comment
        AND type <> io_scan->gc_statement-comment_in_stmnt
        AND type <> io_scan->gc_statement-macro_definition
        AND type <> io_scan->gc_statement-pragma.

      CLEAR lv_statement.
      CLEAR lv_fourth.
      lv_count = 0.

      LOOP AT io_scan->tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from TO <ls_statement>-to
          WHERE type = io_scan->gc_token-identifier
          OR type = io_scan->gc_token-literal.
        lv_count = lv_count + 1.

        IF lv_count = 4.
          lv_fourth = <ls_token>-str.
        ENDIF.

        IF lv_statement IS INITIAL.
          lv_statement = <ls_token>-str.
        ELSE.
          CONCATENATE lv_statement <ls_token>-str
            INTO lv_statement SEPARATED BY space.
        ENDIF.
      ENDLOOP.

      " The keyword DESTINATION needs to be checked only at fourth position
      " as a parameter could also be named like the keyword (e.g. module GET_PRINT_PARAMETERS).
      " This could have also been checked by a regex (when followed by or lead by '=' it is a parameter)
      " but this would require a pretty high SAP_BASIS release.
      IF lv_statement CP 'CALL FUNCTION *'
          AND lv_fourth    = 'DESTINATION'
          AND lv_statement NP '* EXCEPTIONS * MESSAGE *'
          AND lv_statement NP '* DESTINATION ''NONE'' *'.
        lv_include = io_scan->get_include( <ls_statement>-level ).
        inform( p_sub_obj_name = lv_include
                p_line         = <ls_token>-row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = '001' ).
      ELSE.
        " Either no CALL FUNCTION with RFC
        " or it correctly specifies the EXCEPTIONS with MESSAGE addition
        CONTINUE. " to next statement
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '047'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Add RFC call error handling'(m01) ).

  ENDMETHOD.
ENDCLASS.
