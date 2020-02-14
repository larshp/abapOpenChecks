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

      IF lv_fourth <> 'DESTINATION'
          OR lv_statement CP '* EXCEPTIONS * MESSAGE *'
          OR lv_statement CP '* DESTINATION ''NONE'' *'.
        CONTINUE. " to next statement
      ENDIF.

      lv_include = io_scan->get_include( <ls_statement>-level ).
      inform( p_sub_obj_name = lv_include
              p_line         = <ls_token>-row
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '001' ).

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
