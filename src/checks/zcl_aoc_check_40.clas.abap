CLASS zcl_aoc_check_40 DEFINITION
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



CLASS ZCL_AOC_CHECK_40 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    TYPES:
      BEGIN OF t_stack,
        stackposition TYPE i,
        position      TYPE i,
        row           TYPE token_row,
      END OF t_stack.

    DATA: lv_include   TYPE sobj_name,
          lv_check     TYPE abap_bool,
          lv_stack     TYPE i,
          lt_stack     TYPE STANDARD TABLE OF t_stack WITH NON-UNIQUE KEY stackposition,
          ls_stack     LIKE LINE OF lt_stack,
          lv_report    TYPE i,
          lv_position  TYPE i,
          lv_row       TYPE token_row,
          lv_statement TYPE string.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF io_scan->statements,
                   <ls_token>     LIKE LINE OF io_scan->tokens.


    LOOP AT io_scan->statements ASSIGNING <ls_statement>
        WHERE type <> io_scan->gc_statement-empty
        AND type <> io_scan->gc_statement-comment
        AND type <> io_scan->gc_statement-comment_in_stmnt
        AND type <> io_scan->gc_statement-macro_definition
        AND type <> io_scan->gc_statement-pragma.
      lv_position = sy-tabix.

      CLEAR lv_statement.

      LOOP AT io_scan->tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from TO <ls_statement>-to
          WHERE type = io_scan->gc_token-identifier
          OR type = io_scan->gc_token-list.
        IF lv_statement IS INITIAL.
          lv_statement = <ls_token>-str.
        ELSE.
          CONCATENATE lv_statement <ls_token>-str
            INTO lv_statement SEPARATED BY space.
        ENDIF.
      ENDLOOP.

      "re-check return code of last statement within IF/CASE-clause
      LOOP AT lt_stack INTO ls_stack WHERE stackposition = lv_stack.
        IF NOT lv_statement CP '* SY-SUBRC*'
            AND NOT lv_statement CP '*CL_ABAP_UNIT_ASSERT=>ASSERT_SUBRC*'.
          lv_include = io_scan->get_include( <ls_statement>-level ).
          inform( p_sub_obj_name = lv_include
                  p_line         = ls_stack-row
                  p_kind         = mv_errty
                  p_position     = ls_stack-position
                  p_test         = myname
                  p_code         = '001' ).
        ENDIF.
      ENDLOOP.
      DELETE lt_stack WHERE stackposition = lv_stack.

      IF lv_statement CP 'IF *' OR lv_statement CP 'CASE *'.
        lv_stack = lv_stack + 1.
      ENDIF.

      IF lv_statement CP 'READ TABLE *'
          OR lv_statement CP 'IMPORT *'
          OR lv_statement CP 'ASSIGN COMPONENT *'
          OR lv_statement CP 'ASSIGN (*'.
        IF lv_check = abap_true.
          lv_include = io_scan->get_include( <ls_statement>-level ).
          inform( p_sub_obj_name = lv_include
                  p_line         = lv_row
                  p_kind         = mv_errty
                  p_position     = lv_report
                  p_test         = myname
                  p_code         = '001' ).
        ENDIF.
        lv_report = lv_position.
        lv_check = abap_true.
        lv_row = <ls_token>-row.
        CONTINUE. " to next statement
      ENDIF.

      IF lv_statement = 'ENDIF' OR lv_statement = 'ENDCASE'.
        lv_stack = lv_stack - 1.
        CONTINUE.
      ENDIF.

      IF lv_check = abap_true AND ( lv_statement CP 'ELSE*' OR lv_statement CP 'WHEN *' ).
        "collect for re-check after ENDIF/ENDCASE
        ls_stack-position       = lv_report.
        ls_stack-row            = lv_row.
        ls_stack-stackposition  = lv_stack - 1.
        APPEND ls_stack TO lt_stack.
        lv_check = abap_false.
      ENDIF.

      IF lv_check = abap_true
          AND NOT lv_statement CP '* SY-SUBRC*'
          AND NOT lv_statement CP '*CL_ABAP_UNIT_ASSERT=>ASSERT_SUBRC*'.
        lv_include = io_scan->get_include( <ls_statement>-level ).
        inform( p_sub_obj_name = lv_include
                p_line         = lv_row
                p_kind         = mv_errty
                p_position     = lv_report
                p_test         = myname
                p_code         = '001' ).
      ENDIF.

      lv_check = abap_false.

    ENDLOOP.

    "all remaining elements in the stack are positive
    LOOP AT lt_stack INTO ls_stack.
      lv_include = io_scan->get_include( <ls_statement>-level ).
      inform( p_sub_obj_name = lv_include
              p_line         = ls_stack-row
              p_kind         = mv_errty
              p_position     = ls_stack-position
              p_test         = myname
              p_code         = '001' ).
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '040'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Check SY-SUBRC'(m01)
        iv_pcom = '"#EC CI_SUBRC' ).

  ENDMETHOD.
ENDCLASS.
