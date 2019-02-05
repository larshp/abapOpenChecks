CLASS zcl_aoc_check_40 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS check
        REDEFINITION.
    METHODS get_message_text
        REDEFINITION.
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

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                   <ls_token>     LIKE LINE OF it_tokens.


    LOOP AT it_statements ASSIGNING <ls_statement>
        WHERE type <> scan_stmnt_type-empty
        AND type <> scan_stmnt_type-comment
        AND type <> scan_stmnt_type-comment_in_stmnt
        AND type <> scan_stmnt_type-macro_definition
        AND type <> scan_stmnt_type-pragma.
      lv_position = sy-tabix.

      CLEAR lv_statement.

      LOOP AT it_tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from TO <ls_statement>-to
          WHERE type = scan_token_type-identifier
          OR type = scan_token_type-list.
        IF lv_statement IS INITIAL.
          lv_statement = <ls_token>-str.
        ELSE.
          CONCATENATE lv_statement <ls_token>-str
            INTO lv_statement SEPARATED BY space.
        ENDIF.
      ENDLOOP.

      IF lv_statement CP 'IF *' OR lv_statement CP 'CASE *'.
        lv_stack = lv_stack + 1.
      ENDIF.

      IF lv_check = abap_false
          AND ( lv_statement CP 'READ TABLE *'
          OR lv_statement CP 'IMPORT *'
          OR lv_statement CP 'ASSIGN COMPONENT *'
          OR lv_statement CP 'ASSIGN (*' ).
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
          AND NOT lv_statement CP '* SY-SUBRC *'
          AND NOT lv_statement CP '*CL_ABAP_UNIT_ASSERT=>ASSERT_SUBRC*'.
        lv_include = get_include( p_level = <ls_statement>-level ).
        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = lv_include
                p_line         = lv_row
                p_kind         = mv_errty
                p_position     = lv_report
                p_test         = myname
                p_code         = '001' ).
      ENDIF.

      "re-check return code of last statement within IF/CASE-clause
      LOOP AT lt_stack INTO ls_stack WHERE stackposition = lv_stack.
        IF NOT lv_statement CP '* SY-SUBRC *'
            AND NOT lv_statement CP '*CL_ABAP_UNIT_ASSERT=>ASSERT_SUBRC*'.
          lv_include = get_include( p_level = <ls_statement>-level ).
          inform( p_sub_obj_type = c_type_include
                  p_sub_obj_name = lv_include
                  p_line         = ls_stack-row
                  p_kind         = mv_errty
                  p_position     = ls_stack-position
                  p_test         = myname
                  p_code         = '001' ).
        ENDIF.
      ENDLOOP.
      DELETE lt_stack WHERE stackposition = lv_stack.

      lv_check = abap_false.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    DATA: ls_message LIKE LINE OF scimessages.


    super->constructor( ).

    version        = '001'.
    position       = '040'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    mv_errty = c_error.

    ls_message-test = myname.
    ls_message-code = '001'.
    ls_message-kind = c_error.
    ls_message-pcom = '"#EC CI_SUBRC'.
    INSERT ls_message INTO TABLE scimessages.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Check SY-SUBRC'.                          "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
