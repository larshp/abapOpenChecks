class ZCL_AOC_CHECK_40 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.

*"* public components of class ZCL_AOC_CHECK_40
*"* do not include other source files here!!!
  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_AOC_CHECK_40 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lv_include   TYPE sobj_name,
        lv_check     TYPE abap_bool,
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
        WHERE type = scan_token_type-identifier.
      IF lv_statement IS INITIAL.
        lv_statement = <ls_token>-str.
      ELSE.
        CONCATENATE lv_statement <ls_token>-str
          INTO lv_statement SEPARATED BY space.
      ENDIF.
    ENDLOOP.

    IF lv_statement CP 'READ TABLE *'
        OR lv_statement CP 'IMPORT *'
        OR lv_statement CP 'ASSIGN COMPONENT *'.
      lv_report = lv_position.
      lv_check = abap_true.
      lv_row = <ls_token>-row.
      CONTINUE. " to next statement
    ENDIF.

    IF lv_statement = 'ENDIF'.
      CONTINUE.
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

    lv_check = abap_false.

  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  DATA: ls_message LIKE LINE OF scimessages.


  super->constructor( ).

  description    = 'Check SY-SUBRC'.                        "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '001'.
  position       = '040'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

  ls_message-test = myname.
  ls_message-code = '001'.
  ls_message-kind = c_error.
  ls_message-pcom = '"#EC CI_SUBRC'.
  INSERT ls_message INTO TABLE scimessages.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Check SY-SUBRC'.                            "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.
ENDCLASS.