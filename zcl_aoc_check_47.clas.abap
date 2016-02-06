class ZCL_AOC_CHECK_47 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.

*"* public components of class ZCL_AOC_CHECK_47
*"* do not include other source files here!!!
  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
protected section.
*"* protected components of class ZCL_AOC_CHECK_47
*"* do not include other source files here!!!
private section.
ENDCLASS.



CLASS ZCL_AOC_CHECK_47 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lv_include   TYPE sobj_name,
        lv_statement TYPE string.

  FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                 <ls_token>     LIKE LINE OF it_tokens.


  LOOP AT it_statements ASSIGNING <ls_statement>
      WHERE type <> scan_stmnt_type-empty
      AND type <> scan_stmnt_type-comment
      AND type <> scan_stmnt_type-comment_in_stmnt
      AND type <> scan_stmnt_type-macro_definition
      AND type <> scan_stmnt_type-pragma.

    CLEAR lv_statement.

    LOOP AT it_tokens ASSIGNING <ls_token>
        FROM <ls_statement>-from TO <ls_statement>-to
        WHERE type = scan_token_type-identifier
        OR type = scan_token_type-literal.
      IF lv_statement IS INITIAL.
        lv_statement = <ls_token>-str.
      ELSE.
        CONCATENATE lv_statement <ls_token>-str
          INTO lv_statement SEPARATED BY space.
      ENDIF.
    ENDLOOP.

* this is wrong, but will work for most cases,
* DESTINATION might be a parameter name or input
    IF ( NOT lv_statement CP 'CALL FUNCTION * DESTINATION *' )
        OR lv_statement CP '* EXCEPTIONS * MESSAGE *'
        OR lv_statement CP '* DESTINATION ''NONE'' *'.
      CONTINUE. " to next statement
    ENDIF.

    lv_include = get_include( p_level = <ls_statement>-level ).
    inform( p_sub_obj_type = c_type_include
            p_sub_obj_name = lv_include
            p_line         = <ls_token>-row
            p_kind         = mv_errty
            p_test         = myname
            p_code         = '001' ).

  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'RFC call error handling'.               "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '001'.
  position       = '047'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_message_text.

  CLEAR p_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Add RFC call error handling'.                   "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 0.
  ENDCASE.

ENDMETHOD.
ENDCLASS.