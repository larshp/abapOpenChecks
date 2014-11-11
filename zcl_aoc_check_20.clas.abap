class ZCL_AOC_CHECK_20 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.
*"* public components of class ZCL_AOC_CHECK_20
*"* do not include other source files here!!!

  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
protected section.
*"* protected components of class ZCL_AOC_CHECK_20
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_AOC_CHECK_20
*"* do not include other source files here!!!

  constants C_MY_NAME type SEOCLSNAME value 'ZCL_AOC_CHECK_20'. "#EC NOTEXT
ENDCLASS.



CLASS ZCL_AOC_CHECK_20 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lv_begin TYPE i.

  FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                 <ls_token>     LIKE LINE OF it_tokens.


* todo, add more indentation checks

  LOOP AT it_statements ASSIGNING <ls_statement>
      WHERE type <> scan_stmnt_type-comment
      AND type <> scan_stmnt_type-comment_in_stmnt
      AND type <> scan_stmnt_type-empty.

    READ TABLE it_tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
    IF sy-subrc = 0.
      lv_begin = <ls_token>-col.
    ELSE.
      CONTINUE. " current loop
    ENDIF.

    LOOP AT it_tokens ASSIGNING <ls_token>
        FROM <ls_statement>-from + 1 TO <ls_statement>-to.
      IF <ls_token>-col < lv_begin.
        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = get_include( p_level = <ls_statement>-level )
                p_line         = <ls_token>-row
                p_kind         = mv_errty
                p_test         = c_my_name
                p_code         = '001' ).
        EXIT. " current loop
      ENDIF.
    ENDLOOP.

  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Bad indentation'.                       "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Bad indentation'.                           "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT
ENDCLASS.