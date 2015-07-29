class ZCL_AOC_CHECK_16 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.

*"* public components of class ZCL_AOC_CHECK_16
*"* do not include other source files here!!!
  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
protected section.
*"* protected components of class ZCL_AOC_CHECK_16
*"* do not include other source files here!!!
private section.
ENDCLASS.



CLASS ZCL_AOC_CHECK_16 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lv_include TYPE sobj_name,
        lv_ok      TYPE token_row.

  FIELD-SYMBOLS: <ls_token>     LIKE LINE OF it_tokens,
                 <ls_statement> LIKE LINE OF it_statements.


  LOOP AT it_statements ASSIGNING <ls_statement>
      WHERE type <> scan_stmnt_type-empty
      AND type <> scan_stmnt_type-macro_definition
      AND type <> scan_stmnt_type-comment
      AND type <> scan_stmnt_type-comment_in_stmnt.

    LOOP AT it_tokens ASSIGNING <ls_token>
        FROM <ls_statement>-from TO <ls_statement>-to
        WHERE type <> scan_token_type-comment
        AND str <> ')'.
      lv_ok = <ls_token>-row.
    ENDLOOP.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    IF lv_ok <> <ls_statement>-trow.
      lv_include = get_include( p_level = <ls_statement>-level ).
      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = lv_include
              p_line         = <ls_statement>-trow
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '001' ).
    ENDIF.

  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Line only contains . or ).'.            "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.
  position       = '016'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Line contains only . or ).'.                "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT
ENDCLASS.