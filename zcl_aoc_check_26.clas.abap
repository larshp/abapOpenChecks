class ZCL_AOC_CHECK_26 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.
*"* public components of class ZCL_AOC_CHECK_26
*"* do not include other source files here!!!

  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
protected section.
*"* protected components of class ZCL_AOC_CHECK_26
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_AOC_CHECK_26
*"* do not include other source files here!!!

  constants C_MY_NAME type SEOCLSNAME value 'ZCL_AOC_CHECK_26'. "#EC NOTEXT
ENDCLASS.



CLASS ZCL_AOC_CHECK_26 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lv_keyword   TYPE string,
        lt_code      TYPE string_table,
        ls_result    TYPE zcl_aoc_parser=>st_result,
        lv_statement TYPE string.

  FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                 <ls_rt> LIKE LINE OF ls_result-tokens,
                 <ls_token> LIKE LINE OF it_tokens.


  LOOP AT it_statements ASSIGNING <ls_statement> WHERE type = scan_stmnt_type-standard..

    CLEAR lv_keyword.
    CLEAR lv_statement.

    LOOP AT it_tokens ASSIGNING <ls_token>
        FROM <ls_statement>-from
        TO <ls_statement>-to
        WHERE type <> scan_token_type-comment.

      IF lv_keyword IS INITIAL.
        lv_keyword = <ls_token>-str.
      ENDIF.

      IF lv_statement IS INITIAL.
        lv_statement = <ls_token>-str.
      ELSE.
        CONCATENATE lv_statement <ls_token>-str INTO lv_statement SEPARATED BY space.
      ENDIF.
    ENDLOOP.

    IF lv_keyword <> 'UPDATE'
        AND lv_keyword <> 'MODIFY'
        AND lv_keyword <> 'DELETE'
        AND lv_keyword <> 'INSERT'.
      CONTINUE. " current loop
    ENDIF.

    CLEAR lt_code.
    APPEND lv_statement TO lt_code.
    ls_result = zcl_aoc_parser=>run( it_code = lt_code
                                     iv_rule = lv_keyword ).

    IF ls_result-match = abap_false.
      CONTINUE.
    ENDIF.

* first token typed C_ROLE?
* todo, work in progress
*    BREAK-POINT.

  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'No direct changes to standard tables'.  "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'No direct changes to standard tables'.      "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT
ENDCLASS.