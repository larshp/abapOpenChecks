class ZCL_AOC_CHECK_07 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.
*"* public components of class ZCL_AOC_CHECK_07
*"* do not include other source files here!!!

  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
protected section.
*"* protected components of class ZCL_AOC_CHECK_07
*"* do not include other source files here!!!
private section.
ENDCLASS.



CLASS ZCL_AOC_CHECK_07 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lv_token   LIKE sy-tabix,
        lv_include TYPE sobj_name.

  FIELD-SYMBOLS: <ls_token>     LIKE LINE OF it_tokens,
                 <ls_token1>    LIKE LINE OF it_tokens,
                 <ls_token2>    LIKE LINE OF it_tokens,
                 <ls_token3>    LIKE LINE OF it_tokens,
                 <ls_statement> LIKE LINE OF it_statements.


  LOOP AT it_statements ASSIGNING <ls_statement>
      WHERE type = scan_stmnt_type-standard
      OR type = scan_stmnt_type-method_direct.

    lv_token = <ls_statement>-from.
    READ TABLE it_tokens ASSIGNING <ls_token1> INDEX lv_token.
    IF sy-subrc <> 0.
      CONTINUE. " current loop
    ENDIF.
    lv_token = lv_token + 1.
    READ TABLE it_tokens ASSIGNING <ls_token2> INDEX lv_token.
    IF sy-subrc <> 0.
      CONTINUE. " current loop
    ENDIF.
    lv_token = lv_token + 1.
    READ TABLE it_tokens ASSIGNING <ls_token3> INDEX lv_token.
    IF sy-subrc <> 0.
      CONTINUE. " current loop
    ENDIF.

    IF <ls_token3>-str CP '*>(*'
        OR <ls_token3>-str CP '*)=>*'
        OR <ls_token3>-str CP '*)->*'
        OR <ls_token3>-str CP '*(*'.
* allow dynamic calls
      CONTINUE.
    ENDIF.

    LOOP AT it_tokens ASSIGNING <ls_token>
        FROM <ls_statement>-from TO <ls_statement>-to
        WHERE str = 'RECEIVING'
        AND type = scan_token_type-identifier.

      IF <ls_token1>-str = 'CALL' OR <ls_token2>-str = 'BADI'.
        EXIT.
      ENDIF.

* allow if old style exceptions are part of method
      LOOP AT it_tokens TRANSPORTING NO FIELDS
          FROM <ls_statement>-from TO <ls_statement>-to
          WHERE str = 'EXCEPTIONS'
          AND type = scan_token_type-identifier.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        EXIT. " current loop
      ENDIF.

      lv_include = get_include( p_level = <ls_statement>-level ).
      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = lv_include
              p_line         = <ls_token>-row
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '002' ).
    ENDLOOP.

    IF  <ls_token1>-str <> 'CALL' OR  <ls_token2>-str <> 'METHOD'.
      CONTINUE. " current loop
    ENDIF.

    lv_include = get_include( p_level = <ls_statement>-level ).
    inform( p_sub_obj_type = c_type_include
            p_sub_obj_name = lv_include
            p_line         = <ls_token1>-row
            p_kind         = mv_errty
            p_test         = myname
            p_code         = '001' ).

  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Functional writing style for CALL METHOD'. "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Use functional writing style'.              "#EC NOTEXT
    WHEN '002'.
      p_text = 'Use functional writing style instead of RECEIVING'. "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT
ENDCLASS.