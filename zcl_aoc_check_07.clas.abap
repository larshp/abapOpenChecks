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
  methods GET_ATTRIBUTES
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
  methods IF_CI_TEST~DISPLAY_DOCUMENTATION
    redefinition .
  methods IF_CI_TEST~QUERY_ATTRIBUTES
    redefinition .
  methods PUT_ATTRIBUTES
    redefinition .
protected section.
*"* protected components of class ZCL_AOC_CHECK_07
*"* do not include other source files here!!!

  data MV_ERRTY type SCI_ERRTY .
private section.
*"* private components of class ZCL_AOC_CHECK_07
*"* do not include other source files here!!!

  constants C_MY_NAME type SEOCLSNAME value 'ZCL_AOC_CHECK_07'. "#EC NOTEXT
ENDCLASS.



CLASS ZCL_AOC_CHECK_07 IMPLEMENTATION.


METHOD check.

  DATA: lv_token     LIKE sy-tabix,
        lv_include   TYPE sobj_name.

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

    IF <ls_token3>-str CP '*>(*'.
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

      lv_include = get_include( p_level = <ls_statement>-level ).
      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = lv_include
              p_line         = <ls_token>-row
              p_kind         = mv_errty
              p_test         = c_my_name
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
            p_test         = c_my_name
            p_code         = '001' ).

  ENDLOOP.

ENDMETHOD.


METHOD constructor .

  super->constructor( ).

  description    = 'Functional writing style for CALL METHOD'. "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_attributes.

  EXPORT
    mv_errty = mv_errty
    TO DATA BUFFER p_attributes.

ENDMETHOD.


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


METHOD if_ci_test~display_documentation.

  documentation( c_my_name ).

ENDMETHOD.


METHOD if_ci_test~query_attributes.

  DATA: lv_ok         TYPE abap_bool,
        lv_message    TYPE c LENGTH 72,
        lt_attributes TYPE sci_atttab,
        ls_attribute  LIKE LINE OF lt_attributes.

  DEFINE fill_att.
    clear ls_attribute.
    get reference of &1 into ls_attribute-ref.
    ls_attribute-text = &2.
    ls_attribute-kind = &3.
    append ls_attribute to lt_attributes.
  END-OF-DEFINITION.


  fill_att mv_errty 'Error Type' ''.                        "#EC NOTEXT

  WHILE lv_ok = abap_false.
    cl_ci_query_attributes=>generic(
                          p_name       = c_my_name
                          p_title      = 'Options'
                          p_attributes = lt_attributes
                          p_message    = lv_message
                          p_display    = p_display ).       "#EC NOTEXT
    IF mv_errty = c_error OR mv_errty = c_warning OR mv_errty = c_note.
      lv_ok = abap_true.
    ELSE.
      lv_message = 'Fill attributes'.                       "#EC NOTEXT
    ENDIF.
  ENDWHILE.

ENDMETHOD.


METHOD put_attributes.

  IMPORT
    mv_errty = mv_errty
    FROM DATA BUFFER p_attributes.                   "#EC CI_USE_WANTED

ENDMETHOD.
ENDCLASS.