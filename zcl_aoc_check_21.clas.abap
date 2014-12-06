class ZCL_AOC_CHECK_21 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.
*"* public components of class ZCL_AOC_CHECK_21
*"* do not include other source files here!!!

  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
protected section.

*"* protected components of class ZCL_AOC_CHECK_21
*"* do not include other source files here!!!
  methods BUILD_STATEMENT
    importing
      !IS_STATEMENT type SSTMNT
      !IT_TOKENS type STOKESX_TAB
    returning
      value(RV_STATEMENT) type STRING .
  methods FIND_PARAMETERS
    importing
      !IV_STATEMENT type STRING
    returning
      value(RT_PARAMETERS) type STRING_TABLE .
private section.
*"* private components of class ZCL_AOC_CHECK_21
*"* do not include other source files here!!!

  constants C_MY_NAME type SEOCLSNAME value 'ZCL_AOC_CHECK_21'. "#EC NOTEXT
ENDCLASS.



CLASS ZCL_AOC_CHECK_21 IMPLEMENTATION.


METHOD build_statement.

  FIELD-SYMBOLS: <ls_token> LIKE LINE OF it_tokens.


  LOOP AT it_tokens ASSIGNING <ls_token> FROM is_statement-from TO is_statement-to.
    IF rv_statement IS INITIAL.
      rv_statement = <ls_token>-str.
    ELSE.
      CONCATENATE rv_statement <ls_token>-str
        INTO rv_statement SEPARATED BY space.
    ENDIF.
  ENDLOOP.
  CONCATENATE rv_statement '.' INTO rv_statement.

ENDMETHOD.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lt_parameters TYPE string_table,
        lv_parameter  LIKE LINE OF lt_parameters,
        lv_statement  TYPE string,
        ls_form_stmnt LIKE LINE OF it_statements,
        lv_form       TYPE abap_bool.

  FIELD-SYMBOLS: <ls_structure> LIKE LINE OF it_structures,
                 <ls_token>     LIKE LINE OF it_tokens,
                 <ls_statement> LIKE LINE OF it_statements.


  LOOP AT it_structures ASSIGNING <ls_structure>
      WHERE type = scan_struc_type-routine
      AND stmnt_type = scan_struc_stmnt_type-form.

    lv_form = abap_true.

    LOOP AT it_statements ASSIGNING <ls_statement>
        FROM <ls_structure>-stmnt_from
        TO <ls_structure>-stmnt_to
        WHERE type <> scan_stmnt_type-empty
        AND type <> scan_stmnt_type-comment
        AND type <> scan_stmnt_type-comment_in_stmnt.

      lv_statement = build_statement(
          is_statement = <ls_statement>
          it_tokens    = it_tokens ).

      IF lv_form = abap_true.
        ls_form_stmnt = <ls_statement>.
        lt_parameters = find_parameters( lv_statement ).
        lv_form = abap_false.
        CONTINUE.
      ENDIF.

      LOOP AT lt_parameters INTO lv_parameter.
        IF lv_statement CS lv_parameter.
          DELETE lt_parameters INDEX sy-tabix.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

    LOOP AT lt_parameters INTO lv_parameter.
      READ TABLE it_tokens INDEX ls_form_stmnt-from ASSIGNING <ls_token>.

      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = get_include( p_level = ls_form_stmnt-level )
              p_line         = <ls_token>-row
              p_kind         = mv_errty
              p_test         = c_my_name
              p_code         = '001'
              p_param_1      = lv_parameter ).
    ENDLOOP.
  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Unused FORM parameter'.                 "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty       = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD find_parameters.

  DATA: lt_code   TYPE TABLE OF string,
        ls_result TYPE zcl_aoc_parser=>st_result.

  FIELD-SYMBOLS: <ls_res_tok> LIKE LINE OF ls_result-tokens.


  APPEND iv_statement TO lt_code.
  ls_result = zcl_aoc_parser=>run(
      it_code = lt_code
      iv_rule = 'FORM' ).

  LOOP AT ls_result-tokens ASSIGNING <ls_res_tok>
      WHERE value = zcl_aoc_parser=>c_role_fielddefid.
    APPEND <ls_res_tok>-code TO rt_parameters.
  ENDLOOP.

ENDMETHOD.


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Unused FORM parameter &1'.                  "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT
ENDCLASS.