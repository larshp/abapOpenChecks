class ZCL_AOC_CHECK_23 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.
*"* public components of class ZCL_AOC_CHECK_23
*"* do not include other source files here!!!

  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
protected section.
*"* protected components of class ZCL_AOC_CHECK_23
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_AOC_CHECK_23
*"* do not include other source files here!!!

  constants C_MY_NAME type SEOCLSNAME value 'ZCL_AOC_CHECK_23'. "#EC NOTEXT
ENDCLASS.



CLASS ZCL_AOC_CHECK_23 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lv_keyword TYPE string,
        lv_line    TYPE token_row,
        lv_include TYPE program,
        lv_index   LIKE sy-tabix.

  FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements.


  LOOP AT it_statements ASSIGNING <ls_statement>.
    lv_index = sy-tabix.

    lv_keyword = statement_keyword(
        iv_number     = lv_index
        it_statements = it_statements
        it_tokens     = it_tokens ).

    IF lv_keyword <> 'CHECK'.
      CONTINUE. " current loop
    ENDIF.

    LOOP AT it_structures TRANSPORTING NO FIELDS
        WHERE ( stmnt_type = scan_struc_stmnt_type-loop
        OR stmnt_type = scan_struc_stmnt_type-while
        OR stmnt_type = scan_struc_stmnt_type-do
        OR stmnt_type = scan_struc_stmnt_type-select )
        AND stmnt_from <= lv_index
        AND stmnt_to >= lv_index.
      EXIT. " current loop
    ENDLOOP.
    IF sy-subrc <> 0.
      lv_line = statement_row(
        iv_number     = lv_index
        it_statements = it_statements
        it_tokens     = it_tokens ).

      lv_include = get_include( p_level = <ls_statement>-level ).

      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = lv_include
              p_line = lv_line
              p_kind = mv_errty
              p_test = c_my_name
              p_code = '001' ).
    ENDIF.

  ENDLOOP.

ENDMETHOD.


METHOD constructor .

  super->constructor( ).

  description    = 'CHECK outside of LOOP'.                 "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'CHECK outside of loop'.                     "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT
ENDCLASS.