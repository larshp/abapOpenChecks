class ZCL_AOC_SUPER definition
  public
  inheriting from CL_CI_TEST_SCAN
  abstract
  create public .

public section.
*"* public components of class ZCL_AOC_SUPER
*"* do not include other source files here!!!

  methods CHECK
    importing
      !IT_TOKENS type STOKESX_TAB
      !IT_STATEMENTS type SSTMNT_TAB
      !IT_LEVELS type SLEVEL_TAB
      !IT_STRUCTURES type SSTRUC_TAB .

  methods RUN
    redefinition .
protected section.
*"* protected components of class ZCL_AOC_SUPER
*"* do not include other source files here!!!

  class-methods STATEMENT_KEYWORD
    importing
      !IV_NUMBER type STMNT_NR
      !IT_STATEMENTS type SSTMNT_TAB
      !IT_TOKENS type STOKESX_TAB
    returning
      value(RV_RESULT) type STRING .
  class-methods STATEMENT_ROW
    importing
      !IV_NUMBER type STMNT_NR
      !IT_STATEMENTS type SSTMNT_TAB
      !IT_TOKENS type STOKESX_TAB
    returning
      value(RV_RESULT) type TOKEN_ROW .
  class-methods DOCUMENTATION
    importing
      !IV_NAME type SEOCLSNAME .
private section.
*"* private components of class ZCL_AOC_SUPER
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_AOC_SUPER IMPLEMENTATION.


METHOD check.

* add code here
  ASSERT 1 = 2.

ENDMETHOD.


METHOD documentation.

  DATA: lv_object TYPE dokhl-object.


  lv_object = iv_name.

  CALL FUNCTION 'DOCU_CALL'
    EXPORTING
      displ      = abap_true
      displ_mode = 2
      id         = 'CL'
      langu      = 'E'
      object     = lv_object.

ENDMETHOD.


METHOD run.

  CHECK program_name IS NOT INITIAL.
  IF ref_scan IS INITIAL.
    CHECK get( ) = abap_true.
  ENDIF.

  check( it_tokens     = ref_scan->tokens
         it_statements = ref_scan->statements
         it_levels     = ref_scan->levels
         it_structures = ref_scan->structures ).

ENDMETHOD.


METHOD statement_keyword.

  FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                 <ls_token>     LIKE LINE OF it_tokens.


  READ TABLE it_statements ASSIGNING <ls_statement> INDEX iv_number.
  ASSERT sy-subrc = 0.

  READ TABLE it_tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
  ASSERT sy-subrc = 0.

  rv_result = <ls_token>-str.

ENDMETHOD.


METHOD statement_row.

  FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                 <ls_token>     LIKE LINE OF it_tokens.


  READ TABLE it_statements ASSIGNING <ls_statement> INDEX iv_number.
  ASSERT sy-subrc = 0.

  READ TABLE it_tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
  ASSERT sy-subrc = 0.

  rv_result = <ls_token>-row.

ENDMETHOD.
ENDCLASS.