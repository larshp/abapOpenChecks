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
  methods SET_SOURCE
    importing
      !IV_NAME type LEVEL_NAME
      !IT_CODE type STRING_TABLE .

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
  methods GET_SOURCE
    importing
      !IV_NAME type LEVEL_NAME
    returning
      value(RT_CODE) type STRING_TABLE .

  methods GET_INCLUDE
    redefinition .
PRIVATE SECTION.
*"* private components of class ZCL_AOC_SUPER
*"* do not include other source files here!!!

  TYPES:
    BEGIN OF st_source,
           name TYPE level_name,
           code TYPE string_table,
         END OF st_source .
  TYPES:
    tt_source TYPE SORTED TABLE OF st_source WITH UNIQUE KEY name .

  DATA mt_source TYPE tt_source .
ENDCLASS.



CLASS ZCL_AOC_SUPER IMPLEMENTATION.


METHOD check.

* add code here
  ASSERT 1 = 1 + 1.

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


METHOD get_include.

  IF ref_scan IS BOUND.
* not bound during unit testing
    p_result = super->get_include(
        p_ref_scan = p_ref_scan
        p_level    = p_level ).
  ENDIF.

ENDMETHOD.


METHOD get_source.

  DATA: ls_source LIKE LINE OF mt_source.

  FIELD-SYMBOLS: <ls_source> LIKE LINE OF mt_source.


  IF iv_name(1) = '_'
      OR iv_name = 'FILL_ATT'
      OR iv_name = 'FILL_ATT_RB'.
    RETURN.
  ENDIF.

  READ TABLE mt_source ASSIGNING <ls_source> WITH KEY name = iv_name.
  IF sy-subrc = 0.
    rt_code = <ls_source>-code.
  ELSE.
    READ REPORT iv_name INTO rt_code.                  "#EC CI_READ_REP
    ASSERT sy-subrc = 0.

    ls_source-name = iv_name.
    ls_source-code = rt_code.
    INSERT ls_source INTO TABLE mt_source.
  ENDIF.

ENDMETHOD.


METHOD run.

  CLEAR mt_source[].  " limit memory use

  CHECK program_name IS NOT INITIAL.
  IF ref_scan IS INITIAL.
    CHECK get( ) = abap_true.
  ENDIF.

  set_source( iv_name = ref_include->trdir-name
              it_code = ref_include->lines ).

  check( it_tokens     = ref_scan->tokens
         it_statements = ref_scan->statements
         it_levels     = ref_scan->levels
         it_structures = ref_scan->structures ).

ENDMETHOD.


METHOD set_source.

* used for unit testing

  DATA: ls_source LIKE LINE OF mt_source.


  ls_source-name = iv_name.
  ls_source-code = it_code.

  INSERT ls_source INTO TABLE mt_source.

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