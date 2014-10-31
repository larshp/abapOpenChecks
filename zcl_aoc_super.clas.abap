class ZCL_AOC_SUPER definition
  public
  inheriting from CL_CI_TEST_SCAN
  abstract
  create public .

public section.
*"* public components of class ZCL_AOC_SUPER
*"* do not include other source files here!!!

  types:
    TT_STRUCTURES type standard table of SSTRUC .

  methods CHECK
    importing
      !IT_TOKENS type STOKESX_TAB
      !IT_STATEMENTS type SSTMNT_TAB
      !IT_LEVELS type SLEVEL_TAB
      !IT_STRUCTURES type TT_STRUCTURES .
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
      !IS_LEVEL type SLEVEL
    returning
      value(RT_CODE) type STRING_TABLE .

  methods GET_INCLUDE
    redefinition .
  methods INFORM
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

  DATA: lv_url TYPE string VALUE 'https://github.com/larshp/abapOpenChecks/wiki/'.


  CONCATENATE lv_url iv_name INTO lv_url.

  cl_gui_frontend_services=>execute( document = lv_url ).

ENDMETHOD.


METHOD get_include.

  IF p_level = 0.
* in case INCLUDE doesnt exist in the system
    RETURN.
  ENDIF.

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


  IF is_level-type = scan_level_type-macro_define
      OR is_level-type = scan_level_type-macro_trmac.
    RETURN.
  ENDIF.

  READ TABLE mt_source ASSIGNING <ls_source> WITH KEY name = is_level-name.
  IF sy-subrc = 0.
    rt_code = <ls_source>-code.
  ELSE.
    READ REPORT is_level-name INTO rt_code.            "#EC CI_READ_REP
    ASSERT sy-subrc = 0.

    ls_source-name = is_level-name.
    ls_source-code = rt_code.
    INSERT ls_source INTO TABLE mt_source.
  ENDIF.

ENDMETHOD.


METHOD inform.

* skip standard code, todo: namespaces
  IF p_sub_obj_name(1) <> 'Z'
      AND p_sub_obj_name(1) <> 'Y'
      AND p_sub_obj_name <> ''
      AND p_sub_obj_name <> '----------------------------------------'.
    RETURN.
  ENDIF.

  super->inform(
      p_sub_obj_type = p_sub_obj_type
      p_sub_obj_name = p_sub_obj_name
      p_position     = p_position
      p_line         = p_line
      p_column       = p_column
      p_errcnt       = p_errcnt
      p_kind         = p_kind
      p_test         = p_test
      p_code         = p_code
      p_suppress     = p_suppress
      p_param_1      = p_param_1
      p_param_2      = p_param_2
      p_param_3      = p_param_3
      p_param_4      = p_param_4
      p_inclspec     = p_inclspec
* parameters p_detail and p_checksum_1 does not exist in 730
      ).

ENDMETHOD.


METHOD run.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

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

  IF <ls_statement>-from <= <ls_statement>-to.
    READ TABLE it_tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
    ASSERT sy-subrc = 0.

    rv_result = <ls_token>-str.
  ENDIF.

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