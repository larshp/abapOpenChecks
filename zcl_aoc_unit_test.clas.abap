class ZCL_AOC_UNIT_TEST definition
  public
  create public
  for testing .

public section.

*"* public components of class ZCL_AOC_UNIT_TEST
*"* do not include other source files here!!!
  class-methods HANDLER
    for event MESSAGE of CL_CI_TEST_ROOT
    importing
      !P_CHECKSUM_1
      !P_CODE
      !P_COLUMN
      !P_ERRCNT
      !P_KIND
      !P_LINE
      !P_PARAM_1
      !P_PARAM_2
      !P_PARAM_3
      !P_PARAM_4
      !P_SUB_OBJ_NAME
      !P_SUB_OBJ_TYPE
      !P_SUPPRESS
      !P_TEST
      !P_INCLSPEC .
  class-methods CHECK
    importing
      !IT_CODE type STRING_TABLE
    returning
      value(RS_RESULT) type SCIREST_AD .
  class-methods SET_CHECK
    importing
      !IO_CHECK type ref to ZCL_AOC_SUPER .
protected section.
*"* protected components of class ZCL_AOC_UNIT_TEST
*"* do not include other source files here!!!
private section.

*"* private components of class ZCL_AOC_UNIT_TEST
*"* do not include other source files here!!!
  class-data GS_RESULT type SCIREST_AD .
  class-data GO_CHECK type ref to ZCL_AOC_SUPER .
ENDCLASS.



CLASS ZCL_AOC_UNIT_TEST IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lt_tokens     TYPE stokesx_tab,
        lt_statements TYPE sstmnt_tab,
        lt_levels     TYPE slevel_tab,
        lt_structures TYPE zcl_aoc_super=>ty_structures_tt.


  SCAN ABAP-SOURCE it_code
       TOKENS          INTO lt_tokens
       STATEMENTS      INTO lt_statements
       LEVELS          INTO lt_levels
       STRUCTURES      INTO lt_structures
       WITH ANALYSIS
       WITH COMMENTS
       WITH PRAGMAS    abap_true.
  cl_abap_unit_assert=>assert_subrc( msg = 'Error while parsing'(001) ).

  CLEAR gs_result.
  SET HANDLER handler FOR go_check.

  go_check->set_source( iv_name = '----------------------------------------'
                        it_code = it_code ).

  go_check->check(
      it_tokens     = lt_tokens
      it_statements = lt_statements
      it_levels     = lt_levels
      it_structures = lt_structures ).

  rs_result = gs_result.

ENDMETHOD.


METHOD handler.

* assume only one result
  gs_result-sobjname = p_sub_obj_name.
  gs_result-sobjtype = p_sub_obj_type.
  gs_result-line     = p_line.
  gs_result-col      = p_column.
  gs_result-kind     = p_kind.
  gs_result-code     = p_code.

ENDMETHOD.


METHOD set_check.

  go_check = io_check.

ENDMETHOD.
ENDCLASS.