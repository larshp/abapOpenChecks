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
      !IO_CHECK type ref to ZCL_AOC_SUPER
    returning
      value(RS_RESULT) type SCIREST_AD .
protected section.
*"* protected components of class ZCL_AOC_UNIT_TEST
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_AOC_UNIT_TEST
*"* do not include other source files here!!!

  class-data GS_RESULT type SCIREST_AD .
ENDCLASS.



CLASS ZCL_AOC_UNIT_TEST IMPLEMENTATION.


METHOD check.

  DATA: lt_tokens     TYPE stokesx_tab,
        lt_statements TYPE sstmnt_tab,
        lt_levels     TYPE slevel_tab,
        lt_structures TYPE sstruc_tab.


  SCAN ABAP-SOURCE it_code
       TOKENS          INTO lt_tokens
       STATEMENTS      INTO lt_statements
       LEVELS          INTO lt_levels
       STRUCTURES      INTO lt_structures
       WITH ANALYSIS
       WITH COMMENTS.

  CLEAR GS_RESULT.
  SET HANDLER handler FOR io_check.

  io_check->set_source( iv_name = '----------------------------------------'
                        it_code = it_code ).

  io_check->check(
      it_tokens     = lt_tokens
      it_statements = lt_statements
      it_levels     = lt_levels
      it_structures = lt_structures ).

  rs_result = GS_RESULT.

ENDMETHOD.


METHOD handler.

* assume only one result
  GS_RESULT-sobjname = p_sub_obj_name.
  GS_RESULT-sobjtype = p_sub_obj_type.
  GS_RESULT-line     = p_line.
  GS_RESULT-col      = p_column.
  GS_RESULT-kind     = p_kind.
  GS_RESULT-code     = p_code.

ENDMETHOD.
ENDCLASS.