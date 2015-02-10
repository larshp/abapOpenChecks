class ZCL_AOC_STRUCTURE definition
  public
  create public .

public section.
*"* public components of class ZCL_AOC_STRUCTURE
*"* do not include other source files here!!!

  types:
    ty_structure_tt TYPE STANDARD TABLE OF REF TO zcl_aoc_structure WITH DEFAULT KEY .
  types TY_TYPE type CHAR1 .
  types:
    BEGIN OF ty_statement,
        statement TYPE string,
        level TYPE level_levl,
        row TYPE token_row,
      END OF ty_statement .

  data MS_STATEMENT type TY_STATEMENT read-only .
  data MT_STRUCTURE type TY_STRUCTURE_TT read-only .
  data MV_TYPE type STRU_TYPE read-only .
  data MV_STMNT_TYPE type STRU_TYPE read-only .

  class ZCL_AOC_SUPER definition load .
  class-methods BUILD
    importing
      !IT_TOKENS type STOKESX_TAB
      !IT_STATEMENTS type SSTMNT_TAB
      !IT_LEVELS type SLEVEL_TAB
      !IT_STRUCTURES type ZCL_AOC_SUPER=>TT_STRUCTURES
    returning
      value(RO_STRUCTURE) type ref to ZCL_AOC_STRUCTURE .
protected section.
*"* protected components of class ZCL_AOC_STRUCTURE
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_AOC_STRUCTURE
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_AOC_STRUCTURE IMPLEMENTATION.


METHOD build.

  DATA: lo_build TYPE REF TO lcl_build.


  CREATE OBJECT lo_build
    EXPORTING
      it_tokens     = it_tokens
      it_statements = it_statements
      it_levels     = it_levels
      it_structures = it_structures.

  ro_structure = lo_build->build( ).

ENDMETHOD.
ENDCLASS.