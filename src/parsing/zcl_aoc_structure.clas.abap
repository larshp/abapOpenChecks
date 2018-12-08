CLASS zcl_aoc_structure DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS zcl_aoc_super DEFINITION LOAD .

    TYPES:
      ty_structure_tt TYPE STANDARD TABLE OF REF TO zcl_aoc_structure WITH DEFAULT KEY .
    TYPES ty_type TYPE char1 .
    TYPES:
      ty_string_tt TYPE STANDARD TABLE OF string WITH DEFAULT KEY .
    TYPES:
      BEGIN OF ty_statement,
        statement TYPE string,
        level     TYPE level_levl,
        row       TYPE token_row,
      END OF ty_statement .

    DATA ms_statement TYPE ty_statement READ-ONLY .
    DATA mt_structure TYPE ty_structure_tt READ-ONLY .
    DATA mv_type TYPE stru_type READ-ONLY .
    DATA mv_stmnt_type TYPE stru_type READ-ONLY .

    CLASS-METHODS to_string
      IMPORTING
        !io_structure    TYPE REF TO zcl_aoc_structure
      RETURNING
        VALUE(rt_string) TYPE ty_string_tt .
    CLASS-METHODS to_string_simple
      IMPORTING
        !io_structure    TYPE REF TO zcl_aoc_structure
      RETURNING
        VALUE(rv_string) TYPE string .
    CLASS-METHODS build
      IMPORTING
        !it_tokens          TYPE stokesx_tab
        !it_statements      TYPE sstmnt_tab
        !it_structures      TYPE zcl_aoc_super=>ty_structures_tt
      RETURNING
        VALUE(ro_structure) TYPE REF TO zcl_aoc_structure .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_STRUCTURE IMPLEMENTATION.


  METHOD build.

    DATA: lo_build TYPE REF TO lcl_build.


    CREATE OBJECT lo_build
      EXPORTING
        it_tokens     = it_tokens
        it_statements = it_statements
        it_structures = it_structures.

    ro_structure = lo_build->build( ).

    ro_structure = lcl_simplify=>simplify( ro_structure ).

  ENDMETHOD.


  METHOD to_string.

    DATA: lv_string    TYPE string,
          lt_string    TYPE ty_string_tt,
          lv_temp      TYPE string,
          lo_structure TYPE REF TO zcl_aoc_structure.


    lv_temp = |{ io_structure->ms_statement-statement
      }, Children: { lines( io_structure->mt_structure )
      }, Type: { io_structure->mv_type
      }, Stmnt type: { io_structure->mv_stmnt_type }|.
    APPEND lv_temp TO rt_string.
    LOOP AT io_structure->mt_structure INTO lo_structure.
      lt_string = to_string( lo_structure ).
      LOOP AT lt_string INTO lv_string.
        lv_string = '__' && lv_string.
        APPEND lv_string TO rt_string.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD to_string_simple.

    DATA: lo_structure TYPE REF TO zcl_aoc_structure.


    rv_string = io_structure->ms_statement-statement.
    LOOP AT io_structure->mt_structure INTO lo_structure.
      rv_string = rv_string &&
        cl_abap_char_utilities=>newline &&
        to_string_simple( lo_structure ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
