*----------------------------------------------------------------------*
*       CLASS lcl_Test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
* ================

    TYPES: ty_string_tt TYPE STANDARD TABLE OF string WITH DEFAULT KEY.

    DATA: mt_code       TYPE string_table,
          mt_tokens     TYPE stokesx_tab,
          mt_statements TYPE sstmnt_tab,
          mt_levels     TYPE slevel_tab,
          mt_structures TYPE zcl_aoc_super=>tt_structures.

    METHODS: build FOR TESTING.

    METHODS: to_string
      IMPORTING io_structure TYPE REF TO zcl_aoc_structure
      RETURNING value(rt_string) TYPE ty_string_tt.

    METHODS: parse.

ENDCLASS.       "lcl_Test

*----------------------------------------------------------------------*
*       CLASS lcl_Test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_test IMPLEMENTATION.
* ==============================

  DEFINE _code.
    append &1 to mt_code.
  END-OF-DEFINITION.

  METHOD parse.
    SCAN ABAP-SOURCE mt_code
         TOKENS          INTO mt_tokens
         STATEMENTS      INTO mt_statements
         LEVELS          INTO mt_levels
         STRUCTURES      INTO mt_structures
         WITH ANALYSIS
         WITH COMMENTS.
  ENDMETHOD.                    "parse

  METHOD build.

    DATA: lt_string TYPE ty_string_tt,
          lo_stru   TYPE REF TO zcl_aoc_structure.


    _code 'DATA: lv_foo TYPE i, lv_bar TYPE i.'.
    _code 'WRITE lv_foo.'.
    _code 'IF lv_foo = lv_bar.'.
    _code '  WRITE lv_foo.'.
    _code '  WRITE lv_foo.'.
    _code 'ELSEIF lv_foo = lv_bar.'.
    _code '  WRITE lv_foo.'.
    _code 'ELSE.'.
    _code '  WRITE lv_foo.'.
    _code 'ENDIF.'.
    _code 'WRITE lv_foo.'.

    parse( ).

    lo_stru = zcl_aoc_structure=>build(
                it_tokens     = mt_tokens
                it_statements = mt_statements
                it_levels     = mt_levels
                it_structures = mt_structures ).

    lt_string = to_string( lo_stru ).

  ENDMETHOD.       "build

  METHOD to_string.

    DATA: lv_string    TYPE string,
          lt_string    TYPE ty_string_tt,
          lo_structure TYPE REF TO zcl_aoc_structure.


    APPEND io_structure->ms_statement-statement TO rt_string.
    LOOP AT io_structure->mt_structure INTO lo_structure.
      lt_string = to_string( lo_structure ).
      LOOP AT lt_string INTO lv_string.
        lv_string = '__' && lv_string.
        APPEND lv_string TO rt_string.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.                    "to_string

ENDCLASS.       "lcl_Test