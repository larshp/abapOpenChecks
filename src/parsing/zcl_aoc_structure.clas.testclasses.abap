*----------------------------------------------------------------------*
*       CLASS lcl_Test DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_test DEFINITION FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
  FINAL.

  PRIVATE SECTION.
* ================

    DATA: mt_code       TYPE string_table,
          mt_tokens     TYPE stokesx_tab,
          mt_statements TYPE sstmnt_tab,
          mt_structures TYPE zcl_aoc_super=>ty_structures_tt.

    METHODS:
      build01 FOR TESTING,
      build02 FOR TESTING.

    METHODS: parse.

ENDCLASS.       "lcl_Test

*----------------------------------------------------------------------*
*       CLASS lcl_Test IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS ltcl_test IMPLEMENTATION.
* ==============================

  DEFINE _code.
    APPEND &1 TO mt_code.
  END-OF-DEFINITION.

  METHOD parse.
    SCAN ABAP-SOURCE mt_code
         TOKENS          INTO mt_tokens
         STATEMENTS      INTO mt_statements
         STRUCTURES      INTO mt_structures
         WITH ANALYSIS
         WITH COMMENTS.
  ENDMETHOD.                    "parse

  METHOD build01.

    DATA: lt_string TYPE zcl_aoc_structure=>ty_string_tt,
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
      it_structures = mt_structures ).

    lt_string = zcl_aoc_structure=>to_string( lo_stru ).
    cl_abap_unit_assert=>assert_not_initial( lt_string ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_string )
      exp = 14 ).

  ENDMETHOD.       "build

  METHOD build02.

    DATA: lt_string TYPE zcl_aoc_structure=>ty_string_tt,
          lo_stru   TYPE REF TO zcl_aoc_structure.


    _code 'CASE lv_foo.'.
    _code '  WHEN ''a''.'.
    _code '    lv_moo = lv_boo.'.
    _code '  WHEN OTHERS.'.
    _code '    lv_moo = lv_boo.'.
    _code 'ENDCASE.'.

    parse( ).

    lo_stru = zcl_aoc_structure=>build(
      it_tokens     = mt_tokens
      it_statements = mt_statements
      it_structures = mt_structures ).

    lt_string = zcl_aoc_structure=>to_string( lo_stru ).
    cl_abap_unit_assert=>assert_not_initial( lt_string ).

    cl_abap_unit_assert=>assert_equals(
      act = lines( lt_string )
      exp = 6 ).

  ENDMETHOD.

ENDCLASS.       "lcl_Test
