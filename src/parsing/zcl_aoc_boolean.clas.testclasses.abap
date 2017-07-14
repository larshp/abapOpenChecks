CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      parse IMPORTING iv_string        TYPE string
            RETURNING VALUE(rv_result) TYPE string,
      test001 FOR TESTING,
      test002 FOR TESTING,
      test003 FOR TESTING,
      test004 FOR TESTING,
      test005 FOR TESTING,
      test006 FOR TESTING,
      test007 FOR TESTING,
      test008 FOR TESTING,
      test009 FOR TESTING,
      test010 FOR TESTING,
      test011 FOR TESTING.

ENDCLASS.       "ltcl_Test

CLASS ltcl_test IMPLEMENTATION.

  METHOD parse.

    DATA: lt_code       TYPE string_table,
          lt_tokens     TYPE stokesx_tab,
          lo_node       TYPE REF TO zcl_aoc_boolean_node,
          lt_statements TYPE sstmnt_tab.


    APPEND iv_string TO lt_code.

    SCAN ABAP-SOURCE lt_code
         TOKENS        INTO lt_tokens
         STATEMENTS    INTO lt_statements
         WITH ANALYSIS
         WITH COMMENTS
         WITH PRAGMAS  abap_true.
    cl_abap_unit_assert=>assert_subrc( ).

    DELETE lt_tokens TO 1.
    cl_abap_unit_assert=>assert_subrc( ).

    lo_node = zcl_aoc_boolean=>parse( lt_tokens ).
    cl_abap_unit_assert=>assert_bound( lo_node ).

    rv_result = lo_node->to_string( ).

  ENDMETHOD.

  METHOD test001.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF foo = bar.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE').
  ENDMETHOD.

  METHOD test002.
* 'IF foo NE bar.'
  ENDMETHOD.

  METHOD test003.
* 'IF foo <> bar.'
  ENDMETHOD.

  METHOD test004.
* 'IF foo( ) = bar( ).'
  ENDMETHOD.

  METHOD test005.
* 'IF ( foo = bar ).'
  ENDMETHOD.

  METHOD test006.
* 'IF foo = bar AND moo = boo.'
  ENDMETHOD.

  METHOD test007.
* 'IF ( foo = bar AND moo = boo ).'
  ENDMETHOD.

  METHOD test008.
* 'IF ( foo = bar ) AND ( moo = boo ).'
  ENDMETHOD.

  METHOD test009.
* 'IF ( ( foo = bar ) AND ( moo = boo ) ).'
  ENDMETHOD.

  METHOD test010.
* 'IF method( value ) = 1.'
  ENDMETHOD.

  METHOD test011.
* 'IF method( field = value ) = 1.'
  ENDMETHOD.

ENDCLASS.
