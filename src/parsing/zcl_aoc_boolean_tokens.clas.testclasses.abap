
CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      test_to_string IMPORTING iv_code TYPE string,
      to_string01 FOR TESTING,
      to_string02 FOR TESTING,
      to_string03 FOR TESTING,
      to_string04 FOR TESTING,
      test_replace
        IMPORTING
          iv_code     TYPE string
          iv_index    TYPE i
          iv_replace  TYPE string
          iv_expected TYPE string,
      replace01 FOR TESTING,
      parse
        IMPORTING iv_string        TYPE string
        RETURNING VALUE(ro_tokens) TYPE REF TO zcl_aoc_boolean_tokens.

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

    CREATE OBJECT ro_tokens EXPORTING it_tokens = lt_tokens.

  ENDMETHOD.

  METHOD test_to_string.

    DATA: lo_tokens TYPE REF TO zcl_aoc_boolean_tokens.

    lo_tokens = parse( iv_code ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_tokens->to_string( )
      exp = to_upper( iv_code ) ).

  ENDMETHOD.

  METHOD to_string01.
    test_to_string( 'foo = bar' ).
  ENDMETHOD.

  METHOD to_string02.
    test_to_string( 'moo = foo-bar' ).
  ENDMETHOD.

  METHOD to_string03.
    test_to_string( 'moo = foo->bar' ).
  ENDMETHOD.

  METHOD to_string04.
    test_to_string( 'moo = foo->bar( 2 )' ).
  ENDMETHOD.

  METHOD test_replace.

    DATA: lo_tokens TYPE REF TO zcl_aoc_boolean_tokens.

    lo_tokens = parse( iv_code ).

    lo_tokens->replace( iv_index = iv_index
                        iv_str   = iv_replace ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_tokens->to_string( )
      exp = to_upper( iv_expected ) ).

  ENDMETHOD.

  METHOD replace01.

    test_replace( iv_code     = 'moo boo foo'
                  iv_index    = 2
                  iv_replace  = 'baa'
                  iv_expected = 'moo baa foo' ).

  ENDMETHOD.

ENDCLASS.
