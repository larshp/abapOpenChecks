CLASS lcl_parse DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      parse
        IMPORTING iv_string        TYPE string
        RETURNING VALUE(ro_tokens) TYPE REF TO zcl_aoc_boolean_tokens.

ENDCLASS.

CLASS lcl_parse IMPLEMENTATION.

  METHOD parse.

    DATA: lt_code       TYPE string_table,
          lt_tokens     TYPE stokesx_tab,
          lt_statements TYPE sstmnt_tab ##NEEDED.


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

ENDCLASS.

CLASS ltcl_find_end_paren DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      test
        IMPORTING
          iv_code  TYPE string
          iv_start TYPE i
          iv_exp   TYPE i,
      find01 FOR TESTING,
      find02 FOR TESTING,
      find03 FOR TESTING.

ENDCLASS.

CLASS ltcl_find_end_paren IMPLEMENTATION.

  METHOD test.

    DATA: lv_end    TYPE i,
          lo_tokens TYPE REF TO zcl_aoc_boolean_tokens.


    lo_tokens = lcl_parse=>parse( iv_code ).

    lv_end = lo_tokens->find_end_paren( iv_start ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_end
      exp = iv_exp ).

  ENDMETHOD.

  METHOD find01.

    test( iv_code  = 'foo( )'
          iv_start = 1
          iv_exp   = 2 ).

  ENDMETHOD.

  METHOD find02.

    test( iv_code  = 'foo( )->bar( )'
          iv_start = 1
          iv_exp   = 3 ).

  ENDMETHOD.

  METHOD find03.

    test( iv_code  = 'foo( bar( ) )'
          iv_start = 1
          iv_exp   = 4 ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_to_string DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      test_to_string IMPORTING iv_code TYPE string,
      to_string01 FOR TESTING,
      to_string02 FOR TESTING,
      to_string03 FOR TESTING,
      to_string04 FOR TESTING.
ENDCLASS.

CLASS ltcl_to_string IMPLEMENTATION.

  METHOD test_to_string.

    DATA: lo_tokens TYPE REF TO zcl_aoc_boolean_tokens.

    lo_tokens = lcl_parse=>parse( iv_code ).

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

ENDCLASS.

CLASS ltcl_replace DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      test_replace
        IMPORTING
          iv_code     TYPE string
          iv_start    TYPE i
          iv_end      TYPE i OPTIONAL
          iv_replace  TYPE string
          iv_expected TYPE string,
      replace01 FOR TESTING,
      replace02 FOR TESTING.

ENDCLASS.

CLASS ltcl_replace IMPLEMENTATION.

  METHOD test_replace.

    DATA: lo_tokens TYPE REF TO zcl_aoc_boolean_tokens.

    lo_tokens = lcl_parse=>parse( iv_code )->replace( iv_start = iv_start
                                                      iv_end   = iv_end
                                                      iv_str   = iv_replace ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_tokens->to_string( )
      exp = to_upper( iv_expected ) ).

  ENDMETHOD.

  METHOD replace01.

    test_replace( iv_code     = 'moo boo foo'
                  iv_start    = 2
                  iv_replace  = 'baa'
                  iv_expected = 'moo baa foo' ).

  ENDMETHOD.

  METHOD replace02.

    test_replace( iv_code     = 'moo boo foo'
                  iv_start    = 2
                  iv_end      = 3
                  iv_replace  = 'baa'
                  iv_expected = 'moo baa' ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_test DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      test_remove IMPORTING iv_code     TYPE string
                            iv_index    TYPE i
                            iv_expected TYPE string,
      remove01 FOR TESTING.

ENDCLASS.       "ltcl_Test

CLASS ltcl_test IMPLEMENTATION.

  METHOD test_remove.

    DATA: lo_tokens TYPE REF TO zcl_aoc_boolean_tokens.

    lo_tokens = lcl_parse=>parse( iv_code )->remove( iv_index ).

    cl_abap_unit_assert=>assert_equals(
      act = lo_tokens->to_string( )
      exp = to_upper( iv_expected ) ).

  ENDMETHOD.

  METHOD remove01.

    test_remove( iv_code     = 'foo bar'
                 iv_index    = 2
                 iv_expected = 'foo' ).

  ENDMETHOD.

ENDCLASS.
