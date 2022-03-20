CLASS lcl_parse DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      parse IMPORTING iv_code          TYPE string
            RETURNING VALUE(ro_tokens) TYPE REF TO zcl_aoc_boolean_tokens.

ENDCLASS.

CLASS lcl_parse IMPLEMENTATION.

  METHOD parse.

    DATA: lt_code       TYPE string_table,
          lt_statements TYPE sstmnt_tab ##NEEDED,
          lt_tokens     TYPE stokesx_tab.


    APPEND iv_code TO lt_code.

    SCAN ABAP-SOURCE lt_code
         TOKENS INTO lt_tokens
         STATEMENTS INTO lt_statements
         WITH ANALYSIS
         WITH COMMENTS
         WITH PRAGMAS abap_true.
    cl_abap_unit_assert=>assert_subrc( ).

    CREATE OBJECT ro_tokens EXPORTING it_tokens = lt_tokens.

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_parse DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

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
      test011 FOR TESTING,
      test012 FOR TESTING,
      test013 FOR TESTING,
      test014 FOR TESTING,
      test015 FOR TESTING,
      test016 FOR TESTING,
      test017 FOR TESTING,
      test018 FOR TESTING,
      test019 FOR TESTING,
      test020 FOR TESTING,
      test021 FOR TESTING,
      test022 FOR TESTING,
      test023 FOR TESTING,
      test024 FOR TESTING,
      test025 FOR TESTING,
      test026 FOR TESTING,
      test027 FOR TESTING,
      test028 FOR TESTING,
      test029 FOR TESTING,
      test030 FOR TESTING,
      test031 FOR TESTING,
      test032 FOR TESTING,
      test033 FOR TESTING,
      test034 FOR TESTING,
      test035 FOR TESTING,
      test036 FOR TESTING,
      test037 FOR TESTING,
      test038 FOR TESTING,
      test039 FOR TESTING,
      test040 FOR TESTING,
      test041 FOR TESTING,
      test042 FOR TESTING,
      test043 FOR TESTING,
      test044 FOR TESTING,
      test045 FOR TESTING,
      test046 FOR TESTING,
      test047 FOR TESTING,
      test048 FOR TESTING,
      test049 FOR TESTING,
      test050 FOR TESTING,
      test051 FOR TESTING,
      test052 FOR TESTING.

ENDCLASS.       "ltcl_Test

CLASS ltcl_parse IMPLEMENTATION.

  METHOD parse.

    DATA: lt_tokens TYPE stokesx_tab,
          lo_node   TYPE REF TO zcl_aoc_boolean_node.


    lt_tokens = lcl_parse=>parse( iv_string )->remove( 1 )->get_tokens( ).

    lo_node = zcl_aoc_boolean=>parse( lt_tokens ).
    cl_abap_unit_assert=>assert_bound( lo_node ).

    rv_result = lo_node->to_string( ).

  ENDMETHOD.

  METHOD test001.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF foo = bar.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test002.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF foo NE bar.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test003.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF foo <> bar.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test004.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF foo( ) = bar( ).' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test005.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF ( foo = bar ).' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = '( COMPARE )' ).
  ENDMETHOD.

  METHOD test006.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF foo = bar AND moo = boo.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'AND( COMPARE COMPARE )' ).
  ENDMETHOD.

  METHOD test007.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF ( foo = bar AND moo = boo ).' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = '( AND( COMPARE COMPARE ) )' ).
  ENDMETHOD.

  METHOD test008.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF ( foo = bar ) AND ( moo = boo ).' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'AND( ( COMPARE ) ( COMPARE ) )' ).
  ENDMETHOD.

  METHOD test009.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF ( ( foo = bar ) AND ( moo = boo ) ).' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = '( AND( ( COMPARE ) ( COMPARE ) ) )' ).
  ENDMETHOD.

  METHOD test010.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF method( value ) = 1.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test011.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF method( field = value ) = 1.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test012.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF method( VALUE #( foo = bar ) ) = 1.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test013.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF foo->method( field = value ) = 1.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test014.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF moo-foo->method( field = value ) = 1.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test015.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF method1( method2( ) ) = 1.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test016.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF NOT 2 = 3.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'NOT( COMPARE )' ).
  ENDMETHOD.

  METHOD test017.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF NOT 2 = 3 AND NOT 1 = 2.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'AND( NOT( COMPARE ) NOT( COMPARE ) )' ).
  ENDMETHOD.

  METHOD test018.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF NOT ( 2 = 3 AND NOT 1 = 2 ).' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'NOT( ( AND( COMPARE NOT( COMPARE ) ) ) )' ).
  ENDMETHOD.

  METHOD test019.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF 2 = 3 AND NOT 1 = 2.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'AND( COMPARE NOT( COMPARE ) )' ).
  ENDMETHOD.

  METHOD test020.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF 2 = 3 AND NOT 1 = 2 AND 3 = 1.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'AND( COMPARE AND( NOT( COMPARE ) COMPARE ) )' ).
  ENDMETHOD.

  METHOD test021.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF 2 = 3 OR 3 = 1.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'OR( COMPARE COMPARE )' ).
  ENDMETHOD.

  METHOD test022.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF foo IS INITLAL.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test023.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF foo IS NOT INITIAL.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test024.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF foo = moo + 1.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test025.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF foo = moo - boo + 1.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test026.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF lines( lt_cache ) > 0.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test027.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF <ls_visit>-object->type( ) = 2.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test028.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF foo IN bar.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test029.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF foo NOT IN bar.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test030.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF var + strlen( var ) >= 1.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test031.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF lo_object->zif_name~method( ) = 2.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test032.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF lv_x BIT-AND lc_128 = lc_128.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test033.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF var BETWEEN moo AND foo.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test034.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF ( lv_offset + 1 ) MOD 8 = 0.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test035.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF lv_foo+0(1) EQ foo.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test036.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF foo EQ |A|.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test037.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF lv_path = mv_base && mv_swagger_html.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test038.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF foo < bar * ( moo + 1 ).' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test039.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF lt_foo[ 1 ]-address CS ''bar''.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test040.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF boolean_function( bar ).' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test041.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF boolean_function( bar ) AND another( ).' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'AND( COMPARE COMPARE )' ).
  ENDMETHOD.

  METHOD test042.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF method( )-field = 2.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test043.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF lv_line CS |FOO({ lv_name })|.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test044.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF lv_blah IS INSTANCE OF if_sxml_value_node.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test045.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF moo EQ foo OR ( baa EQ bar AND lr_object->/ui2/moo~method( ) = space ).' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'OR( COMPARE ( AND( COMPARE COMPARE ) ) )' ).
  ENDMETHOD.

  METHOD test046.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF lr_object->/ui2/if_edm_model_entity~get_entity_name( ) = space.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test047.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF SY-DATUM + 1 > + SY-DATUM.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test048.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF abap_true = lines( tab ) AND ( NOT line_' &&
      'exists( tab[ bname = ''ASDF'' ] ) OR sy-abcde = abap_true ).' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'AND( COMPARE ( OR( NOT( COMPARE ) COMPARE ) ) )' ).
  ENDMETHOD.

  METHOD test049.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF ( lv_req_date = 0 OR lv_req_date IS INITIAL ) O' &&
      'R ( lv_req_date NOT BETWEEN lv_date_from AND lv_date_to ).' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'OR( ( OR( COMPARE COMPARE ) ) ( COMPARE ) )' ).
  ENDMETHOD.

  METHOD test050.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF var NOT BETWEEN moo AND foo.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test051.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF var Z foo.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD test052.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF lv_blah IS NOT INSTANCE OF if_sxml_value_node.' ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_parse2 DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.
  PRIVATE SECTION.
    METHODS:
      parse IMPORTING iv_string        TYPE string
            RETURNING VALUE(rv_result) TYPE string,
      compare_ge FOR TESTING,
      compare_le FOR TESTING,
      compare_ne FOR TESTING,
      dereference_tbl_exprssn_cmpnnt FOR TESTING,
      structure_component FOR TESTING,
      double_negation_01 FOR TESTING,
      double_negation_02 FOR TESTING,
      reduction_01 FOR TESTING.
ENDCLASS. "ltcl_parse2 definition

CLASS ltcl_parse2 IMPLEMENTATION.

  METHOD parse.

    DATA: lt_tokens TYPE stokesx_tab,
          lo_node   TYPE REF TO zcl_aoc_boolean_node.


    lt_tokens = lcl_parse=>parse( iv_string )->remove( 1 )->get_tokens( ).

    lo_node = zcl_aoc_boolean=>parse( lt_tokens ).
    cl_abap_unit_assert=>assert_bound( lo_node ).

    rv_result = lo_node->to_string( ).

  ENDMETHOD.

  METHOD compare_ge.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF 2 GE 1.' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).

    lv_result = parse( 'IF 2 >= 1.' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).

    lv_result = parse( 'IF 2 => 1.' ). "Obsolete, but not forbidden outside ABAP Objects
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD compare_le.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF 2 LE 1.' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).

    lv_result = parse( 'IF 2 <= 1.' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).

    lv_result = parse( 'IF 2 =< 1.' ). "Obsolete, but not forbidden outside ABAP Objects
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD compare_ne.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF 2 NE 1.' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).

    lv_result = parse( 'IF 2 <> 1.' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).

    lv_result = parse( 'IF 2 >< 1.' ). "Obsolete, but not forbidden outside ABAP Objects
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD dereference_tbl_exprssn_cmpnnt.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF data_ref->*[ 1 ]-component = value.' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

  METHOD structure_component.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF ( structure-component < 1000 AND structure-component > -1000 ).' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = '( AND( COMPARE COMPARE ) )' ).
  ENDMETHOD.

  METHOD double_negation_01.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF NOT NOT range IS INITIAL.' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'NOT( NOT( COMPARE ) )' ).
  ENDMETHOD.

  METHOD double_negation_02.
    DATA: lv_result TYPE string.

    lv_result = parse( 'IF NOT NOT a < 10 OR b > 10.' ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'OR( NOT( NOT( COMPARE ) ) COMPARE )' ).
  ENDMETHOD.

  METHOD reduction_01.
    DATA lv_code TYPE string.
    DATA lv_result TYPE string.

    lv_code = |IF ( REDUCE i( INIT x = 0 FOR wa IN table|
           && |               WHERE ( flag = 'X' )|
           && |               NEXT x = x + 1 ) )|
           && |   GT integer_value.|.
    lv_result = parse( lv_code ).
    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = 'COMPARE' ).
  ENDMETHOD.

ENDCLASS. "ltcl_parse2 implementation

CLASS ltcl_remove_strings DEFINITION DEFERRED.
CLASS zcl_aoc_boolean DEFINITION LOCAL FRIENDS ltcl_remove_strings.

CLASS ltcl_remove_strings DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      test
        IMPORTING
          iv_code TYPE string
          iv_exp  TYPE string,
      test01 FOR TESTING,
      test02 FOR TESTING,
      test03 FOR TESTING.

ENDCLASS.

CLASS ltcl_remove_strings IMPLEMENTATION.

  METHOD test.

    DATA: lv_result TYPE string,
          lo_tokens TYPE REF TO zcl_aoc_boolean_tokens.


    lo_tokens = lcl_parse=>parse( iv_code ).

    zcl_aoc_boolean=>remove_strings( lo_tokens ).
    cl_abap_unit_assert=>assert_bound( lo_tokens ).

    lv_result = lo_tokens->to_string( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = to_upper( iv_exp ) ).

  ENDMETHOD.

  METHOD test01.

    test( iv_code = 'bar'
          iv_exp  = 'bar' ).

  ENDMETHOD.

  METHOD test02.

    test( iv_code = '''bar'''
          iv_exp  = 'str' ).

  ENDMETHOD.

  METHOD test03.

    test( iv_code = '`bar`'
          iv_exp  = 'str' ).

  ENDMETHOD.

ENDCLASS.


CLASS ltcl_remove_method_calls DEFINITION DEFERRED.
CLASS zcl_aoc_boolean DEFINITION LOCAL FRIENDS ltcl_remove_method_calls.

CLASS ltcl_remove_method_calls DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      test
        IMPORTING iv_code TYPE string
                  iv_exp  TYPE string,
      test01 FOR TESTING,
      test02 FOR TESTING,
      test03 FOR TESTING,
      test04 FOR TESTING,
      test05 FOR TESTING,
      test06 FOR TESTING,
      test07 FOR TESTING,
      test08 FOR TESTING,
      test09 FOR TESTING,
      test10 FOR TESTING,
      test11 FOR TESTING,
      test12 FOR TESTING.

    METHODS:
      reduction_01 FOR TESTING.

ENDCLASS.       "ltcl_Remove_Method_Calls

CLASS ltcl_remove_method_calls IMPLEMENTATION.

  METHOD test.

    DATA: lv_result TYPE string,
          lo_tokens TYPE REF TO zcl_aoc_boolean_tokens.


    lo_tokens = lcl_parse=>parse( iv_code ).

    zcl_aoc_boolean=>remove_method_calls( lo_tokens ).
    cl_abap_unit_assert=>assert_bound( lo_tokens ).

    lv_result = lo_tokens->to_string( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = to_upper( iv_exp ) ).

  ENDMETHOD.

  METHOD test01.

    test( iv_code = 'foo( )'
          iv_exp  = 'method' ).

  ENDMETHOD.

  METHOD test02.

    test( iv_code = 'foo( )->bar( )'
          iv_exp  = 'method' ).

  ENDMETHOD.

  METHOD test03.

    test( iv_code = 'foo( bar( ) )'
          iv_exp  = 'method' ).

  ENDMETHOD.

  METHOD test04.

    test( iv_code = 'bar->method( )'
          iv_exp  = 'method' ).

  ENDMETHOD.

  METHOD test05.

    test( iv_code = 'bar=>method( )'
          iv_exp  = 'method' ).

  ENDMETHOD.

  METHOD test06.

    test( iv_code = 'foo-bar->method( )'
          iv_exp  = 'method' ).

  ENDMETHOD.

  METHOD test07.

    test( iv_code = 'foo( ) = bar( )'
          iv_exp  = 'method = method' ).

  ENDMETHOD.

  METHOD test08.

    test( iv_code = 'NEW zcl_foo( )->bar( )'
          iv_exp  = 'method' ).

  ENDMETHOD.

  METHOD test09.

    test( iv_code = 'CONV integer( lv_date )'
          iv_exp  = 'method' ).

  ENDMETHOD.

  METHOD test10.

    test( iv_code = 'VALUE #( lt_tab[ id = 2 ] OPTIONAL )'
          iv_exp  = 'method' ).

  ENDMETHOD.

  METHOD test11.

    test( iv_code = 'lr_object->/ui2/if_edm_model_entity~get_entity_name( )'
          iv_exp  = 'method' ).

  ENDMETHOD.

  METHOD test12.

    test( iv_code = 'REF DATA( lt_tab )'
          iv_exp  = 'method' ).

  ENDMETHOD.

  METHOD reduction_01.
    test( iv_code = |REDUCE i( INIT x = 0 FOR wa IN table WHERE ( flag = 'X' ) NEXT x = x + 1 )|
          iv_exp  = 'reduction' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_remove_templates DEFINITION DEFERRED.
CLASS zcl_aoc_boolean DEFINITION LOCAL FRIENDS ltcl_remove_templates.

CLASS ltcl_remove_templates DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      test
        IMPORTING
          iv_code TYPE string
          iv_exp  TYPE string,
      test01 FOR TESTING,
      test02 FOR TESTING,
      test03 FOR TESTING,
      test04 FOR TESTING.

ENDCLASS.

CLASS ltcl_remove_templates IMPLEMENTATION.

  METHOD test.

    DATA: lv_result TYPE string,
          lo_tokens TYPE REF TO zcl_aoc_boolean_tokens.


    lo_tokens = lcl_parse=>parse( iv_code ).

    zcl_aoc_boolean=>remove_templates( lo_tokens ).
    cl_abap_unit_assert=>assert_bound( lo_tokens ).

    lv_result = lo_tokens->to_string( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = to_upper( iv_exp ) ).

  ENDMETHOD.

  METHOD test01.

    test( iv_code = 'lv_offset'
          iv_exp  = 'lv_offset' ).

  ENDMETHOD.

  METHOD test02.

    test( iv_code = '|foobar|'
          iv_exp  = 'TEMPLATE' ).

  ENDMETHOD.

  METHOD test03.

    test( iv_code = '|foobar { bar }|'
          iv_exp  = 'TEMPLATE' ).

  ENDMETHOD.

  METHOD test04.

    test( iv_code = '|foobar { moo( |blah| ) }|'
          iv_exp  = 'TEMPLATE' ).

  ENDMETHOD.

ENDCLASS.

CLASS ltcl_remove_calculations DEFINITION DEFERRED.
CLASS zcl_aoc_boolean DEFINITION LOCAL FRIENDS ltcl_remove_calculations.

CLASS ltcl_remove_calculations DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      test
        IMPORTING
          iv_code TYPE string
          iv_exp  TYPE string,
      test01 FOR TESTING,
      test02 FOR TESTING,
      test03 FOR TESTING,
      test04 FOR TESTING,
      test05 FOR TESTING.

ENDCLASS.

CLASS ltcl_remove_calculations IMPLEMENTATION.

  METHOD test.

    DATA: lv_result TYPE string,
          lo_tokens TYPE REF TO zcl_aoc_boolean_tokens.


    lo_tokens = lcl_parse=>parse( iv_code ).

    zcl_aoc_boolean=>remove_calculations( lo_tokens ).
    cl_abap_unit_assert=>assert_bound( lo_tokens ).

    lv_result = lo_tokens->to_string( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = to_upper( iv_exp ) ).

  ENDMETHOD.

  METHOD test01.
    test( iv_code = 'lv_offset + 1'
          iv_exp  = 'lv_offset' ).
  ENDMETHOD.

  METHOD test02.
    test( iv_code = 'lv_offset + 1 - 1'
          iv_exp  = 'lv_offset' ).
  ENDMETHOD.

  METHOD test03.
    test( iv_code = '( lv_offset + 1 )'
          iv_exp  = 'lv_offset' ).
  ENDMETHOD.

  METHOD test04.
    test( iv_code = 'bar * ( moo + 1 )'
          iv_exp  = 'bar' ).
  ENDMETHOD.

  METHOD test05.
    test( iv_code = 'IF SY-DATUM + 1 > + SY-DATUM'
          iv_exp  = 'IF SY-DATUM > + SY-DATUM' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_remove_table_expressions DEFINITION DEFERRED.
CLASS zcl_aoc_boolean DEFINITION LOCAL FRIENDS ltcl_remove_table_expressions.

CLASS ltcl_remove_table_expressions DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      test
        IMPORTING
          iv_code TYPE string
          iv_exp  TYPE string,
      test10 FOR TESTING,
      test11 FOR TESTING,
      test12 FOR TESTING,
      test13 FOR TESTING,
      test20 FOR TESTING,
      test21 FOR TESTING,
      test_dereference FOR TESTING.

ENDCLASS.

CLASS ltcl_remove_table_expressions IMPLEMENTATION.

  METHOD test.

    DATA: lv_result TYPE string,
          lo_tokens TYPE REF TO zcl_aoc_boolean_tokens.


    lo_tokens = lcl_parse=>parse( iv_code ).

    zcl_aoc_boolean=>remove_table_expressions( lo_tokens ).
    cl_abap_unit_assert=>assert_bound( lo_tokens ).

    lv_result = lo_tokens->to_string( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = to_upper( iv_exp ) ).

  ENDMETHOD.

  METHOD test10.
    test( iv_code = 'IF table[ 1 ] = some_value'
          iv_exp  = 'IF TABLE = some_value' ).
  ENDMETHOD.

  METHOD test11.
    test( iv_code = 'IF table[ 1 ]-component = some_value'
          iv_exp  = 'IF TABLE-COMPONENT = some_value' ).
  ENDMETHOD.

  METHOD test12.
    test( iv_code = 'IF table[ 1 ] - 50 < some_value'
          iv_exp  = 'IF TABLE - 50 < some_value' ).
  ENDMETHOD.

  METHOD test13.
    test( iv_code = 'IF table[ 1 ]-component - 50 > some_value'
          iv_exp  = 'IF TABLE-COMPONENT - 50 > some_value' ).
  ENDMETHOD.

  METHOD test20.
    test( iv_code = 'IF subtable[ 1 ][ 1 ] = some_value'
          iv_exp  = 'IF subtable = some_value' ).
  ENDMETHOD.

  METHOD test21.
    test( iv_code = 'IF subtable[ 1 ][ 1 ]-component = some_value'
          iv_exp  = 'IF subtable-component = some_value' ).
  ENDMETHOD.

  METHOD test_dereference.
    test( iv_code = 'IF root_data->*[ 1 ]-chm_nature = if_ehfnd_chm_impl_c=>gc_chm_nature-substance'
          iv_exp  = 'IF root_data->*-chm_nature = if_ehfnd_chm_impl_c=>gc_chm_nature-substance' ).
  ENDMETHOD.

ENDCLASS.

CLASS ltcl_remove_dereferences DEFINITION DEFERRED.
CLASS zcl_aoc_boolean DEFINITION LOCAL FRIENDS ltcl_remove_dereferences.

CLASS ltcl_remove_dereferences DEFINITION FOR TESTING DURATION SHORT RISK LEVEL HARMLESS FINAL.

  PRIVATE SECTION.
    METHODS:
      test
        IMPORTING
          iv_code TYPE string
          iv_exp  TYPE string,
      test01 FOR TESTING.

ENDCLASS.

CLASS ltcl_remove_dereferences IMPLEMENTATION.

  METHOD test.

    DATA: lv_result TYPE string,
          lo_tokens TYPE REF TO zcl_aoc_boolean_tokens.


    lo_tokens = lcl_parse=>parse( iv_code ).

    zcl_aoc_boolean=>remove_dereferences( lo_tokens ).
    cl_abap_unit_assert=>assert_bound( lo_tokens ).

    lv_result = lo_tokens->to_string( ).

    cl_abap_unit_assert=>assert_equals(
      act = lv_result
      exp = to_upper( iv_exp ) ).

  ENDMETHOD.

  METHOD test01.
    test( iv_code = 'IF root_data->*-chm_nature = if_ehfnd_chm_impl_c=>gc_chm_nature-substance'
          iv_exp  = 'IF root_data-chm_nature = if_ehfnd_chm_impl_c=>gc_chm_nature-substance' ).
  ENDMETHOD.

ENDCLASS.
