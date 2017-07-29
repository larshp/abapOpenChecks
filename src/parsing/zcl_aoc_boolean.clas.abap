class ZCL_AOC_BOOLEAN definition
  public
  create public .

public section.

  class-methods PARSE
    importing
      !IT_TOKENS type STOKESX_TAB
    returning
      value(RO_NODE) type ref to ZCL_AOC_BOOLEAN_NODE .
protected section.

  class-methods IS_COMPARATOR
    importing
      !IV_STR type STRING
    returning
      value(RV_COMPARATOR) type ABAP_BOOL .
  class-methods PARSE_INTERNAL
    importing
      !IO_TOKENS type ref to ZCL_AOC_BOOLEAN_TOKENS
    returning
      value(RO_NODE) type ref to ZCL_AOC_BOOLEAN_NODE .
  class-methods PARSE_NOT
    importing
      !IO_TOKENS type ref to ZCL_AOC_BOOLEAN_TOKENS
    returning
      value(RO_NODE) type ref to ZCL_AOC_BOOLEAN_NODE .
  class-methods PARSE_PAREN
    importing
      !IO_TOKENS type ref to ZCL_AOC_BOOLEAN_TOKENS
    returning
      value(RO_NODE) type ref to ZCL_AOC_BOOLEAN_NODE .
  class-methods REMOVE_METHOD_CALLS
    importing
      !IO_TOKENS type ref to ZCL_AOC_BOOLEAN_TOKENS .
  class-methods SIMPLIFY
    importing
      !IT_TOKENS type STOKESX_TAB
    returning
      value(RO_TOKENS) type ref to ZCL_AOC_BOOLEAN_TOKENS .
  class-methods REMOVE_STRINGS
    importing
      !IO_TOKENS type ref to ZCL_AOC_BOOLEAN_TOKENS .
private section.
ENDCLASS.



CLASS ZCL_AOC_BOOLEAN IMPLEMENTATION.


  METHOD is_comparator.

    rv_comparator = abap_false.

    IF iv_str = '='
        OR iv_str = '<>'
        OR iv_str = 'NE'
        OR iv_str = 'EQ'.
* todo, there is a lot more
      rv_comparator = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD parse.

    DATA: lo_tokens TYPE REF TO zcl_aoc_boolean_tokens.


lo_tokens = simplify( it_tokens ).

    ro_node = parse_internal( lo_tokens ).

  ENDMETHOD.


  METHOD parse_internal.

    DATA: lv_token1 TYPE string,
          lv_token2 TYPE string,
          lv_token3 TYPE string,
          lo_node   LIKE ro_node,
          lo_split  LIKE io_tokens.


    lv_token1 = io_tokens->get_token( 1 )-str.
    lv_token2 = io_tokens->get_token( 2 )-str.
    lv_token3 = io_tokens->get_token( 3 )-str.

    IF is_comparator( lv_token2 ) AND io_tokens->get_length( ) >= 3.
      CREATE OBJECT ro_node
        EXPORTING
          iv_type = zcl_aoc_boolean_node=>c_type-compare.

      io_tokens->eat( 3 ).

* parse remaining
      IF io_tokens->get_length( ) > 0.
        lo_node = parse_internal( io_tokens ).
        lo_node->prepend_child( ro_node ).
        ro_node = lo_node.
      ENDIF.
    ELSEIF lv_token1 = 'NOT'.
      io_tokens->eat( 1 ).
      ro_node = parse_not( io_tokens ).

* parse remaining
      IF io_tokens->get_length( ) > 0.
        lo_node = parse_internal( io_tokens ).
        lo_node->prepend_child( ro_node ).
        ro_node = lo_node.
      ENDIF.
    ELSEIF lv_token1 = '('.
      ro_node = parse_paren( io_tokens ).

* parse remaining
      IF io_tokens->get_length( ) > 0.
        lo_node = parse_internal( io_tokens ).
        lo_node->prepend_child( ro_node ).
        ro_node = lo_node.
      ENDIF.
    ELSEIF lv_token1 = 'AND'.
      CREATE OBJECT ro_node
        EXPORTING
          iv_type = zcl_aoc_boolean_node=>c_type-and.
      io_tokens->eat( 1 ).
      lo_node = parse_internal( io_tokens ).
      ro_node->append_child( lo_node ).
    ELSEIF lv_token1 = 'OR'.
* zcl_aoc_boolean_node=>c_type-or
      BREAK-POINT.
    ELSE.
* todo: parser error?
      BREAK-POINT.
    ENDIF.

  ENDMETHOD.


  METHOD parse_not.

    DATA: lo_node  TYPE REF TO zcl_aoc_boolean_node,
          lv_end   TYPE i,
          lo_split TYPE REF TO zcl_aoc_boolean_tokens.


    CREATE OBJECT ro_node
      EXPORTING
        iv_type = zcl_aoc_boolean_node=>c_type-not.

    IF io_tokens->get_token( 1 )-str = '('.
      lv_end = io_tokens->find_end_paren( 1 ).
      lo_split = io_tokens->eat( lv_end ).
    ELSE.
      lo_split = io_tokens->eat( 3 ).
    ENDIF.

    lo_node = parse_internal( lo_split ).
    ro_node->append_child( lo_node ).

  ENDMETHOD.


  METHOD parse_paren.

    DATA: lv_token1 TYPE string,
          lo_node   TYPE REF TO zcl_aoc_boolean_node,
          lv_end    TYPE i,
          lo_split  TYPE REF TO zcl_aoc_boolean_tokens.


    ASSERT io_tokens->get_token( 1 )-str = '('.

    CREATE OBJECT ro_node
      EXPORTING
        iv_type = zcl_aoc_boolean_node=>c_type-paren.

    lv_end = io_tokens->find_end_paren( 1 ).
    lo_split = io_tokens->eat( lv_end ).

* remove start and end paren
    lo_split = lo_split->split(
      iv_start = 1
      iv_end   = lo_split->get_length( ) - 1 ).

    lo_node = parse_internal( lo_split ).
    ro_node->append_child( lo_node ).

  ENDMETHOD.


  METHOD remove_method_calls.

    DATA: ls_token   TYPE stokesx,
          lv_end     TYPE i,
          lv_restart TYPE abap_bool,
          lv_index   TYPE i.


    DO.
      lv_restart = abap_false.
      LOOP AT io_tokens->get_tokens( ) INTO ls_token.
        lv_index = sy-tabix.

        FIND REGEX '^[\w>\-=]+\($' IN ls_token-str.
        IF sy-subrc = 0.
          lv_end = io_tokens->find_end_paren( lv_index ).

          io_tokens->replace(
            iv_str   = 'METHOD'
            iv_start = lv_index
            iv_end   = lv_end ).

          lv_restart = abap_true.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lv_restart = abap_false.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD remove_strings.

    DATA: ls_token TYPE stokesx.


    LOOP AT io_tokens->get_tokens( ) INTO ls_token WHERE type = scan_token_type-literal.
      io_tokens->replace(
        iv_str   = 'str'
        iv_start = sy-tabix ).
    ENDLOOP.

  ENDMETHOD.


  METHOD simplify.

* todo: string templates?

    CREATE OBJECT ro_tokens EXPORTING it_tokens = it_tokens.

    remove_strings( ro_tokens ).
    remove_method_calls( ro_tokens ).

  ENDMETHOD.
ENDCLASS.
