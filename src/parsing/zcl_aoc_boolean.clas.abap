CLASS zcl_aoc_boolean DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS parse
      IMPORTING
        !it_tokens     TYPE stokesx_tab
      RETURNING
        VALUE(ro_node) TYPE REF TO zcl_aoc_boolean_node .
  PROTECTED SECTION.

    CLASS-METHODS is_comparator
      IMPORTING
        !io_tokens           TYPE REF TO zcl_aoc_boolean_tokens
      RETURNING
        VALUE(rv_comparator) TYPE i .
    CLASS-METHODS parse_internal
      IMPORTING
        !io_tokens     TYPE REF TO zcl_aoc_boolean_tokens
      RETURNING
        VALUE(ro_node) TYPE REF TO zcl_aoc_boolean_node .
    CLASS-METHODS parse_not
      IMPORTING
        !io_tokens     TYPE REF TO zcl_aoc_boolean_tokens
      RETURNING
        VALUE(ro_node) TYPE REF TO zcl_aoc_boolean_node .
    CLASS-METHODS parse_paren
      IMPORTING
        !io_tokens     TYPE REF TO zcl_aoc_boolean_tokens
      RETURNING
        VALUE(ro_node) TYPE REF TO zcl_aoc_boolean_node .
    CLASS-METHODS remove_method_calls
      IMPORTING
        !io_tokens TYPE REF TO zcl_aoc_boolean_tokens .
    CLASS-METHODS remove_templates
      IMPORTING
        !io_tokens TYPE REF TO zcl_aoc_boolean_tokens .
    CLASS-METHODS simplify
      IMPORTING
        !it_tokens       TYPE stokesx_tab
      RETURNING
        VALUE(ro_tokens) TYPE REF TO zcl_aoc_boolean_tokens .
    CLASS-METHODS remove_calculations
      IMPORTING
        !io_tokens TYPE REF TO zcl_aoc_boolean_tokens .
    CLASS-METHODS remove_strings
      IMPORTING
        !io_tokens TYPE REF TO zcl_aoc_boolean_tokens .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_BOOLEAN IMPLEMENTATION.


  METHOD is_comparator.

    DATA: lv_token2 TYPE string,
          lv_token3 TYPE string,
          lv_token4 TYPE string.


    lv_token2 = io_tokens->get_token( 2 )-str.
    lv_token3 = io_tokens->get_token( 3 )-str.
    lv_token4 = io_tokens->get_token( 4 )-str.

    rv_comparator = 0.

    IF ( lv_token2 = 'IS' AND lv_token3 = 'NOT' )
        OR ( lv_token2 = 'NOT' AND lv_token3 = 'IN' ).
      rv_comparator = 2.
    ELSEIF ( lv_token2 = 'IS' AND lv_token3 = 'INSTANCE' AND lv_token4 = 'OF' )
        OR lv_token2 = 'BETWEEN'.
      rv_comparator = 3.
    ELSEIF lv_token2 = 'NOT' AND lv_token3 = 'BETWEEN'.
      rv_comparator = 4.
    ELSEIF lv_token2 = '='
            OR lv_token2 = 'O'
            OR lv_token2 = 'Z'
            OR lv_token2 = 'M'
            OR lv_token2 = '<>'
            OR lv_token2 = '<'
            OR lv_token2 = 'GT'
            OR lv_token2 = '>'
            OR lv_token2 = 'LT'
            OR lv_token2 = '>='
            OR lv_token2 = 'GE'
            OR lv_token2 = 'NS'
            OR lv_token2 = '<='
            OR lv_token2 = 'LE'
            OR lv_token2 = 'NE'
            OR lv_token2 = 'NA'
            OR lv_token2 = 'CO'
            OR lv_token2 = 'CA'
            OR lv_token2 = 'CS'
            OR lv_token2 = 'CN'
            OR lv_token2 = 'IN'
            OR lv_token2 = 'CP'
            OR lv_token2 = 'NP'
            OR lv_token2 = 'IS'
            OR lv_token2 = 'EQ'.
      rv_comparator = 1.
    ENDIF.

  ENDMETHOD.


  METHOD parse.
* returns initial RO_NODE in case of parser errors

    DATA: lo_tokens TYPE REF TO zcl_aoc_boolean_tokens.


    lo_tokens = simplify( it_tokens ).

    ro_node = parse_internal( lo_tokens ).

  ENDMETHOD.


  METHOD parse_internal.

    DATA: lv_token1     TYPE string,
          lv_token2     TYPE string,
          lo_node       LIKE ro_node,
          lv_comparator TYPE i.


    lv_token1 = io_tokens->get_token( 1 )-str.
    lv_token2 = io_tokens->get_token( 2 )-str.

    lv_comparator = is_comparator( io_tokens ).

    IF lv_comparator > 0 AND io_tokens->get_length( ) >= 3.
      CREATE OBJECT ro_node
        EXPORTING
          iv_type = zcl_aoc_boolean_node=>c_type-compare.
      io_tokens->eat( 2 + lv_comparator ).
    ELSEIF io_tokens->get_length( ) = 1 OR lv_token2 = 'AND' OR lv_token2 = 'OR'.
* Predicative method call, 740SP08 logical expression
      CREATE OBJECT ro_node
        EXPORTING
          iv_type = zcl_aoc_boolean_node=>c_type-compare.
      io_tokens->eat( 1 ).
    ELSEIF lv_token1 = 'NOT'.
      io_tokens->eat( 1 ).
      ro_node = parse_not( io_tokens ).
    ELSEIF lv_token1 = '('.
      ro_node = parse_paren( io_tokens ).
    ELSEIF lv_token1 = 'AND'.
      CREATE OBJECT ro_node
        EXPORTING
          iv_type = zcl_aoc_boolean_node=>c_type-and.
      io_tokens->eat( 1 ).
      lo_node = parse_internal( io_tokens ).
      ro_node->append_child( lo_node ).
    ELSEIF lv_token1 = 'OR'.
      CREATE OBJECT ro_node
        EXPORTING
          iv_type = zcl_aoc_boolean_node=>c_type-or.
      io_tokens->eat( 1 ).
      lo_node = parse_internal( io_tokens ).
      ro_node->append_child( lo_node ).
    ELSE.
* parser error
      RETURN.
    ENDIF.

* parse remaining
    IF io_tokens->get_length( ) > 0.
      lo_node = parse_internal( io_tokens ).
      IF lo_node IS INITIAL.
* parser error
        CLEAR ro_node.
        RETURN.
      ENDIF.
      lo_node->prepend_child( ro_node ).
      ro_node = lo_node.
    ENDIF.

  ENDMETHOD.


  METHOD parse_not.

    DATA: lo_node   TYPE REF TO zcl_aoc_boolean_node,
          lv_end    TYPE i,
          lv_token1 TYPE string,
          lv_token2 TYPE string,
          lo_split  TYPE REF TO zcl_aoc_boolean_tokens.


    lv_token1 = io_tokens->get_token( 1 )-str.
    lv_token2 = io_tokens->get_token( 2 )-str.

    CREATE OBJECT ro_node
      EXPORTING
        iv_type = zcl_aoc_boolean_node=>c_type-not.

    IF lv_token1 = '('.
      lv_end = io_tokens->find_end_paren( 1 ).
      lo_split = io_tokens->eat( lv_end ).
    ELSEIF lv_token2 = 'OR' OR lv_token2 = 'AND'.
* Predicative method call
      lo_split = io_tokens->eat( 1 ).
    ELSE.
      lo_split = io_tokens->eat( 3 ).
    ENDIF.

    lo_node = parse_internal( lo_split ).
    ro_node->append_child( lo_node ).

  ENDMETHOD.


  METHOD parse_paren.

    DATA: lo_node  TYPE REF TO zcl_aoc_boolean_node,
          lv_end   TYPE i,
          lo_split TYPE REF TO zcl_aoc_boolean_tokens.


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


  METHOD remove_calculations.

    DATA: lv_before TYPE i,
          lt_tokens TYPE stokesx_tab,
          ls_token  LIKE LINE OF lt_tokens,
          ls_prev   LIKE LINE OF lt_tokens,
          ls_next   LIKE LINE OF lt_tokens,
          lv_index  TYPE i.


    lt_tokens = io_tokens->get_tokens( ).

    DO.
      lv_before = lines( lt_tokens ).

      LOOP AT lt_tokens INTO ls_token.
        lv_index = sy-tabix.

        CLEAR: ls_prev, ls_next.
        READ TABLE lt_tokens INDEX lv_index - 1 INTO ls_prev. "#EC CI_SUBRC
        READ TABLE lt_tokens INDEX lv_index + 1 INTO ls_next. "#EC CI_SUBRC

        CASE ls_token-str.
          WHEN '+' OR '-' OR '*' OR '/' OR 'MOD' OR 'DIV' OR 'BIT-AND' OR 'BIT-OR' OR '&&'.
            IF ls_next-str <> '('
                AND ls_prev-str <> '='
                AND ls_prev-str <> '<>'
                AND ls_prev-str <> '<'
                AND ls_prev-str <> 'GT'
                AND ls_prev-str <> '>'
                AND ls_prev-str <> 'LT'
                AND ls_prev-str <> '>='
                AND ls_prev-str <> 'GE'
                AND ls_prev-str <> 'NS'
                AND ls_prev-str <> '<='
                AND ls_prev-str <> 'LE'
                AND ls_prev-str <> 'NE'
                AND ls_prev-str <> 'NA'
                AND ls_prev-str <> 'CO'
                AND ls_prev-str <> 'CA'
                AND ls_prev-str <> 'CS'
                AND ls_prev-str <> 'CN'
                AND ls_prev-str <> 'IN'
                AND ls_prev-str <> 'CP'
                AND ls_prev-str <> 'NP'
                AND ls_prev-str <> 'IS'
                AND ls_prev-str <> 'EQ'.
              DO 2 TIMES.
                DELETE lt_tokens INDEX lv_index.
              ENDDO.
              EXIT.
            ENDIF.
        ENDCASE.

* remove paren introduced by calculations
        IF ls_prev-str = '(' AND ls_next-str = ')'.
          DELETE lt_tokens INDEX lv_index + 1.
          DELETE lt_tokens INDEX lv_index - 1.
          EXIT.
        ELSEIF ls_prev-str = '|' AND ls_next-str = '|'.
* quick workaround for string templates, todo
          DELETE lt_tokens INDEX lv_index + 1.
          DELETE lt_tokens INDEX lv_index - 1.
          EXIT.
        ENDIF.
      ENDLOOP.

      IF lines( lt_tokens ) = lv_before.
        EXIT.
      ENDIF.
    ENDDO.

    io_tokens->set_tokens( lt_tokens ).

  ENDMETHOD.


  METHOD remove_method_calls.

    DATA: lt_tokens  TYPE stokesx_tab,
          ls_token   TYPE stokesx,
          lv_end     TYPE i,
          lv_restart TYPE abap_bool,
          lv_index   TYPE i.

    lt_tokens = io_tokens->get_tokens( ).

    DO.
      lv_restart = abap_false.
      LOOP AT lt_tokens INTO ls_token.
        lv_index = sy-tabix.

        FIND REGEX '^[\w<>\/~\-=#]+\($' IN ls_token-str.
        IF sy-subrc = 0.
          lv_end = io_tokens->find_end_paren( lv_index ).

          CASE io_tokens->get_token( lv_index - 1 )-str.
            WHEN 'NEW' OR 'CONV' OR 'COND' OR 'VALUE' OR 'REF'.
              lv_index = lv_index - 1.
          ENDCASE.

          io_tokens->replace(
            iv_str   = 'METHOD'
            iv_start = lv_index
            iv_end   = lv_end ).

          lv_restart = abap_true.
          EXIT.
        ENDIF.

* table comprehensions
        FIND REGEX '^[\w<>~\-=]+\[$' IN ls_token-str.
        IF sy-subrc = 0.
          lv_end = io_tokens->find_end_square( lv_index ).

          io_tokens->replace(
            iv_str   = 'TABLEC'
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

    DATA:
      lt_tokens TYPE stokesx_tab.

    lt_tokens = io_tokens->get_tokens( ).

    LOOP AT lt_tokens TRANSPORTING NO FIELDS WHERE type = zcl_aoc_scan=>gc_token-literal.
      io_tokens->replace(
        iv_str   = 'str'
        iv_start = sy-tabix ).
    ENDLOOP.

  ENDMETHOD.


  METHOD remove_templates.

    DATA: lv_start   TYPE i,
          lv_escaped TYPE i,
          lv_index   TYPE i,
          lt_tokens  TYPE stokesx_tab.

    FIELD-SYMBOLS: <ls_token> LIKE LINE OF lt_tokens.


    lt_tokens = io_tokens->get_tokens( ).

    LOOP AT lt_tokens ASSIGNING <ls_token>.
      lv_index = sy-tabix.

      IF <ls_token>-str = '|' AND lv_start IS INITIAL.
        <ls_token>-str = 'TEMPLATE'.
        lv_start = lv_index.
      ELSEIF <ls_token>-str = '{' AND NOT lv_start IS INITIAL.
        lv_escaped = lv_escaped + 1.
      ELSEIF <ls_token>-str = '}' AND NOT lv_start IS INITIAL.
        lv_escaped = lv_escaped - 1.
      ELSEIF <ls_token>-str = '|' AND lv_escaped = 0.
        DELETE lt_tokens FROM lv_start + 1 TO lv_index.
      ENDIF.
    ENDLOOP.

    io_tokens->set_tokens( lt_tokens ).

  ENDMETHOD.


  METHOD simplify.

* todo: string templates?
* todo: change identifiers, so no keywords are possible

    CREATE OBJECT ro_tokens EXPORTING it_tokens = it_tokens.

    remove_templates( ro_tokens ).
    remove_strings( ro_tokens ).
    remove_method_calls( ro_tokens ).
    remove_calculations( ro_tokens ).

  ENDMETHOD.
ENDCLASS.
