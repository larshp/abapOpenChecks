CLASS zcl_aoc_check_59 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
    METHODS get_attributes
        REDEFINITION .
    METHODS if_ci_test~query_attributes
        REDEFINITION .
    METHODS put_attributes
        REDEFINITION .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_counts,
        level TYPE i,
        paren TYPE i,
        and   TYPE i,
        or    TYPE i,
        not   TYPE i,
      END OF ty_counts .
    TYPES:
      ty_counts_tt TYPE STANDARD TABLE OF ty_counts WITH DEFAULT KEY .

    DATA mv_parser_errors TYPE flag .

    METHODS walk
      IMPORTING
        !io_node         TYPE REF TO zcl_aoc_boolean_node
        !iv_level        TYPE i DEFAULT 0
      RETURNING
        VALUE(rt_counts) TYPE ty_counts_tt .
    METHODS analyze
      IMPORTING
        !it_tokens     TYPE stokesx_tab
      RETURNING
        VALUE(rv_code) TYPE sci_errc .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_59 IMPLEMENTATION.


  METHOD analyze.

    DATA: lt_tokens LIKE it_tokens,
          lt_counts TYPE ty_counts_tt,
          ls_count  LIKE LINE OF lt_counts,
          lo_node   TYPE REF TO zcl_aoc_boolean_node.

    FIELD-SYMBOLS <ls_token> LIKE LINE OF it_tokens.


    READ TABLE it_tokens INDEX 1 ASSIGNING <ls_token>.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CASE <ls_token>-str.
      WHEN 'IF' OR 'ELSEIF' OR 'WHILE' OR 'CHECK'.
* nothing
        CLEAR lt_tokens.
      WHEN 'ASSERT'.
        READ TABLE it_tokens INDEX 2 ASSIGNING <ls_token>.
        ASSERT sy-subrc = 0.
        IF <ls_token>-str = 'FIELDS'.
          RETURN.
        ENDIF.
      WHEN OTHERS.
        RETURN.
    ENDCASE.

    lt_tokens = it_tokens.
    DELETE lt_tokens INDEX 1.

    lo_node = zcl_aoc_boolean=>parse( lt_tokens ).
    IF lo_node IS INITIAL.
      IF mv_parser_errors = abap_true.
        rv_code = '001'.
      ENDIF.
      RETURN.
    ENDIF.

    IF lo_node->get_type( ) = zcl_aoc_boolean_node=>c_type-paren.
      rv_code = '002'.
      RETURN.
    ENDIF.

    lt_counts = walk( lo_node ).

    LOOP AT lt_counts INTO ls_count.
      IF ls_count-paren > 0 AND ls_count-and >= 0 AND ls_count-or = 0 AND ls_count-not = 0.
        rv_code = '002'.
        RETURN.
      ELSEIF ls_count-paren > 0 AND ls_count-and = 0 AND ls_count-or > 0 AND ls_count-not = 0.
        rv_code = '002'.
        RETURN.
      ELSEIF ls_count-paren = 0 AND ls_count-and > 0 AND ls_count-or > 0 AND ls_count-not = 0.
        rv_code = '003'.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_tokens  LIKE io_scan->tokens,
          lv_code    TYPE sci_errc,
          lv_include TYPE sobj_name.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF io_scan->statements,
                   <ls_token>     LIKE LINE OF io_scan->tokens.


    LOOP AT io_scan->statements ASSIGNING <ls_statement>
        WHERE type = io_scan->gc_statement-standard.

      CLEAR lt_tokens.

      LOOP AT io_scan->tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from TO <ls_statement>-to.
        APPEND <ls_token> TO lt_tokens.
      ENDLOOP.

      lv_code = analyze( lt_tokens ).

      IF NOT lv_code IS INITIAL AND <ls_token>-row > 0.
        lv_include = io_scan->get_include( <ls_statement>-level ).
        inform( p_sub_obj_name = lv_include
                p_kind         = mv_errty
                p_line         = <ls_token>-row
                p_test         = myname
                p_code         = lv_code ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '059'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    mv_parser_errors = abap_true.

    insert_scimessage(
      iv_code = '001'
      iv_text = 'abapOpenChecks boolean parser error'(m01) ).

    insert_scimessage(
      iv_code = '002'
      iv_text = 'Superfluous parentheses'(m02) ).

    insert_scimessage(
      iv_code = '003'
      iv_text = 'Too few parentheses'(m03) ).

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mv_parser_errors = mv_parser_errors
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_parser_errors 'Show parser errors' ''. "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mv_parser_errors = mv_parser_errors
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD walk.

    DATA: lt_children_node TYPE zcl_aoc_boolean_node=>ty_children_tt,
          lo_child         TYPE REF TO zcl_aoc_boolean_node,
          lt_children      TYPE ty_counts_tt,
          ls_child         LIKE LINE OF lt_children.

    FIELD-SYMBOLS: <ls_count> LIKE LINE OF rt_counts.


    IF io_node->get_type( ) = zcl_aoc_boolean_node=>c_type-compare.
      RETURN.
    ELSEIF io_node->get_type( ) = zcl_aoc_boolean_node=>c_type-paren.
      rt_counts = walk( io_node  = io_node->get_child( )
                        iv_level = iv_level ).
      RETURN.
    ENDIF.

    APPEND INITIAL LINE TO rt_counts ASSIGNING <ls_count>.
    <ls_count>-level = iv_level.

    CASE io_node->get_type( ).
      WHEN zcl_aoc_boolean_node=>c_type-and.
        <ls_count>-and = 1.
      WHEN zcl_aoc_boolean_node=>c_type-or.
        <ls_count>-or = 1.
      WHEN zcl_aoc_boolean_node=>c_type-not.
        <ls_count>-not = 1.
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

    lt_children_node = io_node->get_children( ).

    LOOP AT lt_children_node INTO lo_child.
      IF lo_child->get_type( ) = zcl_aoc_boolean_node=>c_type-paren.
        <ls_count>-paren = <ls_count>-paren + 1.
      ENDIF.

      lt_children = walk( io_node  = lo_child
                          iv_level = iv_level + 1 ).
      APPEND LINES OF lt_children TO rt_counts.

      LOOP AT lt_children INTO ls_child WHERE level = iv_level + 1.
        <ls_count>-paren = <ls_count>-paren + ls_child-paren.
        <ls_count>-and   = <ls_count>-and   + ls_child-and.
        <ls_count>-or    = <ls_count>-or    + ls_child-or.
        <ls_count>-not   = <ls_count>-not   + ls_child-not.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
