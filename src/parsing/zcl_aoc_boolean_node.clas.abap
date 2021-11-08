CLASS zcl_aoc_boolean_node DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      ty_children_tt TYPE STANDARD TABLE OF REF TO zcl_aoc_boolean_node WITH DEFAULT KEY .
    TYPES:
      ty_type        TYPE c LENGTH 1 .

    CONSTANTS:
      BEGIN OF c_type,
        paren   TYPE ty_type VALUE '1',
        compare TYPE ty_type VALUE '2',
        and     TYPE ty_type VALUE '3',
        or      TYPE ty_type VALUE '4',
        not     TYPE ty_type VALUE '5',
      END OF c_type .

    METHODS append_child
      IMPORTING
        !io_child TYPE REF TO zcl_aoc_boolean_node .
    METHODS constructor
      IMPORTING
        !iv_type TYPE ty_type .
    METHODS get_child
      RETURNING
        VALUE(ro_child) TYPE REF TO zcl_aoc_boolean_node .
    METHODS get_children
      RETURNING
        VALUE(rt_children) TYPE ty_children_tt .
    METHODS get_type
      RETURNING
        VALUE(rv_type) TYPE ty_type .
    METHODS prepend_child
      IMPORTING
        !io_child TYPE REF TO zcl_aoc_boolean_node .
    METHODS to_string
      RETURNING
        VALUE(rv_string) TYPE string .
  PROTECTED SECTION.

    DATA mt_children TYPE ty_children_tt .
    DATA mv_type TYPE ty_type .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_BOOLEAN_NODE IMPLEMENTATION.


  METHOD append_child.
    ASSERT io_child IS BOUND.
    APPEND io_child TO mt_children.
  ENDMETHOD.


  METHOD constructor.
    ASSERT NOT iv_type IS INITIAL.
    mv_type = iv_type.
  ENDMETHOD.


  METHOD get_child.

    ASSERT lines( mt_children ) = 1.

    READ TABLE mt_children INDEX 1 INTO ro_child.         "#EC CI_SUBRC

  ENDMETHOD.


  METHOD get_children.
    rt_children = mt_children.
  ENDMETHOD.


  METHOD get_type.
    rv_type = mv_type.
  ENDMETHOD.


  METHOD prepend_child.
    ASSERT io_child IS BOUND.
    INSERT io_child INTO mt_children INDEX 1.
  ENDMETHOD.


  METHOD to_string.

    DATA: lv_pre   TYPE c LENGTH 3,
          lo_child TYPE REF TO zcl_aoc_boolean_node,
          lt_child TYPE string_table,
          lv_child TYPE string.


    CASE mv_type.
      WHEN c_type-compare.
        rv_string = 'COMPARE'.
        RETURN.
      WHEN c_type-paren.
        lv_pre = ''.
        ASSERT lines( mt_children ) = 1.
      WHEN c_type-and.
        lv_pre = 'AND'.
        ASSERT lines( mt_children ) = 2.
      WHEN c_type-or.
        lv_pre = 'OR'.
        ASSERT lines( mt_children ) = 2.
      WHEN c_type-not.
        lv_pre = 'NOT'.
        ASSERT lines( mt_children ) = 1.
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

    LOOP AT mt_children INTO lo_child.
      APPEND lo_child->to_string( ) TO lt_child.
    ENDLOOP.
    CONCATENATE LINES OF lt_child INTO lv_child SEPARATED BY space.

    rv_string = |{ lv_pre }( { lv_child } )|.
    CONDENSE rv_string.

  ENDMETHOD.
ENDCLASS.
