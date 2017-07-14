class ZCL_AOC_BOOLEAN_NODE definition
  public
  create public .

public section.

  types:
    ty_children_tt TYPE STANDARD TABLE OF REF TO zcl_aoc_boolean_node WITH DEFAULT KEY .
  types:
    ty_type        TYPE c LENGTH 1 .

  constants:
    BEGIN OF c_type,
        paren   TYPE ty_type VALUE '1',
        compare TYPE ty_type VALUE '2',
        and     TYPE ty_type VALUE '3',
        or      TYPE ty_type VALUE '4',
        not     TYPE ty_type VALUE '5',
      END OF c_type .

  methods ADD_CHILD
    importing
      !IO_CHILD type ref to ZCL_AOC_BOOLEAN_NODE .
  methods CONSTRUCTOR
    importing
      !IV_TYPE type TY_TYPE .
  methods GET_CHILDREN
    returning
      value(RT_CHILDREN) type TY_CHILDREN_TT .
  methods GET_TYPE
    returning
      value(RV_TYPE) type TY_TYPE .
  methods TO_STRING
    returning
      value(RV_STRING) type STRING .
protected section.

  data MT_CHILDREN type TY_CHILDREN_TT .
  data MV_TYPE type TY_TYPE .
private section.
ENDCLASS.



CLASS ZCL_AOC_BOOLEAN_NODE IMPLEMENTATION.


  METHOD add_child.
    APPEND io_child TO mt_children.
  ENDMETHOD.


  METHOD constructor.
    ASSERT NOT iv_type IS INITIAL.
    mv_type  = iv_type.
  ENDMETHOD.


  METHOD get_children.
    rt_children = mt_children.
  ENDMETHOD.


  METHOD get_type.
    rv_type = mv_type.
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
