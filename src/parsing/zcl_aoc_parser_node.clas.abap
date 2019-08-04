CLASS zcl_aoc_parser_node DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: ty_node_type TYPE c LENGTH 1.

    DATA: mv_type     TYPE ty_node_type READ-ONLY,
          mv_value    TYPE string READ-ONLY,
          mv_key      TYPE i READ-ONLY,
          mv_rulename TYPE string READ-ONLY,
          mt_edges    TYPE TABLE OF REF TO zcl_aoc_parser_node READ-ONLY.

    METHODS:
      constructor
        IMPORTING
          iv_type     TYPE ty_node_type
          iv_value    TYPE string
          iv_rulename TYPE string,
      edge
        IMPORTING
          io_node TYPE REF TO zcl_aoc_parser_node.

  PRIVATE SECTION.
    CLASS-DATA: gv_key TYPE i VALUE 1.

ENDCLASS.



CLASS ZCL_AOC_PARSER_NODE IMPLEMENTATION.


  METHOD constructor.
    ASSERT NOT iv_value IS INITIAL.

    mv_type  = iv_type.
    mv_value = iv_value.
    mv_key   = gv_key.
    mv_rulename = iv_rulename.

    gv_key   = gv_key + 1.
  ENDMETHOD.                    "constructor


  METHOD edge.
    READ TABLE mt_edges FROM io_node TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      APPEND io_node TO mt_edges.
    ENDIF.
  ENDMETHOD.                    "edge
ENDCLASS.
