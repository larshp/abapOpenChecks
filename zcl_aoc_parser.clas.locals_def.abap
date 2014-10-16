*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

TYPES: syntax_tt TYPE STANDARD TABLE OF ssyntaxstructure.

TYPES: BEGIN OF st_return,
         match TYPE abap_bool,
         index TYPE i,
       END OF st_return.

TYPES: ty_node_type TYPE c LENGTH 1.

CONSTANTS: c_dummy       TYPE ty_node_type VALUE 'D',
           c_start       TYPE ty_node_type VALUE 'S',
           c_end         TYPE ty_node_type VALUE 'E',
           c_role        TYPE ty_node_type VALUE 'R',
           c_terminal    TYPE ty_node_type VALUE 'T',
           c_nonterminal TYPE ty_node_type VALUE 'N'.

*----------------------------------------------------------------------*
*       CLASS lcl_node DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_node DEFINITION.

  PUBLIC SECTION.
    DATA: mv_type  TYPE ty_node_type READ-ONLY,
          mv_value TYPE string READ-ONLY,
          mv_key   TYPE i READ-ONLY,
          mt_edges TYPE TABLE OF REF TO lcl_node READ-ONLY.

    METHODS: constructor IMPORTING iv_type TYPE ty_node_type
                                   iv_value TYPE string,
             edge IMPORTING io_node TYPE REF TO lcl_node.

  PRIVATE SECTION.
    CLASS-DATA: sv_key TYPE i VALUE 1.

ENDCLASS.                    "lcl_node DEFINITIONd

TYPES: BEGIN OF st_graph,
         rulename TYPE string,
         start TYPE REF TO lcl_node,
       END OF st_graph.

TYPES: tt_graph TYPE STANDARD TABLE OF st_graph.