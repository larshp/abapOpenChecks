*"* use this source file for any type of declarations (class
*"* definitions, interfaces or type declarations) you need for
*"* components in the private section

TYPES: syntax_tt TYPE STANDARD TABLE OF ssyntaxstructure.

TYPES: ty_node_type TYPE c LENGTH 1.

CONSTANTS: gc_dummy       TYPE ty_node_type VALUE 'D',
           gc_start       TYPE ty_node_type VALUE 'S',
           gc_end         TYPE ty_node_type VALUE 'E',
           gc_role        TYPE ty_node_type VALUE 'R',
           gc_terminal    TYPE ty_node_type VALUE 'T',
           gc_nonterminal TYPE ty_node_type VALUE 'N'.

*----------------------------------------------------------------------*
*       CLASS lcl_node DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_node DEFINITION FINAL.

  PUBLIC SECTION.
    DATA: mv_type  TYPE ty_node_type READ-ONLY,
          mv_value TYPE string READ-ONLY,
          mv_key   TYPE i READ-ONLY,
          mt_edges TYPE TABLE OF REF TO lcl_node READ-ONLY.

    METHODS: constructor IMPORTING iv_type TYPE ty_node_type
                                   iv_value TYPE string,
             edge IMPORTING io_node TYPE REF TO lcl_node.

  PRIVATE SECTION.
    CLASS-DATA: gv_key TYPE i VALUE 1.

ENDCLASS.                    "lcl_node DEFINITIONd