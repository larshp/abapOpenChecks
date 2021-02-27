CLASS zcl_aoc_check_98 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_AOC_CHECK_98 IMPLEMENTATION.


  METHOD check.
* Test for Github CI checks
  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version  = '001'.
    position = '098'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    add_obj_type( 'PROG' ).
    add_obj_type( 'CLAS' ).
    add_obj_type( 'FUGR' ).

    enable_rfc( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Empty catches should be combined'(m01) ).

  ENDMETHOD.

ENDCLASS.
