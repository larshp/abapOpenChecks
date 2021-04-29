CLASS zcl_aoc_check_01 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.

    METHODS check
        REDEFINITION.

  PROTECTED SECTION.
    METHODS contains_else
      IMPORTING
        !io_structure  TYPE REF TO zcl_aoc_structure
      RETURNING
        VALUE(rv_bool) TYPE abap_bool.
    METHODS run_check
      IMPORTING
        !io_structure TYPE REF TO zcl_aoc_structure
        !io_scan      TYPE REF TO zcl_aoc_scan.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_01 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lo_structure TYPE REF TO zcl_aoc_structure.


    lo_structure = zcl_aoc_structure=>build(
      it_tokens     = io_scan->tokens
      it_statements = io_scan->statements
      it_structures = io_scan->structures ).

    run_check(
      io_structure = lo_structure
      io_scan      = io_scan ).

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '001'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'IF in IF, can easily be reduced'(m01) ).

  ENDMETHOD.


  METHOD contains_else.

    DATA: lt_structure TYPE zcl_aoc_structure=>ty_structure_tt,
          lo_structure TYPE REF TO zcl_aoc_structure.

    lt_structure = io_structure->get_structure( ).

    LOOP AT lt_structure INTO lo_structure.
      IF lo_structure->get_type( ) = zcl_aoc_scan=>gc_structure_statement-else.
        rv_bool = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD run_check.

    DATA: lt_structure TYPE zcl_aoc_structure=>ty_structure_tt,
          lo_structure TYPE REF TO zcl_aoc_structure,
          lo_then      TYPE REF TO zcl_aoc_structure,
          lv_include   TYPE program,
          lv_if        TYPE i,
          lv_other     TYPE i.

    lt_structure = io_structure->get_structure( ).

    IF io_structure->get_type( ) = zcl_aoc_scan=>gc_structure_statement-if
        OR io_structure->get_type( ) = zcl_aoc_scan=>gc_structure_statement-else.

      IF io_structure->get_type( ) = zcl_aoc_scan=>gc_structure_statement-if.
        READ TABLE lt_structure INDEX 1 INTO lo_then.
        ASSERT sy-subrc = 0.

        LOOP AT lt_structure INTO lo_structure.
          CASE lo_structure->get_type( ).
            WHEN zcl_aoc_scan=>gc_structure_statement-elseif
                OR zcl_aoc_scan=>gc_structure_statement-else.
              lv_if = lv_if + 2.
          ENDCASE.
        ENDLOOP.
      ELSE.
        lo_then = io_structure.
      ENDIF.

      LOOP AT lt_structure INTO lo_structure.
        CASE lo_structure->get_type( ).
          WHEN zcl_aoc_scan=>gc_structure_statement-if.
            IF contains_else( lo_structure ) = abap_true
                AND io_structure->get_type( ) = zcl_aoc_scan=>gc_structure_statement-if.
              lv_if = lv_if + 1.
            ENDIF.
            lv_if = lv_if + 1.
          WHEN OTHERS.
            lv_other = lv_other + 1.
        ENDCASE.
      ENDLOOP.
    ENDIF.

    IF lv_if = 1 AND lv_other = 0.
      lv_include = io_scan->get_include( io_structure->get_statement( )-level ).
      inform( p_sub_obj_name = lv_include
              p_line = io_structure->get_statement( )-row
              p_kind = mv_errty
              p_test = myname
              p_code = '001' ).
    ELSE.
      LOOP AT lt_structure INTO lo_structure.
        run_check( io_structure = lo_structure
                   io_scan      = io_scan ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
