CLASS zcl_aoc_check_22 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
  PROTECTED SECTION.

    DATA mo_scan TYPE REF TO zcl_aoc_scan .

    METHODS analyze_condition
      IMPORTING
        !io_structure TYPE REF TO zcl_aoc_structure .
    METHODS compare
      IMPORTING
        !it_structure  TYPE zcl_aoc_structure=>ty_structure_tt
        !iv_first_last TYPE abap_bool .
    METHODS loop
      IMPORTING
        !io_structure TYPE REF TO zcl_aoc_structure .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_22 IMPLEMENTATION.


  METHOD analyze_condition.

    DATA: lt_structure TYPE zcl_aoc_structure=>ty_structure_tt,
          lo_structure TYPE REF TO zcl_aoc_structure,
          lv_found     TYPE abap_bool.

    lt_structure = io_structure->get_structure( ).

* IFs must contain ELSE, CASE must contain OTHERS
    LOOP AT lt_structure INTO lo_structure.
      IF ( io_structure->get_type( ) = zcl_aoc_scan=>gc_structure_statement-if
          AND lo_structure->get_statement( )-statement = 'ELSE' )
          OR ( io_structure->get_type( ) = zcl_aoc_scan=>gc_structure_statement-case
          AND lo_structure->get_statement( )-statement = 'WHEN OTHERS' ).
        lv_found = abap_true.
        EXIT. " current loop.
      ENDIF.
    ENDLOOP.

    IF lv_found = abap_false.
      RETURN.
    ENDIF.

    compare( it_structure  = io_structure->get_structure( )
             iv_first_last = abap_true ).
    compare( it_structure  = io_structure->get_structure( )
             iv_first_last = abap_false ).

  ENDMETHOD.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    mo_scan = io_scan.

    loop( zcl_aoc_structure=>build( it_tokens     = io_scan->tokens
                                    it_statements = io_scan->statements
                                    it_structures = io_scan->structures ) ).

  ENDMETHOD.


  METHOD compare.

    DATA: lt_structure TYPE zcl_aoc_structure=>ty_structure_tt,
          lo_stru      TYPE REF TO zcl_aoc_structure,
          lo_first     TYPE REF TO zcl_aoc_structure,
          lo_compare   TYPE REF TO zcl_aoc_structure,
          lv_str1      TYPE string,
          lv_str2      TYPE string,
          lv_index     TYPE i.

    IF lines( it_structure ) = 1.
      RETURN.
    ENDIF.

* compare first or last statement in each branch
    LOOP AT it_structure INTO lo_stru.

      IF iv_first_last = abap_true.
        lv_index = 1.
      ELSE.
        lv_index = lines( lo_stru->get_structure( ) ).
      ENDIF.

      lt_structure = lo_stru->get_structure( ).

      IF NOT lo_first IS BOUND.
        READ TABLE lt_structure INDEX lv_index INTO lo_first.
        IF sy-subrc <> 0.
          RETURN.
        ENDIF.
        lv_str1 = zcl_aoc_structure=>to_string_simple( lo_first ).
        CONTINUE. " current loop
      ENDIF.
      READ TABLE lt_structure INDEX lv_index INTO lo_compare.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      lv_str2 = zcl_aoc_structure=>to_string_simple( lo_compare ).
      IF lv_str1 <> lv_str2.
        RETURN.
      ENDIF.

    ENDLOOP.
    IF sy-subrc <> 0.
      RETURN. " list is empty
    ENDIF.

    IF lv_str1 IS INITIAL OR lv_str1 = 'ENDIF'.
      RETURN.
    ENDIF.

    inform( p_sub_obj_name = mo_scan->get_include( lo_first->get_statement( )-level )
            p_line         = lo_first->get_statement( )-row
            p_kind         = mv_errty
            p_test         = myname
            p_code         = '001'
            p_param_1      = lv_str1 ).

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '022'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Conditions contain identical code, &1'(m01) ).

  ENDMETHOD.


  METHOD loop.

    DATA: lt_structure TYPE zcl_aoc_structure=>ty_structure_tt,
          lo_structure TYPE REF TO zcl_aoc_structure.


    CASE io_structure->get_type( ).
      WHEN zcl_aoc_scan=>gc_structure_statement-if
          OR zcl_aoc_scan=>gc_structure_statement-case.
        analyze_condition( io_structure ).
    ENDCASE.

    lt_structure = io_structure->get_structure( ).

    LOOP AT lt_structure INTO lo_structure.
      loop( lo_structure ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
