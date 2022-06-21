CLASS zcl_aoc_check_98 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS check
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_AOC_CHECK_98 IMPLEMENTATION.

  METHOD check.
    DATA lv_empty_catch_count TYPE i.

    LOOP AT io_scan->structures ASSIGNING FIELD-SYMBOL(<ls_structure_try>) "#EC CI_SEL_NESTED
        WHERE stmnt_type = zcl_aoc_scan=>gc_structure_statement-try.

      CLEAR lv_empty_catch_count.

      LOOP AT io_scan->structures ASSIGNING FIELD-SYMBOL(<ls_structure_catch>)
          FROM <ls_structure_try>-struc_from TO <ls_structure_try>-struc_to
          WHERE stmnt_type = zcl_aoc_scan=>gc_structure_statement-catch.

        IF <ls_structure_catch>-stmnt_from = <ls_structure_catch>-stmnt_to.      "Empty catch.
          lv_empty_catch_count = lv_empty_catch_count + 1.
        ENDIF.

        IF lv_empty_catch_count = 2.                      "#EC CI_MAGIC
          READ TABLE io_scan->statements ASSIGNING FIELD-SYMBOL(<ls_statement>)
              INDEX <ls_structure_catch>-stmnt_from.      "#EC CI_SUBRC
          inform( p_sub_obj_name = program_name
                  p_line         = <ls_statement>-trow
                  p_kind         = mv_errty
                  p_test         = myname
                  p_code         = '001' ).
        ENDIF.
      ENDLOOP.
    ENDLOOP.

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
