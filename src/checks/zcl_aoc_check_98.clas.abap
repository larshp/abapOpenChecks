CLASS zcl_aoc_check_98 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .
  PUBLIC SECTION.
    METHODS constructor .
    METHODS check REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
    METHODS check_if_already_combined IMPORTING it_statements TYPE sstmnt_tab
                                      CHANGING  ct_structures TYPE ty_structures_tt.
    METHODS inform_about_catch IMPORTING it_structures          TYPE ty_structures_tt
                                         it_complete_structures TYPE ty_structures_tt
                                         it_statements          TYPE sstmnt_tab.
ENDCLASS.



CLASS ZCL_AOC_CHECK_98 IMPLEMENTATION.


  METHOD check.
    DATA lt_structures TYPE ty_structures_tt.
    DATA lv_last_back TYPE i.
    DATA lt_copy_of_structures TYPE ty_structures_tt.
    DATA ls_structure LIKE LINE OF lt_copy_of_structures.

    lt_copy_of_structures = io_scan->structures.

    DELETE lt_copy_of_structures WHERE type <> 'C'.

    LOOP AT lt_copy_of_structures INTO ls_structure.
      " empty catch statements
      IF ls_structure-stmnt_from = ls_structure-stmnt_to.
        IF lv_last_back <> ls_structure-back AND lv_last_back IS NOT INITIAL.
          check_if_already_combined( EXPORTING it_statements = io_scan->statements
                                     CHANGING ct_structures = lt_structures ).
          inform_about_catch( it_structures = lt_structures
                              it_complete_structures = io_scan->structures
                              it_statements = io_scan->statements ).
          CLEAR lt_structures.
        ENDIF.
        lv_last_back = ls_structure-back.
        APPEND ls_structure TO lt_structures.
      ENDIF.
      AT LAST.
        check_if_already_combined( EXPORTING it_statements = io_scan->statements
                                   CHANGING ct_structures = lt_structures ).
        inform_about_catch( it_structures = lt_structures
                            it_complete_structures = io_scan->structures
                            it_statements = io_scan->statements ).
        CLEAR lt_structures.
      ENDAT.
    ENDLOOP.
  ENDMETHOD.


  METHOD check_if_already_combined.
    " If the catches are already combined, the entry will be deleted from the structure table
    " then the check in the inform method will fail --> no info --> good
    DATA ls_structure LIKE LINE OF ct_structures.
    DATA lt_structures LIKE ct_structures.
    DATA ls_statement LIKE LINE OF it_statements.
    LOOP AT ct_structures INTO ls_structure.
      READ TABLE it_statements INTO ls_statement
      INDEX ls_structure-stmnt_from.
      IF ls_statement-terminator = '.'.
        APPEND ls_structure TO lt_structures.
      ENDIF.
    ENDLOOP.
    ct_structures = lt_structures.
  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '098'.

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


  METHOD inform_about_catch.
    DATA lv_lines TYPE i.
    DATA ls_structure LIKE LINE OF it_structures.
    DATA ls_statement LIKE LINE OF it_statements.

    DESCRIBE TABLE it_structures LINES lv_lines.
    IF lv_lines < 2.
      RETURN.
    ENDIF.

    READ TABLE it_structures INTO ls_structure
    INDEX 1.
    READ TABLE it_complete_structures INTO ls_structure
    INDEX ls_structure-back.
    READ TABLE it_statements INTO ls_statement
    INDEX ls_structure-stmnt_from.

    inform( p_sub_obj_name = program_name
            p_line         = ls_statement-trow
            p_kind         = mv_errty
            p_test         = myname
            p_code         = '001' ).
  ENDMETHOD.
ENDCLASS.
