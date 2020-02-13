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

    TYPES: BEGIN OF ty_struc_index_s,
             index TYPE i.
             INCLUDE TYPE sstruc.
    TYPES: END OF ty_struc_index_s,
           ty_struc_index_tt TYPE TABLE OF ty_struc_index_s WITH NON-UNIQUE DEFAULT KEY.

    METHODS inform_about_catch IMPORTING it_structures          TYPE ty_structures_tt
                                         it_complete_structures TYPE ty_structures_tt
                                         it_statements          TYPE sstmnt_tab.

    METHODS get_structure_index IMPORTING it_structures         TYPE ty_structures_tt
                                RETURNING VALUE(rt_struc_index) TYPE ty_struc_index_tt.
ENDCLASS.



CLASS ZCL_AOC_CHECK_98 IMPLEMENTATION.


  METHOD check.

    DATA lt_structures TYPE ty_structures_tt.
    DATA lv_last_back TYPE i.
    DATA ls_structure LIKE LINE OF lt_structures.
    DATA lt_empty_catches TYPE ty_structures_tt.
    DATA ls_struc_index TYPE ty_struc_index_s.
    DATA lt_struc_index TYPE ty_struc_index_tt.

    lt_struc_index = get_structure_index( it_structures = io_scan->structures ).

    DELETE lt_struc_index WHERE type <> 'C'.

    LOOP AT lt_struc_index INTO ls_struc_index.
      " is there logic for this structure?
      READ TABLE io_scan->structures WITH KEY back = ls_struc_index-index TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        " no! --> empty catch
        MOVE-CORRESPONDING ls_struc_index TO ls_structure.
        APPEND ls_structure TO lt_empty_catches.
        CLEAR ls_structure.
      ENDIF.
    ENDLOOP.

    CLEAR ls_structure.
    LOOP AT lt_empty_catches INTO ls_structure.
      IF lv_last_back IS NOT INITIAL AND lv_last_back <> ls_structure-back.
        inform_about_catch( it_structures = lt_structures
                            it_complete_structures = io_scan->structures
                            it_statements          = io_scan->statements ).
        CLEAR lt_structures.
      ENDIF.
      lv_last_back = ls_structure-back.
      APPEND ls_structure TO lt_structures.
      AT LAST.
        inform_about_catch( it_structures = lt_structures
                            it_complete_structures = io_scan->structures
                            it_statements = io_scan->statements ).
        CLEAR lt_structures.
      ENDAT.
    ENDLOOP.
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


  METHOD get_structure_index.
    DATA ls_struc_index TYPE ty_struc_index_s.
    DATA ls_structure LIKE LINE OF it_structures.
    LOOP AT it_structures INTO ls_structure.
      ls_struc_index-index = sy-tabix.
      MOVE-CORRESPONDING ls_structure TO ls_struc_index.
      APPEND ls_struc_index TO rt_struc_index.
      CLEAR ls_struc_index.
    ENDLOOP.
  ENDMETHOD.


  METHOD inform_about_catch.
    DATA ls_structure LIKE LINE OF it_structures.
    DATA ls_statement LIKE LINE OF it_statements.

    IF lines( it_structures ) < 2.
      RETURN.
    ENDIF.

    READ TABLE it_structures INTO ls_structure INDEX 1.
    READ TABLE it_complete_structures INTO ls_structure INDEX ls_structure-back.
    READ TABLE it_statements INTO ls_statement INDEX ls_structure-stmnt_from.

    inform( p_sub_obj_name = program_name
            p_line         = ls_statement-trow
            p_kind         = mv_errty
            p_test         = myname
            p_code         = '001' ).
  ENDMETHOD.
ENDCLASS.
