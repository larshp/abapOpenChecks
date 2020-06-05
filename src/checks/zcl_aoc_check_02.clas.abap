CLASS zcl_aoc_check_02 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS check
        REDEFINITION.
    METHODS if_ci_test~query_attributes
        REDEFINITION.
    METHODS put_attributes
        REDEFINITION.
    METHODS get_attributes
        REDEFINITION.

  PROTECTED SECTION.
    DATA mv_check TYPE flag.
    DATA mv_exit TYPE flag.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aoc_check_02 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_keyword TYPE string,
          lv_line    TYPE token_row,
          lv_include TYPE program,
          lv_error   TYPE sci_errc,
          lv_index   LIKE sy-tabix.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF io_scan->statements.


    LOOP AT io_scan->statements ASSIGNING <ls_statement>.
      lv_index = sy-tabix.

      lv_keyword = io_scan->statement_keyword( lv_index ).

      IF lv_keyword = 'EXIT' AND mv_exit = abap_true.
        lv_error = '001'.
      ELSEIF lv_keyword = 'CHECK' AND mv_check = abap_true.
        lv_error = '002'.
      ELSE.
        CONTINUE. " current loop
      ENDIF.

      LOOP AT io_scan->structures TRANSPORTING NO FIELDS
          WHERE ( stmnt_type = zcl_aoc_scan=>gc_structure_statement-loop
          OR stmnt_type = zcl_aoc_scan=>gc_structure_statement-while
          OR stmnt_type = zcl_aoc_scan=>gc_structure_statement-do
          OR stmnt_type = zcl_aoc_scan=>gc_structure_statement-select )
          AND stmnt_from <= lv_index
          AND stmnt_to >= lv_index.
        EXIT. " current loop
      ENDLOOP.
      IF sy-subrc <> 0.
        lv_line = io_scan->statement_row( lv_index ).

        lv_include = io_scan->get_include( <ls_statement>-level ).

        inform( p_sub_obj_name = lv_include
                p_position     = lv_index
                p_line         = lv_line
                p_kind         = mv_errty
                p_test         = myname
                p_code         = lv_error ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version  = '001'.
    position = '002'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).
    enable_checksum( ).

    mv_check = abap_true.
    mv_exit  = abap_true.

    insert_scimessage(
        iv_code = '001'
        iv_text = 'EXIT outside loop, use RETURN instead'(m01) ).
    insert_scimessage(
        iv_code = '002'
        iv_text = 'CHECK outside of loop'(m02) ).

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mv_check = mv_check
      mv_exit = mv_exit
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_check 'CHECK' 'C'.                    "#EC NOTEXT
    zzaoc_fill_att mv_exit 'EXIT' 'C'.                      "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mv_check = mv_check
      mv_exit = mv_exit
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
