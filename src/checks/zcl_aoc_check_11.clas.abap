CLASS zcl_aoc_check_11 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS check
        REDEFINITION.
    METHODS get_attributes
        REDEFINITION.
    METHODS if_ci_test~query_attributes
        REDEFINITION.
    METHODS put_attributes
        REDEFINITION.
  PROTECTED SECTION.

    DATA mv_skipc TYPE flag.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aoc_check_11 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_position          TYPE i,
          lv_include           TYPE program,
          lv_prev_row          TYPE token_row,
          lv_prev_level        TYPE stmnt_levl,
          lv_prev_inform_row   TYPE token_row,
          lv_prev_inform_level TYPE stmnt_levl.

    FIELD-SYMBOLS: <ls_statement>  LIKE LINE OF io_scan->statements,
                   <ls_token_to>   LIKE LINE OF io_scan->tokens,
                   <ls_level>      LIKE LINE OF io_scan->levels,
                   <ls_token_from> LIKE LINE OF io_scan->tokens.


    LOOP AT io_scan->statements ASSIGNING <ls_statement>
        WHERE terminator = '.'
        AND type <> io_scan->gc_statement-pragma.

      lv_position = sy-tabix.

      IF <ls_statement>-from > <ls_statement>-to.
        CONTINUE.
      ENDIF.

      READ TABLE io_scan->tokens ASSIGNING <ls_token_to> INDEX <ls_statement>-to.
      CHECK sy-subrc = 0.

      READ TABLE io_scan->tokens ASSIGNING <ls_token_from> INDEX <ls_statement>-from.
      CHECK sy-subrc = 0.

      IF <ls_statement>-level = lv_prev_level AND <ls_token_from>-row = lv_prev_row.
        READ TABLE io_scan->levels ASSIGNING <ls_level> INDEX <ls_statement>-level.
        IF sy-subrc = 0 AND ( <ls_level>-type = io_scan->gc_level-macro_define
            OR <ls_level>-type = io_scan->gc_level-macro_trmac ).
          CONTINUE.
        ENDIF.

        lv_include = io_scan->get_include( <ls_statement>-level ).
        IF mv_skipc = abap_true
            AND is_class_definition( lv_include ) = abap_true.
          CONTINUE. " current loop
        ENDIF.
        IF lv_prev_inform_row <> <ls_token_from>-row
            OR lv_prev_inform_level <> <ls_statement>-level.
          inform( p_sub_obj_name = lv_include
                  p_position     = lv_position
                  p_line         = <ls_token_from>-row
                  p_kind         = mv_errty
                  p_test         = myname
                  p_code         = '001' ).
        ENDIF.
        lv_prev_inform_row   = <ls_token_from>-row.
        lv_prev_inform_level = <ls_statement>-level.
      ENDIF.

      lv_prev_row   = <ls_token_to>-row.
      lv_prev_level = <ls_statement>-level.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '002'.
    position       = '011'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).
    enable_checksum( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Max one statement per line'(m01) ).

    mv_skipc = abap_true.

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mv_skipc = mv_skipc
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_skipc 'Skip global class definitions' 'C'. "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mv_skipc = mv_skipc
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
