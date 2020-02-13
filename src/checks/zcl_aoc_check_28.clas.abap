CLASS zcl_aoc_check_28 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
    METHODS get_attributes
        REDEFINITION .
    METHODS if_ci_test~query_attributes
        REDEFINITION .
    METHODS put_attributes
        REDEFINITION .
  PROTECTED SECTION.

    DATA mv_skipc TYPE flag .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_28 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_len   TYPE i,
          lt_code  TYPE string_table,
          lv_code  LIKE LINE OF lt_code,
          lv_level TYPE stmnt_levl.

    FIELD-SYMBOLS: <ls_level>     LIKE LINE OF io_scan->levels,
                   <ls_statement> LIKE LINE OF io_scan->statements,
                   <ls_token>     LIKE LINE OF io_scan->tokens.


    LOOP AT io_scan->levels ASSIGNING <ls_level> WHERE type = io_scan->gc_level-program.
      lv_level = sy-tabix.

      IF is_class_pool( <ls_level>-name ) = abap_true.
        CONTINUE.
      ELSEIF mv_skipc = abap_true
          AND is_class_definition( <ls_level>-name ) = abap_true.
        CONTINUE. " current loop
      ENDIF.

      LOOP AT io_scan->statements ASSIGNING <ls_statement>
          WHERE type <> io_scan->gc_statement-comment
          AND type <> io_scan->gc_statement-comment_in_stmnt
          AND terminator <> ''
          AND level = lv_level.

        READ TABLE io_scan->tokens ASSIGNING <ls_token> INDEX <ls_statement>-to.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        lv_len = <ls_token>-col + strlen( <ls_token>-str ).

        IF <ls_statement>-tcol > lv_len.
          lt_code = get_source( <ls_level> ).
          READ TABLE lt_code INDEX <ls_token>-row INTO lv_code.
* pragmas are not part of IT_TOKENS or IT_STATEMENTS, so check:
          IF sy-subrc = 0 AND lv_code CP '*##*'.
            CONTINUE. " current loop
          ENDIF.

          inform( p_sub_obj_name = <ls_level>-name
                  p_line         = <ls_token>-row
                  p_kind         = mv_errty
                  p_test         = myname
                  p_code         = '001' ).
        ENDIF.

      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version  = '002'.
    position = '028'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    mv_skipc = abap_true.

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Space before . or ,'(m01) ).

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
