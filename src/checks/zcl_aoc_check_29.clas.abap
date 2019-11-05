CLASS zcl_aoc_check_29 DEFINITION
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
  PRIVATE SECTION.

    DATA mv_name TYPE seoclsname.
ENDCLASS.



CLASS ZCL_AOC_CHECK_29 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_name  TYPE string,
          lv_str   TYPE string,
          lv_level TYPE stmnt_levl.

    FIELD-SYMBOLS: <ls_level>     LIKE LINE OF io_scan->levels,
                   <ls_statement> LIKE LINE OF io_scan->statements,
                   <ls_token>     LIKE LINE OF io_scan->tokens.


    LOOP AT io_scan->levels ASSIGNING <ls_level> WHERE type = io_scan->gc_level-program.
      lv_level = sy-tabix.

      LOOP AT io_scan->statements ASSIGNING <ls_statement>
          WHERE type <> io_scan->gc_statement-comment
          AND type <> io_scan->gc_statement-comment_in_stmnt
          AND level = lv_level.

        READ TABLE io_scan->tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
        IF sy-subrc <> 0 OR <ls_token>-str <> 'CLASS'.
          CONTINUE.
        ENDIF.

        READ TABLE io_scan->tokens ASSIGNING <ls_token> INDEX <ls_statement>-from + 1.
        ASSERT sy-subrc = 0.
        lv_name = <ls_token>-str.
        TRANSLATE lv_name TO UPPER CASE.

        CLEAR lv_str.
        LOOP AT io_scan->tokens ASSIGNING <ls_token>
            FROM <ls_statement>-from + 2
            TO <ls_statement>-to.
          IF lv_str IS INITIAL.
            lv_str = <ls_token>-str.
          ELSE.
            CONCATENATE lv_str <ls_token>-str INTO lv_str SEPARATED BY space.
          ENDIF.
        ENDLOOP.

        IF NOT lv_str CP '*FOR TESTING*'.
          CONTINUE.
        ENDIF.

        IF object_type = 'CLAS'
            AND strlen( <ls_level>-name ) = 32
            AND <ls_level>-name+30(2) = 'CU'.
          CONTINUE.
        ENDIF.

        IF NOT lv_name CP mv_name.
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

    version        = '001'.
    position       = '029'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    mv_name = 'LTCL_+*'.

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Naming, Local test classes'(m01) ).

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mv_name = mv_name
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_name 'Name' ''.                       "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mv_name  = mv_name
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
