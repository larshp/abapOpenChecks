CLASS zcl_aoc_check_34 DEFINITION
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

    DATA mv_lines TYPE i .
    DATA mv_incl_comments TYPE flag .

    METHODS run_logic
      IMPORTING
        !is_statement     TYPE sstmnt
        !is_token         TYPE stokesx
        !iv_start         TYPE i
        !iv_comment_lines TYPE i
        !io_scan          TYPE REF TO zcl_aoc_scan .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_34 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_start         TYPE i,
          lv_comment_lines TYPE i.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF io_scan->statements,
                   <ls_token>     LIKE LINE OF io_scan->tokens.


    LOOP AT io_scan->statements ASSIGNING <ls_statement>
        WHERE type = io_scan->gc_statement-standard OR
              type = io_scan->gc_statement-comment.

      READ TABLE io_scan->tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
      ASSERT sy-subrc = 0.

      CASE <ls_token>-str.
        WHEN 'WHEN'.
          run_logic(
            is_statement     = <ls_statement>
            is_token         = <ls_token>
            iv_start         = lv_start
            iv_comment_lines = lv_comment_lines
            io_scan          = io_scan ).
          lv_comment_lines = 0.
          lv_start = <ls_token>-row.
        WHEN 'ENDCASE'.
          run_logic(
            is_statement     = <ls_statement>
            is_token         = <ls_token>
            iv_start         = lv_start
            iv_comment_lines = lv_comment_lines
            io_scan          = io_scan ).
          lv_comment_lines = 0.
          lv_start = 0.
        WHEN OTHERS.
          IF <ls_statement>-type = io_scan->gc_statement-comment.
            lv_comment_lines = lv_comment_lines + <ls_statement>-to - <ls_statement>-from + 1.
          ENDIF.
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '034'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    mv_lines = 20.

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Large WHEN construct'(m01) ).

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mv_lines = mv_lines
      mv_incl_comments = mv_incl_comments
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_lines 'Lines' ''.                     "#EC NOTEXT
    zzaoc_fill_att mv_incl_comments 'Include comments?' ''. "#EC NOTEXT
    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mv_lines = mv_lines
      mv_incl_comments = mv_incl_comments
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD run_logic.

    DATA: lv_include TYPE sobj_name.


    IF iv_start > 0 AND ( ( mv_incl_comments = abap_true
        AND iv_start + mv_lines < is_token-row )
        OR ( mv_incl_comments = abap_false
        AND iv_start + mv_lines < is_token-row - iv_comment_lines ) ).

      lv_include = io_scan->get_include( is_statement-level ).
      inform( p_sub_obj_name = lv_include
              p_line         = iv_start
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '001' ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
