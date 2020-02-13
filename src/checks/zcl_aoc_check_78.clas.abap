CLASS zcl_aoc_check_78 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_78 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_level   LIKE sy-tabix,
          lv_next    TYPE sy-tabix,
          ls_next    LIKE LINE OF io_scan->statements,
          lv_subrc   TYPE abap_bool,
          ls_token   LIKE LINE OF io_scan->tokens,
          lv_comment TYPE string.

    FIELD-SYMBOLS: <ls_level>     LIKE LINE OF io_scan->levels,
                   <ls_token>     LIKE LINE OF io_scan->tokens,
                   <ls_statement> LIKE LINE OF io_scan->statements.



    LOOP AT io_scan->levels ASSIGNING <ls_level>.
      lv_level = sy-tabix.

      LOOP AT io_scan->statements ASSIGNING <ls_statement>
          WHERE level = lv_level
          AND type = io_scan->gc_statement-comment.
        lv_next = sy-tabix + 1.

        CLEAR lv_comment.
        LOOP AT io_scan->tokens FROM <ls_statement>-from TO <ls_statement>-to ASSIGNING <ls_token>.
          CONCATENATE lv_comment <ls_token>-str INTO lv_comment.
        ENDLOOP.

        IF lv_comment NP '*EC CI_SUBRC*' OR lv_comment CP '#**'.
          CONTINUE.
        ENDIF.

        READ TABLE io_scan->statements INDEX lv_next INTO ls_next.
        IF sy-subrc <> 0
            OR ls_next-level <> lv_level
            OR ls_next-type = io_scan->gc_statement-comment.
          CONTINUE.
        ENDIF.

        READ TABLE io_scan->tokens INDEX ls_next-from INTO ls_token. "#EC CI_SUBRC
        IF ls_token-str = 'ASSERT'.
          CONTINUE.
        ENDIF.

        lv_subrc = abap_false.
        LOOP AT io_scan->tokens FROM ls_next-from TO ls_next-to TRANSPORTING NO FIELDS WHERE str = 'SY-SUBRC'.
          lv_subrc = abap_true.
        ENDLOOP.

        IF lv_subrc = abap_true.
          inform( p_sub_obj_name = io_scan->get_include( <ls_statement>-level )
                  p_line         = <ls_token>-row
                  p_column       = <ls_token>-col
                  p_kind         = mv_errty
                  p_test         = myname
                  p_code         = '001' ).
        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version     = '001'.
    position    = '078'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = '"#EC CI_SUBRC can be removed'(m01) ).

  ENDMETHOD.
ENDCLASS.
