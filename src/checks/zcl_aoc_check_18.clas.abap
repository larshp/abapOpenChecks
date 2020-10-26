CLASS zcl_aoc_check_18 DEFINITION
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



CLASS zcl_aoc_check_18 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_include TYPE program,
          lv_found   TYPE abap_bool.

    FIELD-SYMBOLS: <ls_structure> LIKE LINE OF io_scan->structures,
                   <ls_token>     LIKE LINE OF io_scan->tokens,
                   <ls_statement> LIKE LINE OF io_scan->statements.


    LOOP AT io_scan->structures ASSIGNING <ls_structure>
        WHERE type = io_scan->gc_structure-condition.

      lv_found = abap_false.

      LOOP AT io_scan->statements ASSIGNING <ls_statement>
          FROM <ls_structure>-stmnt_from TO <ls_structure>-stmnt_to.

        READ TABLE io_scan->tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
        IF sy-subrc <> 0 OR <ls_token>-type = io_scan->gc_token-comment.
          CONTINUE.
        ENDIF.

        CASE <ls_token>-str.
          WHEN 'WHEN' OR 'ELSEIF' OR 'ELSE'.
            CONTINUE.
          WHEN OTHERS.
            lv_found = abap_true.
            EXIT.
        ENDCASE.
      ENDLOOP.

      IF lv_found = abap_false.
        READ TABLE io_scan->statements ASSIGNING <ls_statement> INDEX <ls_structure>-stmnt_from.
        CHECK sy-subrc = 0.
        READ TABLE io_scan->tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
        CHECK sy-subrc = 0.

        lv_include = io_scan->get_include( <ls_statement>-level ).

        inform( p_sub_obj_name = lv_include
                p_position     = <ls_structure>-stmnt_from
                p_line         = <ls_token>-row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = '001' ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '018'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).
    enable_checksum( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Empty branch'(m01) ).

  ENDMETHOD.
ENDCLASS.
