CLASS zcl_aoc_check_12 DEFINITION
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



CLASS ZCL_AOC_CHECK_12 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_include        TYPE program,
          lv_lines          TYPE i,
          lv_statement_sort TYPE string,
          lv_error          TYPE abap_bool,
          lt_results        TYPE TABLE OF string,
          lv_statement      LIKE LINE OF lt_results.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF io_scan->statements,
                   <ls_token>     LIKE LINE OF io_scan->tokens.


    LOOP AT io_scan->statements ASSIGNING <ls_statement> WHERE type = io_scan->gc_statement-standard.

      READ TABLE io_scan->tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
      CHECK sy-subrc = 0 AND <ls_token>-str = 'SORT'.

      CLEAR lv_statement.
      LOOP AT io_scan->tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from TO <ls_statement>-to.
        IF lv_statement IS INITIAL.
          lv_statement = <ls_token>-str.
        ELSE.
          CONCATENATE lv_statement <ls_token>-str
            INTO lv_statement SEPARATED BY space.
        ENDIF.
      ENDLOOP.

* parsing, derpy derp
      SPLIT lv_statement AT ' BY ' INTO lv_statement_sort lv_statement.
      IF lv_statement IS INITIAL OR lv_statement CA '()'.
        CONTINUE. " current loop
      ENDIF.

      "check priority before by
      SPLIT lv_statement_sort AT space INTO TABLE lt_results.
      lv_lines = lines( lt_results ).
      IF lv_lines >= 3.
        READ TABLE lt_results INTO lv_statement_sort INDEX lv_lines. "#EC CI_SUBRC
        IF lv_statement_sort = 'ASCENDING' OR lv_statement_sort = 'DESCENDING'.
          CONTINUE.
        ENDIF.
      ENDIF.

      "check priority after each component
      REPLACE ALL OCCURRENCES OF ' AS TEXT' IN lv_statement WITH ''.
      SPLIT lv_statement AT space INTO TABLE lt_results.

      lv_error = abap_false.
      IF lines( lt_results ) MOD 2 <> 0.
        lv_error = abap_true.
      ENDIF.

      LOOP AT lt_results INTO lv_statement.
        IF sy-tabix MOD 2 = 0
            AND lv_statement <> 'ASCENDING'
            AND lv_statement <> 'DESCENDING'.
          lv_error = abap_true.
        ENDIF.
      ENDLOOP.

      IF lv_error = abap_true.
        lv_include = io_scan->get_include( <ls_statement>-level ).

        inform( p_sub_obj_name = lv_include
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
    position       = '012'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Specify SORT order'(m01) ).

  ENDMETHOD.
ENDCLASS.
