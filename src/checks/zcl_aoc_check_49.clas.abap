CLASS zcl_aoc_check_49 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_code,
        text TYPE c LENGTH 255,
        row  TYPE token_row,
        name TYPE level_name,
      END OF ty_code.
    TYPES:
      ty_code_tt TYPE STANDARD TABLE OF ty_code WITH DEFAULT KEY.

    METHODS build
      IMPORTING
        !it_tokens     TYPE stokesx_tab
        !it_statements TYPE sstmnt_tab
        !it_levels     TYPE slevel_tab
      RETURNING
        VALUE(rt_code) TYPE ty_code_tt.
    METHODS check_code
      IMPORTING
        !it_code TYPE ty_code_tt.
ENDCLASS.



CLASS ZCL_AOC_CHECK_49 IMPLEMENTATION.


  METHOD build.

    DATA: lv_offset TYPE i,
          lv_str    TYPE string,
          lv_level  LIKE sy-tabix.

    FIELD-SYMBOLS: <ls_level>     LIKE LINE OF it_levels,
                   <ls_statement> LIKE LINE OF it_statements,
                   <ls_token>     LIKE LINE OF it_tokens,
                   <ls_code>      LIKE LINE OF rt_code.


    LOOP AT it_levels ASSIGNING <ls_level> WHERE type = zcl_aoc_scan=>gc_level-program.
      lv_level = sy-tabix.

      LOOP AT it_statements ASSIGNING <ls_statement>
          WHERE level = lv_level
          AND coloncol = 0.

        UNASSIGN <ls_code>.
        LOOP AT it_tokens ASSIGNING <ls_token>
            FROM <ls_statement>-from TO <ls_statement>-to
            WHERE type <> zcl_aoc_scan=>gc_token-comment.
          IF NOT <ls_code> IS ASSIGNED.
            APPEND INITIAL LINE TO rt_code ASSIGNING <ls_code>.
            <ls_code>-name = <ls_level>-name.
            <ls_code>-row = <ls_token>-row.
          ELSEIF <ls_code>-row <> <ls_token>-row.
* only the first row of the statement is needed when checking for spaces
            EXIT.
          ENDIF.

          lv_str = <ls_token>-str.
          IF <ls_token>-type = zcl_aoc_scan=>gc_token-literal.
            REPLACE ALL OCCURRENCES OF ` ` IN lv_str WITH 'A'.
          ENDIF.

          lv_offset = <ls_token>-col.
          <ls_code>-text+lv_offset = lv_str.
        ENDLOOP.

      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_code TYPE ty_code_tt.


    lt_code = build( it_tokens     = io_scan->tokens
                     it_statements = io_scan->statements
                     it_levels     = io_scan->levels ).

    check_code( lt_code ).

  ENDMETHOD.


  METHOD check_code.

    DATA: lv_error TYPE sci_errc,
          lv_code  TYPE c LENGTH 255.

    FIELD-SYMBOLS: <ls_code> LIKE LINE OF it_code.


    LOOP AT it_code ASSIGNING <ls_code>.

      CLEAR lv_error.

      lv_code = <ls_code>-text.
      WHILE lv_code(1) = space.
        SHIFT lv_code LEFT DELETING LEADING space IN CHARACTER MODE.
      ENDWHILE.

      IF lv_code CP 'IF  *'.
        lv_error = '001'.
      ELSEIF lv_code CP 'SHIFT  *'.
        lv_error = '002'.
      ELSEIF lv_code CP 'WHEN  *'.
        lv_error = '003'.
      ELSEIF lv_code CP 'READ TABLE  *'.
        lv_error = '004'.
      ELSEIF lv_code CP 'MODIFY  *'.
        lv_error = '005'.
      ELSEIF lv_code CP 'DELETE  *'.
        lv_error = '006'.
      ELSEIF lv_code CP 'COLLECT  *'.
        lv_error = '007'.
      ELSEIF lv_code CP 'CHECK  *'.
        lv_error = '008'.
      ELSEIF lv_code CP 'SORT  *'.
        lv_error = '009'.
      ELSEIF lv_code CP 'REPORT  *' AND NOT lv_code = 'REPORT'.
        lv_error = '010'.
      ELSEIF lv_code CP 'ELSEIF  *'.
        lv_error = '011'.
      ELSEIF lv_code CP 'DATA  *'.
        lv_error = '012'.
      ELSEIF lv_code CP 'SET TITLEBAR  *'.
        lv_error = '013'.
      ELSEIF lv_code CP 'MOVE-CORRESPONDING  *'.
        lv_error = '014'.
      ELSEIF lv_code CP 'APPEND  *'.
        lv_error = '015'.
      ELSEIF lv_code CP 'METHOD  *'.
        lv_error = '017'.
      ENDIF.

      IF lv_error IS INITIAL.
        FIND REGEX '\w+\(  [ ]*\)' IN lv_code ##NO_TEXT.
        IF sy-subrc = 0.
          lv_error = '016'.
        ENDIF.
      ENDIF.

      IF lv_error IS INITIAL.
        FIND REGEX '\([ ]{2}[ ]*\S' IN lv_code ##NO_TEXT.
        IF sy-subrc = 0.
          lv_error = '018'.
        ENDIF.
      ENDIF.

      IF lv_error IS INITIAL.
        FIND REGEX '\S[ ]*[ ]{2}\)' IN lv_code ##NO_TEXT.
        IF sy-subrc = 0.
          lv_error = '019'.
        ENDIF.
      ENDIF.

      IF NOT lv_error IS INITIAL.
        inform( p_sub_obj_name = <ls_code>-name
                p_line         = <ls_code>-row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = lv_error ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '049'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Double space after IF'(m01) ).

    insert_scimessage(
        iv_code = '002'
        iv_text = 'Double space after SHIFT'(m02) ).

    insert_scimessage(
        iv_code = '003'
        iv_text = 'Double space after WHEN'(m03) ).

    insert_scimessage(
        iv_code = '004'
        iv_text = 'Double space after READ TABLE'(m04) ).

    insert_scimessage(
        iv_code = '005'
        iv_text = 'Double space after MODIFY'(m05) ).

    insert_scimessage(
        iv_code = '006'
        iv_text = 'Double space after DELETE'(m06) ).

    insert_scimessage(
        iv_code = '007'
        iv_text = 'Double space after COLLECT'(m07) ).

    insert_scimessage(
        iv_code = '008'
        iv_text = 'Double space after CHECK'(m08) ).

    insert_scimessage(
        iv_code = '009'
        iv_text = 'Double space after SORT'(m09) ).

    insert_scimessage(
        iv_code = '010'
        iv_text = 'Double space after REPORT'(m10) ).

    insert_scimessage(
        iv_code = '011'
        iv_text = 'Double space after ELSEIF'(m11) ).

    insert_scimessage(
        iv_code = '012'
        iv_text = 'Double space after DATA'(m12) ).

    insert_scimessage(
        iv_code = '013'
        iv_text = 'Double space after SET TITLEBAR'(m13) ).

    insert_scimessage(
        iv_code = '014'
        iv_text = 'Double space after MOVE-CORRESPONDING'(m14) ).

    insert_scimessage(
        iv_code = '015'
        iv_text = 'Double space after APPEND'(m15) ).

    insert_scimessage(
        iv_code = '016'
        iv_text = 'Double space in method call'(m16) ).

    insert_scimessage(
        iv_code = '017'
        iv_text = 'Double space after METHOD'(m17) ).

    insert_scimessage(
        iv_code = '018'
        iv_text = 'Double space after start parenthesis'(m18) ).

    insert_scimessage(
        iv_code = '019'
        iv_text = 'Double space before end parenthesis'(m19) ).

  ENDMETHOD.
ENDCLASS.
