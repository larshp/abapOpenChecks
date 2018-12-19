CLASS zcl_aoc_check_49 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS check
        REDEFINITION.
    METHODS get_message_text
        REDEFINITION.
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


    LOOP AT it_levels ASSIGNING <ls_level> WHERE type = scan_level_type-program.
      lv_level = sy-tabix.

      LOOP AT it_statements ASSIGNING <ls_statement>
          WHERE level = lv_level
          AND coloncol = 0.

        UNASSIGN <ls_code>.
        LOOP AT it_tokens ASSIGNING <ls_token>
            FROM <ls_statement>-from TO <ls_statement>-to
            WHERE type <> scan_token_type-comment.
          IF NOT <ls_code> IS ASSIGNED.
            APPEND INITIAL LINE TO rt_code ASSIGNING <ls_code>.
            <ls_code>-name = <ls_level>-name.
            <ls_code>-row = <ls_token>-row.
          ELSEIF <ls_code>-row <> <ls_token>-row.
* only the first row of the statement is needed when checking for spaces
            EXIT.
          ENDIF.

          lv_str = <ls_token>-str.
          IF <ls_token>-type = scan_token_type-literal.
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


    lt_code = build( it_tokens     = it_tokens
                     it_statements = it_statements
                     it_levels     = it_levels ).

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
        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = <ls_code>-name
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

    mv_errty = c_error.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Double space after IF'.                   "#EC NOTEXT
      WHEN '002'.
        p_text = 'Double space after SHIFT'.                "#EC NOTEXT
      WHEN '003'.
        p_text = 'Double space after WHEN'.                 "#EC NOTEXT
      WHEN '004'.
        p_text = 'Double space after READ TABLE'.           "#EC NOTEXT
      WHEN '005'.
        p_text = 'Double space after MODIFY'.               "#EC NOTEXT
      WHEN '006'.
        p_text = 'Double space after DELETE'.               "#EC NOTEXT
      WHEN '007'.
        p_text = 'Double space after COLLECT'.              "#EC NOTEXT
      WHEN '008'.
        p_text = 'Double space after CHECK'.                "#EC NOTEXT
      WHEN '009'.
        p_text = 'Double space after SORT'.                 "#EC NOTEXT
      WHEN '010'.
        p_text = 'Double space after REPORT'.               "#EC NOTEXT
      WHEN '011'.
        p_text = 'Double space after ELSEIF'.               "#EC NOTEXT
      WHEN '012'.
        p_text = 'Double space after DATA'.                 "#EC NOTEXT
      WHEN '013'.
        p_text = 'Double space after SET TITLEBAR'.         "#EC NOTEXT
      WHEN '014'.
        p_text = 'Double space after MOVE-CORRESPONDING'.   "#EC NOTEXT
      WHEN '015'.
        p_text = 'Double space after APPEND'.               "#EC NOTEXT
      WHEN '016'.
        p_text = 'Double space in method call'.             "#EC NOTEXT
      WHEN '017'.
        p_text = 'Double space after METHOD'.               "#EC NOTEXT
      WHEN '018'.
        p_text = 'Double space after start parenthesis'.    "#EC NOTEXT
      WHEN '019'.
        p_text = 'Double space before end parenthesis'.     "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.                    "GET_MESSAGE_TEXT
ENDCLASS.
