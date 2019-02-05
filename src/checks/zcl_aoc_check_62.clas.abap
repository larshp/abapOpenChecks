CLASS zcl_aoc_check_62 DEFINITION
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

    METHODS check_continue
      IMPORTING
        !is_statement  TYPE ty_statement
        !is_next       TYPE ty_statement
        !is_next_next  TYPE ty_statement
      RETURNING
        VALUE(rv_code) TYPE sci_errc .
    METHODS check_delete
      IMPORTING
        !is_statement  TYPE ty_statement
        !is_next       TYPE ty_statement
      RETURNING
        VALUE(rv_code) TYPE sci_errc .
    METHODS check_lines
      IMPORTING
        !is_statement  TYPE ty_statement
        !is_next       TYPE ty_statement
      RETURNING
        VALUE(rv_code) TYPE sci_errc .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_62 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_statements TYPE ty_statements,
          lv_index      TYPE i,
          lv_code       TYPE sci_errc.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF lt_statements,
                   <ls_next>      LIKE LINE OF lt_statements,
                   <ls_next_next> LIKE LINE OF lt_statements.


    lt_statements = build_statements(
      it_tokens     = it_tokens
      it_statements = it_statements ).

    LOOP AT lt_statements ASSIGNING <ls_statement>.
      lv_index = sy-tabix + 1.
      READ TABLE lt_statements INDEX lv_index ASSIGNING <ls_next>.
      IF sy-subrc <> 0 OR <ls_next>-include <> <ls_statement>-include.
        CONTINUE.
      ENDIF.

      lv_index = lv_index + 1.
      READ TABLE lt_statements INDEX lv_index ASSIGNING <ls_next_next>.
      IF sy-subrc <> 0 OR <ls_next_next>-include <> <ls_statement>-include.
        CONTINUE.
      ENDIF.

      lv_code = check_delete(
        is_statement = <ls_statement>
        is_next      = <ls_next> ).

      IF lv_code IS INITIAL.
        lv_code = check_continue(
          is_statement = <ls_statement>
          is_next      = <ls_next>
          is_next_next = <ls_next_next> ).
      ENDIF.

      IF lv_code IS INITIAL.
        lv_code = check_lines(
          is_statement = <ls_statement>
          is_next      = <ls_next> ).
      ENDIF.

      IF NOT lv_code IS INITIAL.
        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = <ls_statement>-include
                p_line         = <ls_statement>-start-row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = lv_code ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_continue.

* this is a limited check, but it covers the examples I typically see implemented

    IF is_statement-str = 'CONTINUE'
        AND ( is_next-str = 'ENDDO'
        OR is_next-str = 'ENDLOOP'
        OR is_next-str = 'ENDWHILE' ).
      rv_code = '002'.
    ENDIF.


    IF is_statement-str = 'CONTINUE'
        AND ( is_next-str = 'ENDIF'
        OR is_next-str = 'ENDTRY' )
        AND ( is_next_next-str = 'ENDDO'
        OR is_next_next-str = 'ENDLOOP'
        OR is_next_next-str = 'ENDWHILE' ).
      rv_code = '002'.
    ENDIF.

  ENDMETHOD.


  METHOD check_delete.

    DATA: lv_target TYPE string,
          lv_source TYPE string.


    IF is_statement-str CP 'LOOP AT *'.
      FIND REGEX ' (\w+) ASSIGNING (<\w+>)' IN is_statement-str
        SUBMATCHES lv_source lv_target ##NO_TEXT.
      IF sy-subrc <> 0.
        FIND REGEX ' (\w+) INTO DATA\((\w+)\)' IN is_statement-str
          SUBMATCHES lv_source lv_target ##NO_TEXT.
      ENDIF.
      IF sy-subrc <> 0.
        FIND REGEX ' (\w+) ASSIGNING FIELD-SYMBOL\((<\w+>)\)' IN is_statement-str
          SUBMATCHES lv_source lv_target ##NO_TEXT.
      ENDIF.
      IF sy-subrc <> 0.
        FIND REGEX ' (\w+) INTO (\w+)' IN is_statement-str
          SUBMATCHES lv_source lv_target ##NO_TEXT.
      ENDIF.

      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      IF is_next-str = |DELETE { lv_source } FROM { lv_target }|.
        rv_code = '001'.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD check_lines.

    DATA: lv_table TYPE string.

    FIND REGEX '^IF NOT (\w+) IS INITIAL$' IN is_statement-str SUBMATCHES lv_table ##NO_TEXT.
    IF sy-subrc <> 0.
      FIND REGEX '^IF (\w+) IS NOT INITIAL$' IN is_statement-str SUBMATCHES lv_table ##NO_TEXT.
    ENDIF.
    IF sy-subrc <> 0.
* assuming LINES method is not overwritten by custom method
      FIND REGEX '^IF LINES\( (\w+) \) > 0$' IN is_statement-str SUBMATCHES lv_table ##NO_TEXT.
    ENDIF.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lv_table = |^LOOP AT { lv_table } |.
    FIND REGEX lv_table IN is_next-str.
    IF sy-subrc = 0.
      rv_code = '003'.
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version     = '001'.
    position    = '062'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    mv_errty   = c_error.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Use DELETE WHERE instead'.                "#EC NOTEXT
      WHEN '002'.
        p_text = 'CONTINUE as last statement in loop'.      "#EC NOTEXT
      WHEN '003'.
        p_text = 'Checking for lines before looping'.       "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
