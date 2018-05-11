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
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_62 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_statements TYPE ty_statements,
          lv_index      TYPE i,
          lv_target     TYPE string,
          lv_source     TYPE string,
          lv_code       TYPE sci_errc.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF lt_statements,
                   <ls_next>      LIKE LINE OF lt_statements.


    lt_statements = build_statements(
      it_tokens     = it_tokens
      it_statements = it_statements
      it_levels     = it_levels ).

    LOOP AT lt_statements ASSIGNING <ls_statement>.
      lv_index = sy-tabix + 1.

      READ TABLE lt_statements INDEX lv_index ASSIGNING <ls_next>.
      IF sy-subrc <> 0 OR <ls_next>-include <> <ls_statement>-include.
        CONTINUE.
      ENDIF.

      CLEAR lv_code.

      IF <ls_statement>-str CP 'LOOP AT *'.
        FIND REGEX ' (\w+) ASSIGNING (<\w+>)' IN <ls_statement>-str
          SUBMATCHES lv_source lv_target ##NO_TEXT.
        IF sy-subrc <> 0.
          FIND REGEX ' (\w+) INTO DATA\((\w+)\)' IN <ls_statement>-str
            SUBMATCHES lv_source lv_target ##NO_TEXT.
        ENDIF.
        IF sy-subrc <> 0.
          FIND REGEX ' (\w+) ASSIGNING FIELD-SYMBOL\((<\w+>)\)' IN <ls_statement>-str
            SUBMATCHES lv_source lv_target ##NO_TEXT.
        ENDIF.
        IF sy-subrc <> 0.
          FIND REGEX ' (\w+) INTO (\w+)' IN <ls_statement>-str
            SUBMATCHES lv_source lv_target ##NO_TEXT.
        ENDIF.

        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        IF <ls_next>-str = |DELETE { lv_source } FROM { lv_target }|.
          lv_code = '001'.
        ENDIF.
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
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
