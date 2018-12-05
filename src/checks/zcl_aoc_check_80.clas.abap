CLASS zcl_aoc_check_80 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
    METHODS get_message_text
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS ZCL_AOC_CHECK_80 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_code   TYPE string_table,
          lv_offset TYPE i,
          lv_length TYPE i,
          lv_level  TYPE i,
          lv_report TYPE abap_bool,
          lv_line   TYPE token_row.

    FIELD-SYMBOLS: <ls_level>     LIKE LINE OF it_levels,
                   <ls_token>     LIKE LINE OF it_tokens,
                   <ls_statement> LIKE LINE OF it_statements,
                   <lv_code>      LIKE LINE OF lt_code.


    LOOP AT it_levels ASSIGNING <ls_level>.
      lv_level = sy-tabix.
      lt_code = get_source( <ls_level> ).

      LOOP AT lt_code ASSIGNING <lv_code>.
        lv_line = sy-tabix.

        FIND FIRST OCCURRENCE OF ` : ` IN <lv_code>
          MATCH OFFSET lv_offset
          MATCH LENGTH lv_length.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        lv_report = abap_true.
        LOOP AT it_statements ASSIGNING <ls_statement> WHERE level = lv_level.
          LOOP AT it_tokens ASSIGNING <ls_token> FROM <ls_statement>-from TO <ls_statement>-to
              WHERE row = lv_line AND type CA 'SC'
              AND col <= lv_offset.
            IF <ls_token>-col + <ls_token>-len1 >= lv_offset + lv_length.
              lv_report = abap_false.
              EXIT.
            ENDIF.
          ENDLOOP.
        ENDLOOP.

        IF lv_report = abap_true.
          inform( p_sub_obj_type = c_type_include
                  p_sub_obj_name = <ls_level>-name
                  p_line         = lv_line
                  p_kind         = mv_errty
                  p_test         = myname
                  p_code         = '001' ).
        ENDIF.

      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    category = 'ZCL_AOC_CATEGORY'.
    version  = '001'.
    position = '080'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Space before colon'.                      "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
