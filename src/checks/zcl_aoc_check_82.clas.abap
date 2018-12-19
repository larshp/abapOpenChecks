CLASS zcl_aoc_check_82 DEFINITION
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



CLASS ZCL_AOC_CHECK_82 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_level TYPE i.

    FIELD-SYMBOLS: <ls_level>     LIKE LINE OF it_levels,
                   <ls_token>     LIKE LINE OF it_tokens,
                   <ls_statement> LIKE LINE OF it_statements.


    LOOP AT it_levels ASSIGNING <ls_level>.
      lv_level = sy-tabix.
      LOOP AT it_statements ASSIGNING <ls_statement> WHERE level = lv_level
          AND type = scan_stmnt_type-comment_in_stmnt.
        LOOP AT it_tokens ASSIGNING <ls_token> FROM <ls_statement>-from TO <ls_statement>-to.
          IF strlen( <ls_token>-str ) > 4 AND <ls_token>-str(4) = '"#EC'.
            inform( p_sub_obj_type = c_type_include
                    p_sub_obj_name = <ls_level>-name
                    p_line         = <ls_token>-row
                    p_column       = <ls_token>-col + <ls_token>-len1
                    p_kind         = mv_errty
                    p_test         = myname
                    p_code         = '001' ).
          ENDIF.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    category = 'ZCL_AOC_CATEGORY'.
    version  = '001'.
    position = '082'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Pseudo comment placement'.                "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
