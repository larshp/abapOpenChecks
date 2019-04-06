CLASS zcl_aoc_check_11 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS check
        REDEFINITION.
    METHODS get_attributes
        REDEFINITION.
    METHODS get_message_text
        REDEFINITION.
    METHODS if_ci_test~query_attributes
        REDEFINITION.
    METHODS put_attributes
        REDEFINITION.
  PROTECTED SECTION.

    DATA mv_skipc TYPE flag.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_11 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_position          TYPE i,
          lv_include           TYPE program,
          lv_prev_row          TYPE token_row,
          lv_prev_level        TYPE stmnt_levl,
          lv_prev_inform_row   TYPE token_row,
          lv_prev_inform_level TYPE stmnt_levl.

    FIELD-SYMBOLS: <ls_statement>  LIKE LINE OF it_statements,
                   <ls_token_to>   LIKE LINE OF it_tokens,
                   <ls_level>      LIKE LINE OF it_levels,
                   <ls_token_from> LIKE LINE OF it_tokens.


    LOOP AT it_statements ASSIGNING <ls_statement>
        WHERE terminator = '.'
        AND type <> scan_stmnt_type-pragma.

      lv_position = sy-tabix.

      IF <ls_statement>-from > <ls_statement>-to.
        CONTINUE.
      ENDIF.

      READ TABLE it_tokens ASSIGNING <ls_token_to> INDEX <ls_statement>-to.
      CHECK sy-subrc = 0.

      READ TABLE it_tokens ASSIGNING <ls_token_from> INDEX <ls_statement>-from.
      CHECK sy-subrc = 0.

      IF <ls_statement>-level = lv_prev_level AND <ls_token_from>-row = lv_prev_row.
        READ TABLE it_levels ASSIGNING <ls_level> INDEX <ls_statement>-level.
        IF sy-subrc = 0 AND ( <ls_level>-type = scan_level_type-macro_define
            OR <ls_level>-type = scan_level_type-macro_trmac ).
          CONTINUE.
        ENDIF.

        lv_include = get_include( p_level = <ls_statement>-level ).
        IF mv_skipc = abap_true
            AND is_class_definition( lv_include ) = abap_true.
          CONTINUE. " current loop
        ENDIF.
        IF lv_prev_inform_row <> <ls_token_from>-row
            OR lv_prev_inform_level <> <ls_statement>-level.
          inform( p_sub_obj_type = c_type_include
                  p_sub_obj_name = lv_include
                  p_position     = lv_position
                  p_line         = <ls_token_from>-row
                  p_kind         = mv_errty
                  p_test         = myname
                  p_code         = '001' ).
        ENDIF.
        lv_prev_inform_row   = <ls_token_from>-row.
        lv_prev_inform_level = <ls_statement>-level.
      ENDIF.

      lv_prev_row   = <ls_token_to>-row.
      lv_prev_level = <ls_statement>-level.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '002'.
    position       = '011'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).
    set_uses_checksum( ).

    mv_errty = c_error.
    mv_skipc = abap_true.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mv_skipc = mv_skipc
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Max one statement per line'.              "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.                    "GET_MESSAGE_TEXT


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_skipc 'Skip global class definitions' 'C'. "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mv_skipc = mv_skipc
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
