CLASS zcl_aoc_check_07 DEFINITION
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



CLASS ZCL_AOC_CHECK_07 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_position TYPE i,
          lv_token    LIKE sy-tabix,
          lv_include  TYPE sobj_name.

    FIELD-SYMBOLS: <ls_token>     LIKE LINE OF io_scan->tokens,
                   <ls_token1>    LIKE LINE OF io_scan->tokens,
                   <ls_token2>    LIKE LINE OF io_scan->tokens,
                   <ls_token3>    LIKE LINE OF io_scan->tokens,
                   <ls_statement> LIKE LINE OF io_scan->statements.


    LOOP AT io_scan->statements ASSIGNING <ls_statement>
        WHERE type = scan_stmnt_type-standard
        OR type = scan_stmnt_type-method_direct.

      lv_position = sy-tabix.

      lv_token = <ls_statement>-from.
      READ TABLE io_scan->tokens ASSIGNING <ls_token1> INDEX lv_token.
      IF sy-subrc <> 0.
        CONTINUE. " current loop
      ENDIF.
      lv_token = lv_token + 1.
      READ TABLE io_scan->tokens ASSIGNING <ls_token2> INDEX lv_token.
      IF sy-subrc <> 0.
        CONTINUE. " current loop
      ENDIF.
      lv_token = lv_token + 1.
      READ TABLE io_scan->tokens ASSIGNING <ls_token3> INDEX lv_token.
      IF sy-subrc <> 0.
        CONTINUE. " current loop
      ENDIF.

      IF <ls_token3>-str CP '*>(*'
          OR <ls_token3>-str CP '*)=>*'
          OR <ls_token3>-str CP '*)->*'
          OR <ls_token3>-str CP '(*'
          OR <ls_token3>-str = 'OF'.
* allow dynamic calls and OLE interaction
        CONTINUE.
      ENDIF.

      LOOP AT io_scan->tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from TO <ls_statement>-to
          WHERE str = 'RECEIVING'
          AND type = scan_token_type-identifier.

        IF <ls_token1>-str = 'CALL' OR <ls_token2>-str = 'BADI'.
          EXIT.
        ENDIF.

* allow if old style exceptions are part of method
        LOOP AT io_scan->tokens TRANSPORTING NO FIELDS
            FROM <ls_statement>-from TO <ls_statement>-to
            WHERE str = 'EXCEPTIONS'
            AND type = scan_token_type-identifier.
          EXIT.
        ENDLOOP.
        IF sy-subrc = 0.
          EXIT. " current loop
        ENDIF.

        lv_include = get_include( p_level = <ls_statement>-level ).
        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = lv_include
                p_position     = lv_position
                p_line         = <ls_token>-row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = '002' ).
      ENDLOOP.

      IF <ls_token1>-str <> 'CALL' OR <ls_token2>-str <> 'METHOD'.
        CONTINUE. " current loop
      ENDIF.

      lv_include = get_include( p_level = <ls_statement>-level ).
      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = lv_include
              p_position     = lv_position
              p_line         = <ls_token1>-row
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '001' ).

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '007'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).
    set_uses_checksum( ).

    mv_errty = c_error.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Use functional writing style'.            "#EC NOTEXT
      WHEN '002'.
        p_text = 'Use functional writing style instead of RECEIVING'. "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.                    "GET_MESSAGE_TEXT
ENDCLASS.
