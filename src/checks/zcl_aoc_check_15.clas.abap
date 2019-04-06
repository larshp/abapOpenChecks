CLASS zcl_aoc_check_15 DEFINITION
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



CLASS ZCL_AOC_CHECK_15 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_position  TYPE i,
          lv_include   TYPE sobj_name,
          lv_statement TYPE string.

    FIELD-SYMBOLS: <ls_token>     LIKE LINE OF it_tokens,
                   <ls_statement> LIKE LINE OF it_statements.


    LOOP AT it_statements ASSIGNING <ls_statement>
        WHERE type <> scan_stmnt_type-empty
        AND type <> scan_stmnt_type-comment.

      lv_position = sy-tabix.
      CLEAR lv_statement.

      LOOP AT it_tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from TO <ls_statement>-to
          WHERE type <> scan_token_type-comment.
        IF lv_statement IS INITIAL.
          lv_statement = <ls_token>-str.
        ELSE.
          CONCATENATE lv_statement <ls_token>-str
            INTO lv_statement SEPARATED BY space.
        ENDIF.
      ENDLOOP.

      IF lv_statement CP 'CALL FUNCTION *'
          OR lv_statement CP 'CALL METHOD *'
          OR lv_statement CP 'CALL CUSTOMER-FUNCTION *'
          OR lv_statement CP 'CALL SCREEN *'
          OR lv_statement CP 'CALL SELECTION-SCREEN *'
          OR lv_statement CP 'CALL TRANSACTION *'
          OR lv_statement CP 'CALL TRANSFORMATION *'
          OR lv_statement CP 'CALL BADI *'.
        CONTINUE.
      ELSEIF lv_statement CP 'CALL *'
          OR lv_statement CP 'SYSTEM-CALL *'.
        lv_include = get_include( p_level = <ls_statement>-level ).
        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = lv_include
                p_position     = lv_position
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
    position       = '015'.

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
        p_text = 'Kernel CALL'.                             "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.                    "GET_MESSAGE_TEXT
ENDCLASS.
