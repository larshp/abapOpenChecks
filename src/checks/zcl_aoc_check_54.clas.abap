CLASS zcl_aoc_check_54 DEFINITION PUBLIC INHERITING FROM zcl_aoc_super CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
      REDEFINITION .
    METHODS get_message_text
      REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_54 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_statements TYPE zcl_aoc_scan=>ty_statements,
          lv_code       TYPE sci_errc.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF lt_statements.


    lt_statements = io_scan->build_statements( ).

    LOOP AT lt_statements ASSIGNING <ls_statement>.
      CLEAR lv_code.

      IF <ls_statement>-str NP 'CALL TRANSACTION *'.
        CONTINUE.
      ELSEIF <ls_statement>-str NP '* AUTHORITY-CHECK*'.
        lv_code = '001'.
      ENDIF.

      IF NOT lv_code IS INITIAL.
        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = <ls_statement>-include
                p_position     = <ls_statement>-index
                p_line         = <ls_statement>-start-row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = lv_code ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '054'.

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
        p_text = 'Specify AUTHORITY-CHECK'.                 "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
