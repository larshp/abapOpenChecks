CLASS zcl_aoc_check_51 DEFINITION PUBLIC INHERITING FROM zcl_aoc_super CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS check
         REDEFINITION.
    METHODS get_message_text
         REDEFINITION.

  PROTECTED SECTION.

    CLASS-DATA gv_run TYPE abap_bool.
    CLASS-DATA gv_supported TYPE abap_bool.

    METHODS supported
      RETURNING
        VALUE(rv_supported) TYPE abap_bool.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_51 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_statements TYPE ty_statements,
          lv_code       TYPE sci_errc.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF lt_statements.


    IF supported( ) = abap_false.
      RETURN.
    ENDIF.

    lt_statements = build_statements(
        it_tokens     = it_tokens
        it_statements = it_statements ).

    LOOP AT lt_statements ASSIGNING <ls_statement>.
      CLEAR lv_code.

      IF <ls_statement>-str CP 'SELECT *'.
        IF <ls_statement>-str CP '* INTO ( @*'
            OR <ls_statement>-str CP '* INTO (@*'
            OR <ls_statement>-str CP '* INTO @*'
            OR <ls_statement>-str CP '* INTO TABLE @*'
            OR <ls_statement>-str CP '* INTO CORRESPONDING FIELDS OF @*'
            OR <ls_statement>-str CP 'SELECT COUNT( #* ) FROM *'
            OR <ls_statement>-str CP '* INTO CORRESPONDING FIELDS OF TABLE @*'
            OR <ls_statement>-str CP '* APPENDING TABLE @*'
            OR <ls_statement>-str CP '* APPENDING CORRESPONDING FIELDS OF TABLE @*'.
          CONTINUE.
        ELSE.
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

    version        = '001'.
    position       = '051'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Escape host variables'.                   "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.


  METHOD supported.

    DATA: lt_itab  TYPE STANDARD TABLE OF string,
          lv_mess  TYPE string,
          lv_lin   TYPE i,
          ls_trdir TYPE trdir,
          lv_code  TYPE string,
          lv_wrd   TYPE string.


    IF gv_run = abap_true.
      rv_supported = gv_supported.
      RETURN.
    ENDIF.

    lv_code = 'REPORT zfoobar.' ##NO_TEXT.
    APPEND lv_code TO lt_itab.
    lv_code = 'DATA lv_bname TYPE usr02-bname.' ##NO_TEXT.
    APPEND lv_code TO lt_itab.
    lv_code = 'SELECT SINGLE bname FROM usr02 INTO @lv_bname.' ##NO_TEXT.
    APPEND lv_code TO lt_itab.

    ls_trdir-uccheck = abap_true.
    ls_trdir-fixpt   = abap_true.

    SYNTAX-CHECK FOR lt_itab
      MESSAGE lv_mess
      LINE lv_lin
      WORD lv_wrd
      DIRECTORY ENTRY ls_trdir.
    IF sy-subrc = 0.
      rv_supported = abap_true.
    ELSE.
      rv_supported = abap_false.
    ENDIF.

    gv_supported = rv_supported.
    gv_run = abap_true.

  ENDMETHOD.
ENDCLASS.
