class ZCL_AOC_CHECK_51 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.

  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
protected section.

  class-data GV_RUN type ABAP_BOOL .
  class-data GV_SUPPORTED type ABAP_BOOL .

  methods SUPPORTED
    returning
      value(RV_SUPPORTED) type ABAP_BOOL .
private section.
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
        it_statements = it_statements
        it_levels     = it_levels ).

    LOOP AT lt_statements ASSIGNING <ls_statement>.
      CLEAR lv_code.

      IF <ls_statement>-str CP 'SELECT*INTO +*'
          AND NOT <ls_statement>-str CP 'SELECT*INTO @*'.
        lv_code = '001'.
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

    description    = 'Open SQL - Escape host variables'.    "#EC NOTEXT
    category       = 'ZCL_AOC_CATEGORY'.
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