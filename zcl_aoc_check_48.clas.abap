class ZCL_AOC_CHECK_48 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.

*"* public components of class ZCL_AOC_CHECK_48
*"* do not include other source files here!!!
  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
protected section.

  methods SUPPORT_EMPTY_KEY
    returning
      value(RV_SUPPORTED) type ABAP_BOOL .
*"* protected components of class ZCL_AOC_CHECK_48
*"* do not include other source files here!!!
private section.

  class-data GV_CHECKED type ABAP_BOOL .
  class-data GV_SUPPORTED type ABAP_BOOL .
*"* private components of class ZCL_AOC_CHECK_48
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_AOC_CHECK_48 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lt_statements TYPE ty_statements.

  FIELD-SYMBOLS: <ls_statement> LIKE LINE OF lt_statements.


  lt_statements = build_statements(
      it_tokens     = it_tokens
      it_statements = it_statements
      it_levels     = it_levels ).

  LOOP AT lt_statements ASSIGNING <ls_statement>.
    IF ( <ls_statement>-str CP 'DATA* WITH DEFAULT KEY*'
        OR <ls_statement>-str CP 'TYPE* WITH DEFAULT KEY*' )
        AND support_empty_key( ) = abap_true.
      inform( p_sub_obj_type = c_type_include
          p_sub_obj_name = <ls_statement>-include
          p_line         = <ls_statement>-start-row
          p_kind         = mv_errty
          p_test         = myname
          p_code         = '001' ).
    ENDIF.

  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Table DEFAULT KEY'.                     "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '002'.
  position       = '048'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_message_text.

  CLEAR p_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Add table key or EMPTY KEY'.                "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 0 = 1.
  ENDCASE.

ENDMETHOD.


METHOD support_empty_key.

  DATA: lt_itab  TYPE STANDARD TABLE OF string,
        lv_mess  TYPE string,
        lv_lin   TYPE i,
        ls_trdir TYPE trdir,
        lv_code  TYPE string,
        lv_wrd   TYPE string.


  IF gv_checked = abap_true.
    rv_supported = gv_supported.
    RETURN.
  ENDIF.

  lv_code = 'REPORT zfoobar.' ##NO_TEXT.
  APPEND lv_code TO lt_itab.
  lv_code = 'TYPES: ty_table TYPE STANDARD TABLE OF usr02 WITH EMPTY KEY.' ##NO_TEXT.
  APPEND lv_code TO lt_itab.

  ls_trdir-uccheck = abap_true.

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
  gv_checked = abap_true.

ENDMETHOD.
ENDCLASS.