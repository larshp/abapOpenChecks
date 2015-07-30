class ZCL_AOC_CHECK_37 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.

*"* public components of class ZCL_AOC_CHECK_37
*"* do not include other source files here!!!
  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_AOC_CHECK_37 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lv_statement TYPE string,
        lv_include   TYPE sobj_name.

  FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                 <ls_token>     LIKE LINE OF it_tokens.


  LOOP AT it_statements ASSIGNING <ls_statement>.

    CLEAR lv_statement.
    LOOP AT it_tokens ASSIGNING <ls_token>
        FROM <ls_statement>-from TO <ls_statement>-to.
      IF lv_statement IS INITIAL.
        lv_statement = <ls_token>-str.
      ELSE.
        CONCATENATE lv_statement <ls_token>-str
          INTO lv_statement SEPARATED BY space.
      ENDIF.
    ENDLOOP.

    IF lv_statement CP 'MESSAGE ''*'
        OR lv_statement CP 'MESSAGE text-+++ *'.
      lv_include = get_include( p_level = <ls_statement>-level ).

      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = lv_include
              p_line         = <ls_token>-row
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '001' ).
    ENDIF.

  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Define message texts in SE91'.          "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '001'.
  position       = '037'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Define message texts in SE91'.              "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT
ENDCLASS.