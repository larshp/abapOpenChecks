class ZCL_AOC_CHECK_38 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.

*"* public components of class ZCL_AOC_CHECK_38
*"* do not include other source files here!!!
  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_AOC_CHECK_38 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lv_include   TYPE sobj_name.

  FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                 <ls_token>     LIKE LINE OF it_tokens.


  LOOP AT it_statements ASSIGNING <ls_statement>.

    READ TABLE it_tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    IF <ls_token>-str = 'ENDSELECT'.
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

  description    = 'Avoid use of SELECT-ENDSELECT'.         "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '001'.
  position       = '038'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_message_text.

  CLEAR p_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Avoid use of SELECT-ENDSELECT'.             "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 0 = 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT
ENDCLASS.