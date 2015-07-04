class ZCL_AOC_CHECK_08 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.

*"* public components of class ZCL_AOC_CHECK_08
*"* do not include other source files here!!!
  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
protected section.
*"* protected components of class ZCL_AOC_CHECK_08
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_AOC_CHECK_08
*"* do not include other source files here!!!

  constants C_MY_NAME type SEOCLSNAME value 'ZCL_AOC_CHECK_08'. "#EC NOTEXT
ENDCLASS.



CLASS ZCL_AOC_CHECK_08 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lv_include   TYPE sobj_name,
        lv_code      TYPE sci_errc,
        lv_statement TYPE string.

  FIELD-SYMBOLS: <ls_token>     LIKE LINE OF it_tokens,
                 <ls_statement> LIKE LINE OF it_statements.


  LOOP AT it_statements ASSIGNING <ls_statement>.

    CLEAR lv_statement.

    LOOP AT it_tokens ASSIGNING <ls_token>
        FROM <ls_statement>-from TO <ls_statement>-to
        WHERE type = scan_token_type-identifier.
      IF lv_statement IS INITIAL.
        lv_statement = <ls_token>-str.
      ELSE.
        CONCATENATE lv_statement <ls_token>-str INTO lv_statement SEPARATED BY space.
      ENDIF.
    ENDLOOP.

    CLEAR lv_code.

    IF lv_statement CP 'REFRESH *'.
      lv_code = '001'.
    ELSEIF lv_statement CP '*IS REQUESTED*'.
      lv_code = '002'.
    ELSEIF lv_statement = 'LEAVE'.
      lv_code = '003'.
    ELSEIF lv_statement CP 'COMPUTE *'.
      lv_code = '004'.
    ELSEIF lv_statement CP 'MOVE *'.
      lv_code = '005'.
    ELSEIF lv_statement CP '* >< *'
        OR lv_statement CP '* =< *'
        OR lv_statement CP '* => *'.
      lv_code = '006'.
    ELSEIF lv_statement CP '* EQ *'
        OR lv_statement CP '* NE *'
        OR lv_statement CP '* LT *'
        OR lv_statement CP '* GT *'
        OR lv_statement CP '* LE *'
        OR lv_statement CP '* GE *'.
      lv_code = '007'.
    ELSEIF lv_statement CP 'DEMAND *'.
      lv_code = '008'.
    ELSEIF lv_statement CP 'SUPPLY *'.
      lv_code = '009'.
    ELSEIF lv_statement CP 'CONTEXTS *'.
      lv_code = '010'.
    ENDIF.

    IF NOT lv_code IS INITIAL.
      lv_include = get_include( p_level = <ls_statement>-level ).
      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = lv_include
              p_line         = <ls_token>-row
              p_kind         = mv_errty
              p_test         = c_my_name
              p_code         = lv_code ).
    ENDIF.

  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Obsolete statement'.                    "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'REFRESH is obsolete'.                       "#EC NOTEXT
    WHEN '002'.
      p_text = 'IS REQUESTED is obsolete'.                  "#EC NOTEXT
    WHEN '003'.
      p_text = 'LEAVE is obsolete'.                         "#EC NOTEXT
    WHEN '004'.
      p_text = 'COMPUTE is obsolete'.                       "#EC NOTEXT
    WHEN '005'.
      p_text = 'MOVE is obsolete'.                          "#EC NOTEXT
    WHEN '006'.
      p_text = 'Obsolete operator'.                         "#EC NOTEXT
    WHEN '007'.
      p_text = 'Use new operator'.                          "#EC NOTEXT
    WHEN '008'.
      p_text = 'DEMAND is obsolete'.                        "#EC NOTEXT
    WHEN '009'.
      p_text = 'SUPPLY is obsolete'.                        "#EC NOTEXT
    WHEN '010'.
      p_text = 'CONTEXTS is obsolete'.                      "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT
ENDCLASS.