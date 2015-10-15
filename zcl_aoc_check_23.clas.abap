class ZCL_AOC_CHECK_23 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.

*"* public components of class ZCL_AOC_CHECK_23
*"* do not include other source files here!!!
  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
protected section.
*"* protected components of class ZCL_AOC_CHECK_23
*"* do not include other source files here!!!
private section.
ENDCLASS.



CLASS ZCL_AOC_CHECK_23 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lv_include TYPE program.

  FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                 <ls_token>     LIKE LINE OF it_tokens.


  LOOP AT it_statements ASSIGNING <ls_statement>
      WHERE coloncol <> 0
      AND type <> scan_stmnt_type-pragma.

    READ TABLE it_tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    IF <ls_token>-str = 'TYPES'
        OR <ls_token>-str = 'DATA'
        OR <ls_token>-str = 'CLASS-DATA'
        OR <ls_token>-str = 'STATICS'
        OR <ls_token>-str = 'WRITE'
        OR <ls_token>-str = 'MOVE'  " is obsolete anyhow
        OR <ls_token>-str = 'METHODS'
        OR <ls_token>-str = 'CLEAR'
        OR <ls_token>-str = 'PERFORM'
        OR <ls_token>-str = 'REFRESH'
        OR <ls_token>-str = 'UNASSIGN'
        OR <ls_token>-str = 'FREE'
        OR <ls_token>-str = 'CONSTANTS'
        OR <ls_token>-str = 'TABLES'
        OR <ls_token>-str = 'PARAMETERS'
        OR <ls_token>-str = 'INTERFACES'
        OR <ls_token>-str = 'SELECT-OPTIONS'
        OR <ls_token>-str = 'CLASS-METHODS'
        OR <ls_token>-str = 'FIELD-SYMBOLS'.
      CONTINUE.
    ENDIF.

    lv_include = get_include( p_level = <ls_statement>-level ).

    inform( p_sub_obj_type = c_type_include
            p_sub_obj_name = lv_include
            p_line = <ls_token>-row
            p_kind = mv_errty
            p_test = myname
            p_code = '001' ).

  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Chained Statements'.                    "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '001'.
  position       = '023'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Use chained statements mainly for declarations'. "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT
ENDCLASS.