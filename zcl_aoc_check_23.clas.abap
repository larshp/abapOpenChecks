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

  DATA: lv_code       TYPE sci_errc,
        lt_statements LIKE it_statements,
        lv_include    TYPE program.

  FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                 <ls_token>     LIKE LINE OF it_tokens.


  lt_statements = it_statements.

  LOOP AT lt_statements ASSIGNING <ls_statement>
      WHERE coloncol <> 0
      AND type <> scan_stmnt_type-pragma.

    CLEAR lv_code.

    READ TABLE it_tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    IF <ls_token>-str <> 'TYPES'
        AND <ls_token>-str <> 'DATA'
        AND <ls_token>-str <> 'CLASS-DATA'
        AND <ls_token>-str <> 'STATICS'
        AND <ls_token>-str <> 'WRITE'
        AND <ls_token>-str <> 'MOVE'  " anyhow obsolete
        AND <ls_token>-str <> 'RANGES' " anyhow obsolete
        AND <ls_token>-str <> 'METHODS'
        AND <ls_token>-str <> 'CLEAR'
        AND <ls_token>-str <> 'PERFORM'
        AND <ls_token>-str <> 'REFRESH'
        AND <ls_token>-str <> 'UNASSIGN'
        AND <ls_token>-str <> 'FREE'
        AND <ls_token>-str <> 'CONSTANTS'
        AND <ls_token>-str <> 'TABLES'
        AND <ls_token>-str <> 'PARAMETERS'
        AND <ls_token>-str <> 'PARAMETER'
        AND <ls_token>-str <> 'INTERFACES'
        AND <ls_token>-str <> 'SELECT-OPTIONS'
        AND <ls_token>-str <> 'SELECTION-SCREEN'
        AND <ls_token>-str <> 'ALIASES'
        AND <ls_token>-str <> 'INCLUDE'
        AND <ls_token>-str <> 'TYPE-POOLS'
        AND <ls_token>-str <> 'CLASS-METHODS'
        AND <ls_token>-str <> 'FIELD-SYMBOLS'.
      lv_code = '001'.
    ENDIF.

    IF lv_code IS INITIAL
          AND strlen( <ls_token>-str ) + <ls_token>-col <> <ls_statement>-coloncol
        AND <ls_token>-str <> 'PERFORM'.
      lv_code = '002'.
    ENDIF.

    IF lv_code IS INITIAL.
      LOOP AT it_tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from TO <ls_statement>-to
          WHERE row = <ls_statement>-colonrow.
        IF <ls_token>-col = <ls_statement>-coloncol + 1.
          lv_code = '003'.
          EXIT. " current loop
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF NOT lv_code IS INITIAL.
      lv_include = get_include( p_level = <ls_statement>-level ).

      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = lv_include
              p_line = <ls_token>-row
              p_kind = mv_errty
              p_test = myname
              p_code = lv_code ).

* do not report multiple errors for the same chanined statement
      DELETE lt_statements
        WHERE level = <ls_statement>-level
        AND colonrow = <ls_statement>-colonrow
        AND coloncol = <ls_statement>-coloncol.
    ENDIF.

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

  CLEAR p_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Use chained statements mainly for declarations'. "#EC NOTEXT
    WHEN '002'.
      p_text = 'Space before colon'.                        "#EC NOTEXT
    WHEN '003'.
      p_text = 'Missing space after colon'.                 "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 0 = 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT
ENDCLASS.