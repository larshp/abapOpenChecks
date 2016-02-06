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

  methods REPORT
    importing
      !IS_STATEMENT type SSTMNT
      !IS_TOKEN type STOKESX
      !IV_CODE type SCI_ERRC .
ENDCLASS.



CLASS ZCL_AOC_CHECK_37 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lv_number    TYPE t100-msgnr,
        lv_class     TYPE t100-arbgb,
        lv_text      TYPE t100-text,
        lv_trash     TYPE string ##NEEDED,
        lv_statement TYPE string.

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
      report( is_statement = <ls_statement>
              is_token     = <ls_token>
              iv_code      = '001' ).
    ENDIF.

    IF lv_statement CP 'MESSAGE *->GET_TEXT( ) *'
        OR lv_statement CP 'MESSAGE *->IF_MESSAGE~GET_TEXT( ) *'.
      report( is_statement = <ls_statement>
              is_token     = <ls_token>
              iv_code      = '002' ).
    ENDIF.

    IF lv_statement CP 'MESSAGE ++++(*) *'.
      lv_number = lv_statement+9(3).
      lv_class = lv_statement+13.
      SPLIT lv_class AT ')' INTO lv_class lv_trash.
      SELECT SINGLE text FROM t100
        INTO lv_text
        WHERE sprsl = sy-langu
        AND arbgb = lv_class
        AND msgnr = lv_number.
      IF sy-subrc = 0 AND lv_text CO '&12345678 '.
        report( is_statement = <ls_statement>
                is_token     = <ls_token>
                iv_code      = '003' ).
      ENDIF.
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

  CLEAR p_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Define message texts in SE91'.              "#EC NOTEXT
    WHEN '002'.
      p_text = 'Remove ''->get_text( )'''.                  "#EC NOTEXT
    WHEN '003'.
      p_text = 'Use of generic SE91 message'.               "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 0 = 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT


METHOD report.

  DATA: lv_include TYPE sobj_name.


  lv_include = get_include( p_level = is_statement-level ).

  inform( p_sub_obj_type = c_type_include
          p_sub_obj_name = lv_include
          p_line         = is_token-row
          p_kind         = mv_errty
          p_test         = myname
          p_code         = iv_code ).

ENDMETHOD.
ENDCLASS.