class ZCL_AOC_CHECK_34 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.

*"* public components of class ZCL_AOC_CHECK_34
*"* do not include other source files here!!!
  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_ATTRIBUTES
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
  methods IF_CI_TEST~QUERY_ATTRIBUTES
    redefinition .
  methods PUT_ATTRIBUTES
    redefinition .
protected section.

  data MV_LINES type I .
private section.
ENDCLASS.



CLASS ZCL_AOC_CHECK_34 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DEFINE _check.
    IF lv_start > 0 AND lv_start + mv_lines < <ls_token>-row.
      lv_include = get_include( p_level = <ls_statement>-level ).
      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = lv_include
              p_line         = lv_start
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '001' ).
    ENDIF.
  END-OF-DEFINITION.

  DATA: lv_start    TYPE i,
        lv_include  TYPE sobj_name.

  FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                 <ls_token>     LIKE LINE OF it_tokens.


  LOOP AT it_statements ASSIGNING <ls_statement>
      WHERE type = scan_stmnt_type-standard.

    READ TABLE it_tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
    CASE <ls_token>-str.
      WHEN 'WHEN'.
        _check.
        lv_start = <ls_token>-row.
      WHEN 'ENDCASE'.
        _check.
        lv_start = 0.
    ENDCASE.

  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Large WHEN construct'.                  "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '001'.
  position       = '034'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.
  mv_lines = 20.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_attributes.

  EXPORT
    mv_errty = mv_errty
    mv_lines = mv_lines
    TO DATA BUFFER p_attributes.

ENDMETHOD.


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Large WHEN construct'.                      "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT


METHOD if_ci_test~query_attributes.

  DATA: lt_attributes TYPE sci_atttab,
        ls_attribute  LIKE LINE OF lt_attributes.

  DEFINE fill_att.
    get reference of &1 into ls_attribute-ref.
    ls_attribute-text = &2.
    ls_attribute-kind = &3.
    append ls_attribute to lt_attributes.
  END-OF-DEFINITION.


  fill_att mv_errty 'Error Type' ''.                        "#EC NOTEXT
  fill_att mv_lines 'Lines' ''.                             "#EC NOTEXT

  cl_ci_query_attributes=>generic(
                        p_name       = myname
                        p_title      = 'Options'
                        p_attributes = lt_attributes
                        p_display    = p_display ).         "#EC NOTEXT
  IF mv_errty = c_error OR mv_errty = c_warning OR mv_errty = c_note.
    attributes_ok = abap_true.
  ELSE.
    attributes_ok = abap_false.
  ENDIF.

ENDMETHOD.


METHOD put_attributes.

  IMPORT
    mv_errty = mv_errty
    mv_lines = mv_lines
    FROM DATA BUFFER p_attributes.                   "#EC CI_USE_WANTED

ENDMETHOD.
ENDCLASS.