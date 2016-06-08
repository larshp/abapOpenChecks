class ZCL_AOC_CHECK_11 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.

*"* public components of class ZCL_AOC_CHECK_11
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

  data MV_SKIPC type FLAG .
*"* protected components of class ZCL_AOC_CHECK_11
*"* do not include other source files here!!!
private section.
ENDCLASS.



CLASS ZCL_AOC_CHECK_11 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lv_include    TYPE program,
        lv_prev_row   TYPE token_row,
        lv_prev_level TYPE stmnt_levl.

  FIELD-SYMBOLS: <ls_statement>  LIKE LINE OF it_statements,
                 <ls_token_to>   LIKE LINE OF it_tokens,
                 <ls_level>      LIKE LINE OF it_levels,
                 <ls_token_from> LIKE LINE OF it_tokens.


  LOOP AT it_statements ASSIGNING <ls_statement>
      WHERE terminator = '.'
      AND type <> scan_stmnt_type-pragma.

    IF <ls_statement>-from > <ls_statement>-to.
      CONTINUE.
    ENDIF.

    READ TABLE it_tokens ASSIGNING <ls_token_to> INDEX <ls_statement>-to.
    CHECK sy-subrc = 0.

    READ TABLE it_tokens ASSIGNING <ls_token_from> INDEX <ls_statement>-from.
    CHECK sy-subrc = 0.

    IF <ls_statement>-level = lv_prev_level AND <ls_token_from>-row = lv_prev_row.
      READ TABLE it_levels ASSIGNING <ls_level> INDEX <ls_statement>-level.
      IF sy-subrc = 0 AND ( <ls_level>-type = scan_level_type-macro_define
          OR <ls_level>-type = scan_level_type-macro_trmac ).
        CONTINUE.
      ENDIF.

      lv_include = get_include( p_level = <ls_statement>-level ).
      IF mv_skipc = abap_true
          AND is_class_definition( lv_include ) = abap_true.
        CONTINUE. " current loop
      ENDIF.
      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = lv_include
              p_line         = <ls_token_from>-row
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '001' ).
    ENDIF.

    lv_prev_row   = <ls_token_to>-row.
    lv_prev_level = <ls_statement>-level.

  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Max one statement per line'.            "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '002'.
  position       = '011'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.
  mv_skipc = abap_true.

ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mv_skipc = mv_skipc
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


METHOD get_message_text.

  CLEAR p_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Max one statement per line'.                "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 0 = 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_skipc 'Skip global class definitions' 'C'. "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mv_skipc = mv_skipc
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.