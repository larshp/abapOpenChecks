class ZCL_AOC_CHECK_45 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.

*"* public components of class ZCL_AOC_CHECK_45
*"* do not include other source files here!!!
  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_ATTRIBUTES
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
  methods PUT_ATTRIBUTES
    redefinition .
  methods IF_CI_TEST~QUERY_ATTRIBUTES
    redefinition .
protected section.

  types:
    BEGIN OF ty_statement,
      str     TYPE string,
      row     TYPE token_row,
      include TYPE programm,
      count   TYPE i,
    END OF ty_statement .
  types:
    ty_statements TYPE STANDARD TABLE OF ty_statement WITH DEFAULT KEY .

  type-pools ABAP .
  methods CHECK_NEW
    returning
      value(RV_SUPPORTED) type ABAP_BOOL .
  methods BUILD_STATEMENTS
    importing
      !IT_TOKENS type STOKESX_TAB
      !IT_STATEMENTS type SSTMNT_TAB
      !IT_LEVELS type SLEVEL_TAB
    returning
      value(RT_STATEMENTS) type TY_STATEMENTS .
*"* protected components of class ZCL_AOC_CHECK_45
*"* do not include other source files here!!!
private section.

  data MV_LINES type FLAG .
  data MV_NEW type FLAG .
*"* private components of class ZCL_AOC_CHECK_45
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_AOC_CHECK_45 IMPLEMENTATION.


METHOD build_statements.

  DATA: lv_str   TYPE string,
        lv_count TYPE i.

  FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                 <ls_token>     LIKE LINE OF it_tokens,
                 <ls_add>       LIKE LINE OF rt_statements.


  LOOP AT it_statements ASSIGNING <ls_statement>
      WHERE type <> scan_stmnt_type-empty
      AND type <> scan_stmnt_type-comment
      AND type <> scan_stmnt_type-comment_in_stmnt
      AND type <> scan_stmnt_type-pragma.

    CLEAR lv_str.
    lv_count = 0.

    LOOP AT it_tokens ASSIGNING <ls_token>
        FROM <ls_statement>-from TO <ls_statement>-to.
      IF lv_str IS INITIAL.
        lv_str = <ls_token>-str.
      ELSE.
        CONCATENATE lv_str <ls_token>-str INTO lv_str SEPARATED BY space.
      ENDIF.
      lv_count = lv_count + 1.
    ENDLOOP.
    IF sy-subrc = 0.
      APPEND INITIAL LINE TO rt_statements ASSIGNING <ls_add>.
      <ls_add>-str = lv_str.
      <ls_add>-include = get_include( p_level = <ls_statement>-level ).
      <ls_add>-row = <ls_token>-row.
      <ls_add>-count = lv_count.
    ENDIF.

  ENDLOOP.

ENDMETHOD.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lt_statements TYPE ty_statements,
        lv_new        TYPE abap_bool,
        lv_code       TYPE sci_errc.

  FIELD-SYMBOLS: <ls_statement> LIKE LINE OF lt_statements.


  lv_new = check_new( ).

  lt_statements = build_statements(
      it_tokens     = it_tokens
      it_statements = it_statements
      it_levels     = it_levels ).

  LOOP AT lt_statements ASSIGNING <ls_statement>.
    CLEAR lv_code.

    IF <ls_statement>-str CP 'DESCRIBE TABLE *'
        AND <ls_statement>-count = 3
        AND mv_lines = abap_true.
      lv_code = '001'.
    ELSEIF <ls_statement>-str CP 'DESCRIBE TABLE * LINES *'
        AND mv_lines = abap_true.
      lv_code = '001'.
    ELSEIF <ls_statement>-str CP 'CREATE OBJECT *'
        AND mv_new = abap_true
        AND lv_new = abap_true.
      lv_code = '002'.
    ENDIF.

* todo, add READ TABLE?
*           IF can be changed to: boolc()
*           CONCATENATE -> string templates

    IF NOT lv_code IS INITIAL.
      inform( p_sub_obj_type = c_type_include
          p_sub_obj_name = <ls_statement>-include
          p_line         = <ls_statement>-row
          p_kind         = mv_errty
          p_test         = myname
          p_code         = lv_code ).
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD check_new.

  DATA: lt_itab  TYPE STANDARD TABLE OF string,
        lv_mess  TYPE string,
        lv_lin   TYPE i,
        ls_trdir TYPE trdir,
        lv_code  TYPE string,
        lv_wrd   TYPE string.


  lv_code = 'REPORT zfoobar.'.
  APPEND lv_code TO lt_itab.
  lv_code = 'DATA(lo_new) = NEW cl_gui_frontend_services( ).'.
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

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Use expressions'.                       "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '001'.
  position       = '045'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

  mv_lines = abap_true.
  mv_new = abap_true.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_attributes.

  EXPORT
    mv_lines = mv_lines
    mv_new = mv_new
    TO DATA BUFFER p_attributes.

ENDMETHOD.


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Use lines( ) expression'.                   "#EC NOTEXT
    WHEN '002'.
      p_text = 'Use NEW abc( ) expression'.                 "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.


METHOD if_ci_test~query_attributes.

  zzaoc_top.

  zzaoc_fill_att mv_errty 'Error Type' ''.                  "#EC NOTEXT
  zzaoc_fill_att mv_lines 'lines( )' ''.                    "#EC NOTEXT
  zzaoc_fill_att mv_new 'NEW' ''.                           "#EC NOTEXT

  zzaoc_popup.

ENDMETHOD.


METHOD put_attributes.

  IMPORT
    mv_lines = mv_lines
    mv_new = mv_new
    FROM DATA BUFFER p_attributes.                   "#EC CI_USE_WANTED
  ASSERT sy-subrc = 0.

ENDMETHOD.
ENDCLASS.