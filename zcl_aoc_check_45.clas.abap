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
    BEGIN OF ty_position,
           row TYPE token_row,
           col TYPE token_col,
         END OF ty_position .
  types:
    BEGIN OF ty_statement,
      str     TYPE string,
      start   TYPE ty_position,
      end     TYPE ty_position,
      include TYPE programm,
      count   TYPE i,
    END OF ty_statement .
  types:
    ty_statements TYPE STANDARD TABLE OF ty_statement WITH DEFAULT KEY .

  methods CHECK_LOOP
    importing
      !IS_STATEMENT type TY_STATEMENT
    returning
      value(RV_BOOL) type ABAP_BOOL .
  class-methods TOKEN_POSITION
    importing
      !IS_TOKEN type STOKESX
    returning
      value(RS_POSITION) type TY_POSITION .
  methods SUPPORT_INLINE_DECL
    returning
      value(RV_SUPPORTED) type ABAP_BOOL .
  methods SUPPORT_NEW
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

  class-data GV_NEW_RUN type ABAP_BOOL .
  class-data GV_NEW_SUPPORTED type ABAP_BOOL .
  data MV_LINES type FLAG .
  data MV_NEW type FLAG .
  data MV_LOOP_DECL type FLAG .
*"* private components of class ZCL_AOC_CHECK_45
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_AOC_CHECK_45 IMPLEMENTATION.


METHOD build_statements.

  DATA: lv_str   TYPE string,
        ls_start TYPE ty_position,
        ls_end   TYPE ty_position,
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
        ls_start = token_position( <ls_token> ).
      ELSE.
        CONCATENATE lv_str <ls_token>-str INTO lv_str SEPARATED BY space.
      ENDIF.
      lv_count = lv_count + 1.
      ls_end = token_position( <ls_token> ).
    ENDLOOP.
    IF sy-subrc = 0.
      APPEND INITIAL LINE TO rt_statements ASSIGNING <ls_add>.
      <ls_add>-str = lv_str.
      <ls_add>-include = get_include( p_level = <ls_statement>-level ).
      <ls_add>-start   = ls_start.
      <ls_add>-end     = ls_end.
      <ls_add>-count   = lv_count.
    ENDIF.

  ENDLOOP.

ENDMETHOD.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lt_statements TYPE ty_statements,
        lv_code       TYPE sci_errc.

  FIELD-SYMBOLS: <ls_statement> LIKE LINE OF lt_statements.


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
        AND support_new( ) = abap_true.
      lv_code = '002'.
    ELSEIF ( ( <ls_statement>-str CP 'LOOP AT * INTO *'
        AND NOT <ls_statement>-str CP 'LOOP AT * INTO DATA(*' )
        OR ( <ls_statement>-str CP 'LOOP AT * ASSIGNING *'
        AND NOT <ls_statement>-str CP 'LOOP AT * ASSIGNING FIELD-SYMBOL(*' ) )
        AND mv_loop_decl = abap_true
        AND support_inline_decl( ) = abap_true
        AND check_loop( <ls_statement> ) = abap_true.
      lv_code = '003'.
    ENDIF.

* todo, add READ TABLE?
*           IF can be changed to: boolc()
*           CONCATENATE -> string templates

    IF NOT lv_code IS INITIAL.
      inform( p_sub_obj_type = c_type_include
          p_sub_obj_name = <ls_statement>-include
          p_line         = <ls_statement>-start-row
          p_kind         = mv_errty
          p_test         = myname
          p_code         = lv_code ).
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD check_loop.

  CONSTANTS: lc_into      TYPE string VALUE 'INTO',
             lc_assigning TYPE string VALUE 'ASSIGNING'.

  DATA: lt_result   TYPE scr_refs,
        lt_str      TYPE TABLE OF string,
        lv_var      TYPE string.

  FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_result.


  lt_result = get_compiler( ).
  DELETE lt_result WHERE tag <> cl_abap_compiler=>tag_data.
  DELETE lt_result WHERE name = ''.

  SPLIT is_statement-str AT space INTO TABLE lt_str.
  READ TABLE lt_str FROM lc_into TRANSPORTING NO FIELDS.
  IF sy-subrc <> 0.
    READ TABLE lt_str FROM lc_assigning TRANSPORTING NO FIELDS.
  ENDIF.
  ASSERT sy-subrc = 0.
  sy-tabix = sy-tabix + 1.
  READ TABLE lt_str INDEX sy-tabix INTO lv_var.
  ASSERT sy-subrc = 0.

* this will make sure it is a local variable
  READ TABLE lt_result WITH KEY
    name = lv_var
    grade = cl_abap_compiler=>grade_definition
    mode2 = cl_abap_compiler=>mode2_def
    statement->source_info->name = is_statement-include
    TRANSPORTING NO FIELDS.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

* find the first use of the variable, this should be the LOOP
  READ TABLE lt_result ASSIGNING <ls_result> WITH KEY
    name = lv_var
    grade = cl_abap_compiler=>grade_direct
    statement->source_info->name = is_statement-include.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  IF <ls_result>-statement->start_line = is_statement-start-row.
    rv_bool = abap_true.
  ENDIF.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Use expressions'.                       "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '002'.
  position       = '045'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

  mv_lines     = abap_true.
  mv_new       = abap_true.
  mv_loop_decl = abap_true.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_attributes.

  EXPORT
    mv_lines = mv_lines
    mv_new = mv_new
    TO DATA BUFFER p_attributes.

ENDMETHOD.


METHOD get_message_text.

  CLEAR p_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Use lines( ) expression'.                   "#EC NOTEXT
    WHEN '002'.
      p_text = 'Use NEW abc( ) expression'.                 "#EC NOTEXT
    WHEN '003'.
      p_text = 'Declare variable in LOOP statement'.        "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.


METHOD if_ci_test~query_attributes.

  zzaoc_top.

  zzaoc_fill_att mv_errty 'Error Type' ''.                  "#EC NOTEXT
  zzaoc_fill_att mv_lines 'lines( )' ''.                    "#EC NOTEXT
  zzaoc_fill_att mv_new 'NEW' ''.                           "#EC NOTEXT
  zzaoc_fill_att mv_loop_decl 'Declare variable in LOOP' ''. "#EC NOTEXT

  zzaoc_popup.

ENDMETHOD.


METHOD put_attributes.

  IMPORT
    mv_lines = mv_lines
    mv_new = mv_new
    FROM DATA BUFFER p_attributes.                   "#EC CI_USE_WANTED
  ASSERT sy-subrc = 0.

ENDMETHOD.


METHOD support_inline_decl.

* the check for NEW also uses inline data declarations, so just reuse it
  rv_supported = support_new( ).

ENDMETHOD.


METHOD support_new.

  DATA: lt_itab  TYPE STANDARD TABLE OF string,
        lv_mess  TYPE string,
        lv_lin   TYPE i,
        ls_trdir TYPE trdir,
        lv_code  TYPE string,
        lv_wrd   TYPE string.


  IF gv_new_run = abap_true.
    rv_supported = gv_new_supported.
    RETURN.
  ENDIF.

  lv_code = 'REPORT zfoobar.' ##NO_TEXT.
  APPEND lv_code TO lt_itab.
  lv_code = 'DATA(lo_new) = NEW cl_gui_frontend_services( ).' ##NO_TEXT.
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

  gv_new_supported = rv_supported.
  gv_new_run = abap_true.

ENDMETHOD.


METHOD token_position.

  rs_position-col = is_token-col.
  rs_position-row = is_token-row.

ENDMETHOD.
ENDCLASS.