class ZCL_AOC_CHECK_41 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.
*"* public components of class ZCL_AOC_CHECK_41
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
*"* protected components of class ZCL_AOC_CHECK_41
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_AOC_CHECK_41
*"* do not include other source files here!!!

  data MV_IGNORE type FLAG .
ENDCLASS.



CLASS ZCL_AOC_CHECK_41 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lv_include TYPE sobj_name,
        lv_comment TYPE abap_bool,
        lv_len     TYPE i,
        lv_prev    TYPE i.

  FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                 <ls_token>     LIKE LINE OF it_tokens,
                 <ls_scomment>  LIKE LINE OF it_statements,
                 <ls_tcomment>  LIKE LINE OF it_tokens.


  LOOP AT it_statements ASSIGNING <ls_statement>
      WHERE type <> scan_stmnt_type-empty
      AND type <> scan_stmnt_type-comment
      AND type <> scan_stmnt_type-comment_in_stmnt
      AND type <> scan_stmnt_type-macro_definition
      AND type <> scan_stmnt_type-pragma
      AND colonrow = 0.

    CLEAR lv_prev.

    LOOP AT it_tokens ASSIGNING <ls_token> FROM <ls_statement>-from TO <ls_statement>-to.
      lv_len = strlen( <ls_token>-str ) - 1.
      IF <ls_token>-str(1) = '(' AND <ls_token>-str+lv_len(1) = ')'.
* special case see unit test 05, SAP is fun
        EXIT.
      ENDIF.

      IF lv_prev IS INITIAL
          OR lv_prev = <ls_token>-row
          OR lv_prev = <ls_token>-row - 1.
        lv_prev = <ls_token>-row.
        CONTINUE.
      ENDIF.

      IF <ls_token>-str = 'GLOBAL'.
* global friends in classes are auto generated with newlines
        EXIT.
      ENDIF.

      lv_comment = abap_false.
      LOOP AT it_statements ASSIGNING <ls_scomment>
          WHERE type = scan_stmnt_type-comment_in_stmnt
          AND level = <ls_statement>-level.
        LOOP AT it_tokens ASSIGNING <ls_tcomment>
            FROM <ls_statement>-from TO <ls_statement>-to.
          IF <ls_tcomment>-row = lv_prev.
            lv_comment = abap_true.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

      IF mv_ignore = abap_false OR lv_comment = abap_false.
        lv_include = get_include( p_level = <ls_statement>-level ).
        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = lv_include
                p_line         = <ls_token>-row
                p_kind         = mv_errty
                p_test         = myname
                p_code         = '001' ).
        EXIT.
      ENDIF.

      lv_prev = <ls_token>-row.
    ENDLOOP.

  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Empty line in statement'.               "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '001'.
  position       = '041'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.
  mv_ignore      = abap_false.

  mv_errty = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_attributes.

  EXPORT
    mv_errty = mv_errty
    mv_ignore = mv_ignore
    TO DATA BUFFER p_attributes.

ENDMETHOD.


METHOD get_message_text.

  CLEAR p_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Empty line in statement'.                   "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.


METHOD if_ci_test~query_attributes.

  zzaoc_top.

  zzaoc_fill_att mv_errty 'Error Type' ''.                  "#EC NOTEXT
  zzaoc_fill_att mv_ignore 'Ignore comments' ''.            "#EC NOTEXT

  zzaoc_popup.

ENDMETHOD.


METHOD put_attributes.

  IMPORT
    mv_errty = mv_errty
    mv_ignore = mv_ignore
    FROM DATA BUFFER p_attributes.                   "#EC CI_USE_WANTED
  ASSERT sy-subrc = 0.

ENDMETHOD.
ENDCLASS.