CLASS zcl_aoc_check_24 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_list,
      proc_name1     TYPE sci_proc_name,
      code1          TYPE string,
      line1          TYPE sci_proc_line,
      proc_name2     TYPE sci_proc_name,
      code2          TYPE string,
      line2          TYPE sci_proc_line,
      END   OF ty_list .
    TYPES:
      ty_list_tt TYPE STANDARD TABLE OF ty_list with default key.

*"* public components of class ZCL_AOC_CHECK_24
*"* do not include other source files here!!!
    METHODS constructor .

    METHODS check
      REDEFINITION .
    METHODS get_attributes
      REDEFINITION .
    METHODS get_message_text
      REDEFINITION .
    METHODS get_result_node
      REDEFINITION .
    METHODS if_ci_test~query_attributes
      REDEFINITION .
    METHODS put_attributes
      REDEFINITION .
protected section.

  types:
*"* protected components of class ZCL_AOC_CHECK_24
*"* do not include other source files here!!!
    BEGIN OF ty_code,
             statement TYPE string,
             level TYPE i,
             row TYPE token_row,
           END OF ty_code .
  types:
    tt_code TYPE STANDARD TABLE OF ty_code WITH NON-UNIQUE DEFAULT KEY .
  types:
    BEGIN OF ty_block,
             statements TYPE string,
             statement_list type standard table of string with default key,
             level TYPE i,
             row TYPE token_row,
           END OF ty_block .
  types:
    tt_block TYPE STANDARD TABLE OF ty_block WITH NON-UNIQUE DEFAULT KEY .

  data MV_STATEMENTS type I .

  methods PACK
    importing
      !IT_LIST type TY_LIST_TT
    returning
      value(RV_STRING) type STRING .
  methods ANALYZE
    changing
      !CT_BLOCKS type TT_BLOCK .
  methods BUILD_BLOCKS
    importing
      !IT_CODE type TT_CODE
    exporting
      !ET_BLOCKS type TT_BLOCK .
  methods BUILD_CODE
    importing
      !IT_TOKENS type STOKESX_TAB
      !IT_STATEMENTS type SSTMNT_TAB
      !IT_LEVELS type SLEVEL_TAB
    returning
      value(RT_CODE) type TT_CODE .
private section.
ENDCLASS.



CLASS ZCL_AOC_CHECK_24 IMPLEMENTATION.


METHOD analyze.

  DATA: ls_prev     LIKE LINE OF ct_blocks,
        lt_list     TYPE ty_list_tt,
        ls_list     LIKE LINE OF lt_list,
        lv_add      TYPE i,
        lv_include1 TYPE program,
        lv_include2 TYPE program.

  FIELD-SYMBOLS: <ls_block>     LIKE LINE OF ct_blocks,
                 <lv_statement> TYPE string,
                 <ls_list>      LIKE LINE OF lt_list.


  SORT ct_blocks BY statements ASCENDING.

  LOOP AT ct_blocks ASSIGNING <ls_block>.

    IF <ls_block>-statements = ls_prev-statements.
      lv_include1 = get_include( p_level = ls_prev-level ).
      lv_include2 = get_include( p_level = <ls_block>-level ).

      ls_list-proc_name1 = lv_include1.
      ls_list-line1      = ls_prev-row.
      ls_list-proc_name2 = lv_include2.
      ls_list-line2      = <ls_block>-row.

      CLEAR lt_list.
      LOOP AT <ls_block>-statement_list ASSIGNING <lv_statement>.
        lv_add = sy-tabix - 1.

        APPEND INITIAL LINE TO lt_list ASSIGNING <ls_list>.
        <ls_list> = ls_list.
        <ls_list>-code1 = <lv_statement>.
        <ls_list>-code2 = <lv_statement>.
        <ls_list>-line1 = <ls_list>-line1 + lv_add.
        <ls_list>-line2 = <ls_list>-line2 + lv_add.
      ENDLOOP.

      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = lv_include1
              p_line         = ls_prev-row
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '001'
              p_param_1      = pack( lt_list ) ).

* dont report too may findings if there are identical blocks larger than mv_statements
      DELETE ct_blocks WHERE level = ls_prev-level
        AND row >= ls_prev-row
        AND row <= ls_prev-row + mv_statements.
      CLEAR ls_prev.
      CONTINUE.
    ENDIF.

    ls_prev = <ls_block>.
  ENDLOOP.

ENDMETHOD.


METHOD build_blocks.

  DATA: lv_index TYPE i,
        lv_add   TYPE abap_bool,
        ls_block LIKE LINE OF et_blocks,
        lv_level TYPE i.

  FIELD-SYMBOLS: <ls_code> LIKE LINE OF it_code,
                 <ls_level> LIKE LINE OF it_code.


* todo, look at memory usage
  LOOP AT it_code ASSIGNING <ls_level>.
    lv_index = sy-tabix.
    lv_level = <ls_level>-level.

    lv_add = abap_true.
    CLEAR ls_block.

    DO mv_statements TIMES.
      READ TABLE it_code ASSIGNING <ls_code> INDEX lv_index.
      IF sy-subrc <> 0 OR <ls_code>-level <> lv_level.
        lv_add = abap_false.
        EXIT.
      ENDIF.
      IF ls_block IS INITIAL.
        ls_block-level = <ls_code>-level.
        ls_block-row = <ls_code>-row.
      ENDIF.
      CONCATENATE ls_block-statements <ls_code>-statement
        INTO ls_block-statements SEPARATED BY space.
      APPEND <ls_code>-statement TO ls_block-statement_list.
      lv_index = lv_index + 1.
    ENDDO.

    IF lv_add = abap_true.
      APPEND ls_block TO et_blocks.
    ENDIF.

  ENDLOOP.

ENDMETHOD.


METHOD build_code.

  DATA: lv_level     TYPE i,
        lv_statement TYPE string.

  FIELD-SYMBOLS: <ls_level> LIKE LINE OF it_levels,
                 <ls_token> LIKE LINE OF it_tokens,
                 <ls_code>  LIKE LINE OF rt_code,
                 <ls_statement> LIKE LINE OF it_statements.


  LOOP AT it_levels ASSIGNING <ls_level>.
    lv_level = sy-tabix.

    LOOP AT it_statements ASSIGNING <ls_statement> WHERE level = lv_level.

      CLEAR lv_statement.
      LOOP AT it_tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from
          TO <ls_statement>-to
          WHERE type <> scan_token_type-comment.
        IF lv_statement IS INITIAL.
          lv_statement = <ls_token>-str.
        ELSE.
          CONCATENATE lv_statement <ls_token>-str
            INTO lv_statement SEPARATED BY space.
        ENDIF.
      ENDLOOP.

      IF lv_statement IS INITIAL.
        CONTINUE.
      ENDIF.

      APPEND INITIAL LINE TO rt_code ASSIGNING <ls_code>.
      <ls_code>-statement = lv_statement.
      <ls_code>-level = lv_level.
      <ls_code>-row = <ls_token>-row.

    ENDLOOP.

  ENDLOOP.

ENDMETHOD.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lt_code   TYPE tt_code,
        lt_blocks TYPE tt_block.


  lt_code = build_code(
      it_tokens     = it_tokens
      it_statements = it_statements
      it_levels     = it_levels ).

  build_blocks( EXPORTING it_code   = lt_code
                IMPORTING et_blocks = lt_blocks ).

  analyze( CHANGING ct_blocks = lt_blocks ).

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Identical code blocks'.                 "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.
  position       = '024'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.
  mv_statements = 10.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_attributes.

  EXPORT mv_errty = mv_errty mv_statements = mv_statements TO DATA BUFFER p_attributes.

ENDMETHOD.


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Identical code blocks, dbl click for details'. "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT


METHOD get_result_node.

  CREATE OBJECT p_result TYPE zcl_aoc_check_24_result
    EXPORTING
      iv_kind = p_kind.

ENDMETHOD.


METHOD if_ci_test~query_attributes.

  DATA: lv_ok         TYPE abap_bool,
        lv_message    TYPE c LENGTH 72,
        lt_attributes TYPE sci_atttab,
        ls_attribute  LIKE LINE OF lt_attributes.

  DEFINE fill_att.
    get reference of &1 into ls_attribute-ref.
    ls_attribute-text = &2.
    ls_attribute-kind = &3.
    append ls_attribute to lt_attributes.
  END-OF-DEFINITION.


  fill_att mv_errty 'Error Type' ''.                        "#EC NOTEXT
  fill_att mv_statements 'Statements' ''.                   "#EC NOTEXT

  WHILE lv_ok = abap_false.
    cl_ci_query_attributes=>generic(
                          p_name       = myname
                          p_title      = 'Options'
                          p_attributes = lt_attributes
                          p_message    = lv_message
                          p_display    = p_display ).       "#EC NOTEXT
    IF ( mv_errty = c_error OR mv_errty = c_warning OR mv_errty = c_note )
        AND mv_statements > 0.
      lv_ok = abap_true.
    ELSE.
      lv_message = 'Fill attributes'.                       "#EC NOTEXT
    ENDIF.
  ENDWHILE.

ENDMETHOD.


METHOD pack.

  DATA: lv_xstring TYPE xstring.


  EXPORT list = it_list TO DATA BUFFER lv_xstring COMPRESSION ON.
  rv_string = lv_xstring.

ENDMETHOD.


METHOD put_attributes.

  IMPORT
    mv_errty = mv_errty
    mv_statements = mv_statements
    FROM DATA BUFFER p_attributes.                   "#EC CI_USE_WANTED

ENDMETHOD.
ENDCLASS.