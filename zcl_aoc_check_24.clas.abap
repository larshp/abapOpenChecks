class ZCL_AOC_CHECK_24 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.
*"* public components of class ZCL_AOC_CHECK_24
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
*"* protected components of class ZCL_AOC_CHECK_24
*"* do not include other source files here!!!

  types:
    BEGIN OF ty_code,
             statement TYPE string,
             level TYPE i,
             row TYPE token_row,
           END OF ty_code .
  types:
    tt_code TYPE STANDARD TABLE OF ty_code WITH NON-UNIQUE DEFAULT KEY .

  data MV_STATEMENTS type I .

  methods ANALYZE
    changing
      !CT_BLOCKS type TT_CODE .
  methods BUILD_BLOCKS
    importing
      !IT_CODE type TT_CODE
    returning
      value(RT_BLOCKS) type TT_CODE .
  methods BUILD_CODE
    importing
      !IT_TOKENS type STOKESX_TAB
      !IT_STATEMENTS type SSTMNT_TAB
      !IT_LEVELS type SLEVEL_TAB
    returning
      value(RT_CODE) type TT_CODE .
private section.
*"* private components of class ZCL_AOC_CHECK_24
*"* do not include other source files here!!!

  constants C_MY_NAME type SEOCLSNAME value 'ZCL_AOC_CHECK_24'. "#EC NOTEXT
ENDCLASS.



CLASS ZCL_AOC_CHECK_24 IMPLEMENTATION.


METHOD analyze.

  DATA: ls_prev    LIKE LINE OF ct_blocks,
        lv_include TYPE program.

  FIELD-SYMBOLS: <ls_block> LIKE LINE OF ct_blocks.


  SORT ct_blocks BY statement ASCENDING.

  LOOP AT ct_blocks ASSIGNING <ls_block>.

    IF <ls_block>-statement = ls_prev-statement.
      lv_include = get_include( p_level = ls_prev-level ).

      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = lv_include
              p_line = ls_prev-row
              p_kind = mv_errty
              p_test = c_my_name
              p_code = '001' ).
    ENDIF.

    ls_prev = <ls_block>.
  ENDLOOP.

ENDMETHOD.


METHOD build_blocks.

  DATA: lv_index TYPE i,
        lv_add   TYPE abap_bool,
        ls_block LIKE LINE OF rt_blocks,
        lv_level TYPE i.

  FIELD-SYMBOLS: <ls_code> LIKE LINE OF it_code,
                 <ls_level> LIKE LINE OF it_code.


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
      CONCATENATE ls_block-statement <ls_code>-statement
        INTO ls_block-statement SEPARATED BY space.
      lv_index = lv_index + 1.
    ENDDO.

    IF lv_add = abap_true.
      APPEND ls_block TO rt_blocks.
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
        lt_blocks TYPE tt_code.


  lt_code = build_code(
      it_tokens     = it_tokens
      it_statements = it_statements
      it_levels     = it_levels ).

  lt_blocks = build_blocks( lt_code ).

  analyze( CHANGING ct_blocks = lt_blocks ).

ENDMETHOD.


METHOD constructor .

  super->constructor( ).

  description    = 'Identical code blocks'.                 "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.

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
      p_text = 'Identical code blocks'.                     "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT


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
                          p_name       = c_my_name
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


METHOD put_attributes.

  IMPORT
    mv_errty = mv_errty
    mv_statements = mv_statements
    FROM DATA BUFFER p_attributes.                   "#EC CI_USE_WANTED

ENDMETHOD.
ENDCLASS.