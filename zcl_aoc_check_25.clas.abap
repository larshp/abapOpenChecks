class ZCL_AOC_CHECK_25 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.
*"* public components of class ZCL_AOC_CHECK_25
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
*"* protected components of class ZCL_AOC_CHECK_25
*"* do not include other source files here!!!

  types:
    BEGIN OF ty_field,
             name TYPE string,
             level TYPE i,
             row TYPE token_row,
           END OF ty_field .
  types:
    tt_fields TYPE STANDARD TABLE OF ty_field WITH NON-UNIQUE DEFAULT KEY .

  data MV_SKIP_RADIO type SYCHAR01 .

  methods STRIP
    importing
      !IV_INPUT type STRING
    returning
      value(RV_OUTPUT) type STRING .
  methods ANALYZE
    importing
      !IT_TOKENS type STOKESX_TAB
      !IT_FIELDS type TT_FIELDS
      !IT_STATEMENTS type SSTMNT_TAB .
  methods FIND_FIELDS
    importing
      !IT_TOKENS type STOKESX_TAB
      !IT_STATEMENTS type SSTMNT_TAB
    returning
      value(RT_FIELDS) type TT_FIELDS .
private section.
ENDCLASS.



CLASS ZCL_AOC_CHECK_25 IMPLEMENTATION.


METHOD analyze.

* this might not be 100% correct but will work for most cases

  DATA: lv_include TYPE program,
        lv_name    TYPE string,
        lt_fields  LIKE it_fields.

  FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                 <ls_field> LIKE LINE OF it_fields,
                 <ls_token> LIKE LINE OF it_tokens.


  lt_fields[] = it_fields[].

  LOOP AT it_statements ASSIGNING <ls_statement>.
    LOOP AT it_tokens ASSIGNING <ls_token>
        FROM <ls_statement>-from
        TO <ls_statement>-to
        WHERE type <> scan_token_type-comment.
      lv_name = strip( <ls_token>-str ).
      DELETE lt_fields
        WHERE name = lv_name
        AND ( row <> <ls_token>-row
        OR level <> <ls_statement>-level ).
    ENDLOOP.
  ENDLOOP.

  LOOP AT lt_fields ASSIGNING <ls_field>.

    lv_include = get_include( p_level = <ls_field>-level ).

    inform( p_sub_obj_type = c_type_include
            p_sub_obj_name = lv_include
            p_line         = <ls_field>-row
            p_kind         = mv_errty
            p_test         = myname
            p_code         = '001'
            p_param_1      = <ls_field>-name ).
  ENDLOOP.

ENDMETHOD.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lt_fields TYPE tt_fields.


  lt_fields = find_fields( it_tokens     = it_tokens
                           it_statements = it_statements ).

  analyze( it_tokens     = it_tokens
           it_fields     = lt_fields
           it_statements = it_statements ).

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Selection screen data not referenced statically'. "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.
  position       = '025'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.
  mv_skip_radio = abap_true.

ENDMETHOD.                    "CONSTRUCTOR


METHOD find_fields.

  DATA: lv_keyword   TYPE string,
        lt_code      TYPE string_table,
        ls_result    TYPE zcl_aoc_parser=>st_result,
        ls_field     LIKE LINE OF rt_fields,
        lv_statement TYPE string.

  FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                 <ls_rt>        LIKE LINE OF ls_result-tokens,
                 <ls_token>     LIKE LINE OF it_tokens.


  LOOP AT it_statements ASSIGNING <ls_statement> WHERE type = scan_stmnt_type-standard.

    CLEAR lv_keyword.
    CLEAR lv_statement.

    LOOP AT it_tokens ASSIGNING <ls_token>
        FROM <ls_statement>-from
        TO <ls_statement>-to
        WHERE type <> scan_token_type-comment.

      IF lv_keyword IS INITIAL.
        lv_keyword = <ls_token>-str.
      ENDIF.

      IF lv_statement IS INITIAL.
        lv_statement = <ls_token>-str.
      ELSE.
        CONCATENATE lv_statement <ls_token>-str INTO lv_statement SEPARATED BY space.
      ENDIF.
    ENDLOOP.

    IF lv_keyword <> 'PARAMETERS'
        AND lv_keyword <> 'PARAMETER'
        AND lv_keyword <> 'SELECT-OPTIONS'.
      CONTINUE. " current loop
    ENDIF.

    CLEAR lt_code.
    APPEND lv_statement TO lt_code.
    ls_result = zcl_aoc_parser=>run( it_code = lt_code
                                     iv_rule = lv_keyword ).

    IF ls_result-match = abap_false.
      CONTINUE.
    ENDIF.

    LOOP AT ls_result-tokens ASSIGNING <ls_rt>
        WHERE value = zcl_aoc_parser=>c_role_fielddefid
        AND type = zcl_aoc_parser=>c_role.

      IF mv_skip_radio = abap_true.
        READ TABLE ls_result-tokens
          WITH KEY type = zcl_aoc_parser=>c_terminal code = 'RADIOBUTTON'
          TRANSPORTING NO FIELDS.
        IF sy-subrc = 0.
          CONTINUE.
        ENDIF.
      ENDIF.

      CLEAR ls_field.
      ls_field-name  = strip( <ls_rt>-code ).
      ls_field-level = <ls_statement>-level.
      ls_field-row   = <ls_token>-row.
      APPEND ls_field TO rt_fields.
    ENDLOOP.

  ENDLOOP.

ENDMETHOD.


METHOD get_attributes.

  EXPORT
    mv_errty = mv_errty
    mv_skip_radio = mv_skip_radio
    TO DATA BUFFER p_attributes.

ENDMETHOD.


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = '&1 not referenced statically'. "#EC NOTEXT
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
  fill_att mv_skip_radio 'Skip radio buttons' 'C'.          "#EC NOTEXT

  WHILE lv_ok = abap_false.
    cl_ci_query_attributes=>generic(
                          p_name       = myname
                          p_title      = 'Options'
                          p_attributes = lt_attributes
                          p_message    = lv_message
                          p_display    = p_display ).       "#EC NOTEXT
    IF mv_errty = c_error OR mv_errty = c_warning OR mv_errty = c_note.
      lv_ok = abap_true.
    ELSE.
      lv_message = 'Fill attributes'.                       "#EC NOTEXT
    ENDIF.
  ENDWHILE.

ENDMETHOD.


METHOD put_attributes.

  IMPORT
    mv_errty = mv_errty
    mv_skip_radio = mv_skip_radio
    FROM DATA BUFFER p_attributes.                   "#EC CI_USE_WANTED

ENDMETHOD.


METHOD strip.

  DATA: lv_offset TYPE i.


  rv_output = iv_input.

  FIND FIRST OCCURRENCE OF '[' IN rv_output MATCH OFFSET lv_offset.
  IF sy-subrc = 0.
    rv_output = rv_output(lv_offset).
  ENDIF.

  FIND FIRST OCCURRENCE OF '+' IN rv_output MATCH OFFSET lv_offset.
  IF sy-subrc = 0.
    rv_output = rv_output(lv_offset).
  ENDIF.

  FIND FIRST OCCURRENCE OF '(' IN rv_output MATCH OFFSET lv_offset.
  IF sy-subrc = 0.
    rv_output = rv_output(lv_offset).
  ENDIF.

ENDMETHOD.
ENDCLASS.