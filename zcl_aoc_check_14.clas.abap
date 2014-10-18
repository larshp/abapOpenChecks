class ZCL_AOC_CHECK_14 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.
*"* public components of class ZCL_AOC_CHECK_14
*"* do not include other source files here!!!

  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_ATTRIBUTES
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
  methods IF_CI_TEST~DISPLAY_DOCUMENTATION
    redefinition .
  methods IF_CI_TEST~QUERY_ATTRIBUTES
    redefinition .
  methods PUT_ATTRIBUTES
    redefinition .
protected section.
*"* protected components of class ZCL_AOC_CHECK_14
*"* do not include other source files here!!!

  data MV_ERRTY type SCI_ERRTY .
private section.
*"* private components of class ZCL_AOC_CHECK_14
*"* do not include other source files here!!!

  constants C_MY_NAME type SEOCLSNAME value 'ZCL_AOC_CHECK_14'. "#EC NOTEXT

  methods PARSE
    importing
      !IT_COMMENTED type STRING_TABLE
      !IS_LEVEL type SLEVEL
      !IV_LINE type TOKEN_ROW .
ENDCLASS.



CLASS ZCL_AOC_CHECK_14 IMPLEMENTATION.


METHOD check.

  DATA: lt_code      TYPE string_table,
        lt_commented TYPE string_table,
        lv_line      TYPE token_row.

  FIELD-SYMBOLS: <ls_level> LIKE LINE OF it_levels,
                 <lv_code>  LIKE LINE OF lt_code.


  LOOP AT it_levels ASSIGNING <ls_level> WHERE type = scan_level_type-program.
    lt_code = get_source( <ls_level> ).

    LOOP AT lt_code ASSIGNING <lv_code>.
      lv_line = sy-tabix.

      IF strlen( <lv_code> ) > 1 AND <lv_code>(1) = '*'.
        APPEND <lv_code>+1 TO lt_commented.
      ELSE.
        parse( it_commented = lt_commented
               is_level     = <ls_level>
               iv_line      = lv_line - 1 ).
        CLEAR lt_commented.
      ENDIF.

    ENDLOOP.

    parse( it_commented = lt_commented
           is_level     = <ls_level>
           iv_line      = lv_line ).
    CLEAR lt_commented.
  ENDLOOP.

ENDMETHOD.


METHOD constructor .

  super->constructor( ).

  description    = 'Commented code'.                        "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_attributes.

  EXPORT mv_errty = mv_errty TO DATA BUFFER p_attributes.

ENDMETHOD.


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Commented code'.                            "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT


METHOD if_ci_test~display_documentation.

  documentation( c_my_name ).

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

  WHILE lv_ok = abap_false.
    cl_ci_query_attributes=>generic(
                          p_name       = c_my_name
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


METHOD parse.

  IF lines( it_commented ) = 0.
    RETURN.
  ENDIF.

  IF zcl_aoc_parser=>run( it_commented ) = abap_true.
    inform( p_sub_obj_type = c_type_include
            p_sub_obj_name = is_level-name
            p_line         = iv_line
            p_kind         = mv_errty
            p_test         = c_my_name
            p_code         = '001' ).
  ENDIF.

ENDMETHOD.


METHOD put_attributes.

  IMPORT
    mv_errty = mv_errty
    FROM DATA BUFFER p_attributes.                   "#EC CI_USE_WANTED

ENDMETHOD.
ENDCLASS.