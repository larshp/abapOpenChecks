class ZCL_AOC_CHECK_05 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.
*"* public components of class ZCL_AOC_CHECK_05
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
*"* protected components of class ZCL_AOC_CHECK_05
*"* do not include other source files here!!!

  data MV_ERRTY type SCI_ERRTY .
private section.
*"* private components of class ZCL_AOC_CHECK_05
*"* do not include other source files here!!!

  constants C_MY_NAME type SEOCLSNAME value 'ZCL_AOC_CHECK_05'. "#EC NOTEXT
ENDCLASS.



CLASS ZCL_AOC_CHECK_05 IMPLEMENTATION.


METHOD check.

  DATA: lt_code    TYPE string_table,
        lv_line    TYPE token_row,
        lv_column  TYPE token_col,
        lo_obj     TYPE REF TO cl_abap_conv_out_ce,
        lv_offset  TYPE i,
        lv_bit     TYPE c LENGTH 1,
        lv_xstring TYPE xstring,
        lv_hex     TYPE x LENGTH 2.

  FIELD-SYMBOLS: <ls_level> LIKE LINE OF it_levels,
                 <lv_code>  LIKE LINE OF lt_code.


* convert to UTF-16BE
  lo_obj = cl_abap_conv_out_ce=>create( encoding = '4102' ).

  LOOP AT it_levels ASSIGNING <ls_level> WHERE type = scan_level_type-program..
    lt_code = get_source( <ls_level> ).
    LOOP AT lt_code ASSIGNING <lv_code>.
      lv_line = sy-tabix.

      TRY.
          lo_obj->convert( EXPORTING data   = <lv_code>
                           IMPORTING buffer = lv_xstring ).
        CATCH cx_parameter_invalid_range
              cx_sy_codepage_converter_init
              cx_sy_conversion_codepage
              cx_parameter_invalid_type.
          CONTINUE. " current loop
      ENDTRY.

      DO xstrlen( lv_xstring ) / 2 TIMES.
        lv_offset = ( sy-index - 1 ) * 2.
        lv_hex = lv_xstring+lv_offset(2).

        GET BIT 9 OF lv_hex INTO lv_bit.
        IF lv_bit = '1' OR lv_hex(1) <> '00'.
          lv_column = lv_offset / 2 + 1.
          inform( p_sub_obj_type = c_type_include
                  p_sub_obj_name = <ls_level>-name
                  p_line         = lv_line
                  p_column       = lv_column
                  p_kind         = mv_errty
                  p_test         = c_my_name
                  p_code         = '001' ).
          EXIT. " current loop, skip to next line
        ENDIF.
      ENDDO.

    ENDLOOP.
  ENDLOOP.

ENDMETHOD.


METHOD constructor .

  super->constructor( ).

  description    = '7 bit ASCII'.                           "#EC NOTEXT
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
      p_text = 'Conatins non 7 bit ASCII'.                  "#EC NOTEXT
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


METHOD put_attributes.

  IMPORT
    mv_errty = mv_errty
    FROM DATA BUFFER p_attributes.                   "#EC CI_USE_WANTED

ENDMETHOD.
ENDCLASS.