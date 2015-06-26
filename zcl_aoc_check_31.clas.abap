class ZCL_AOC_CHECK_31 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.

*"* public components of class ZCL_AOC_CHECK_31
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
private section.

  data MT_CODES type ZAOC_SLIN_DESC_KEY_RANGE_TT .
*"* private components of class ZCL_AOC_CHECK_31
*"* do not include other source files here!!!
  constants C_MY_NAME type SEOCLSNAME value 'ZCL_AOC_CHECK_31'. "#EC NOTEXT

  methods SET_FLAGS
    returning
      value(RS_FLAGS) type RSLIN_TEST_FLAGS .
ENDCLASS.



CLASS ZCL_AOC_CHECK_31 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lv_obj_name TYPE sobj_name,
        lv_text     TYPE string,
        ls_flags    TYPE rslin_test_flags,
        lt_result   TYPE slin_result.

  FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_result,
                 <ls_line>   LIKE LINE OF <ls_result>-lines.


  ls_flags = set_flags( ).
  CALL FUNCTION 'EXTENDED_PROGRAM_CHECK'
    EXPORTING
      program    = program_name
      test_flags = ls_flags
    IMPORTING
      result     = lt_result.

  LOOP AT lt_result ASSIGNING <ls_result> WHERE code IN mt_codes. "#EC CI_SORTSEQ

    LOOP AT <ls_result>-lines ASSIGNING <ls_line>.
      CONCATENATE LINES OF cl_slin_io=>old_line_to_src( <ls_line> ) INTO lv_text.
      EXIT.
    ENDLOOP.

    lv_obj_name = <ls_result>-src_incl.
    inform( p_sub_obj_type = c_type_include
            p_sub_obj_name = lv_obj_name
            p_line         = <ls_result>-src_line
            p_kind         = mv_errty
            p_test         = c_my_name
            p_code         = '001'
            p_param_1      = <ls_result>-code
            p_param_2      = lv_text ).
  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Extended Program Check, Filterable'.    "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_attributes.

  EXPORT
    mv_errty = mv_errty
    mt_codes = mt_codes
    TO DATA BUFFER p_attributes.

ENDMETHOD.


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = '&1 &2'.                                     "#EC NOTEXT
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
  fill_att mt_codes 'Code' 'S'.                             "#EC NOTEXT

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
    mt_codes = mt_codes
    FROM DATA BUFFER p_attributes.                   "#EC CI_USE_WANTED

ENDMETHOD.


METHOD set_flags.

  rs_flags-x_per = abap_true.
  rs_flags-x_cal = abap_true.
  rs_flags-x_dat = abap_true.
  rs_flags-x_opf = abap_true.
  rs_flags-x_unr = abap_true.
  rs_flags-x_ges = abap_true.
  rs_flags-x_mes = abap_true.
  rs_flags-x_pfs = abap_true.
  rs_flags-x_bre = abap_true.
  rs_flags-x_woo = abap_true.
  rs_flags-x_wrn = abap_true.
  rs_flags-x_ste = abap_true.
  rs_flags-x_txt = abap_true.
  rs_flags-x_aut = abap_true.
  rs_flags-x_sub = abap_true.
  rs_flags-x_loa = abap_true.
  rs_flags-x_mls = abap_true.
  rs_flags-x_put = abap_true.
  rs_flags-x_hel = abap_true.

ENDMETHOD.
ENDCLASS.