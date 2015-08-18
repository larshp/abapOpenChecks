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

  data MT_ERROR type ZAOC_SLIN_DESC_KEY_RANGE_TT .
  data MT_WARN type ZAOC_SLIN_DESC_KEY_RANGE_TT .
  data MT_INFO type ZAOC_SLIN_DESC_KEY_RANGE_TT .
  data MT_IGNORE type ZAOC_SLIN_DESC_KEY_RANGE_TT .

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
        lv_tmp      TYPE string,
        ls_flags    TYPE rslin_test_flags,
        lv_errty    TYPE sci_errty,
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

  LOOP AT lt_result ASSIGNING <ls_result>.              "#EC CI_SORTSEQ

    CLEAR lv_text.
    LOOP AT <ls_result>-lines ASSIGNING <ls_line>.
      CONCATENATE LINES OF cl_slin_io=>old_line_to_src( <ls_line> ) INTO lv_tmp.
      IF lv_text IS INITIAL.
        lv_text = lv_tmp.
      ELSE.
        CONCATENATE lv_text cl_abap_char_utilities=>newline lv_tmp INTO lv_text.
      ENDIF.
    ENDLOOP.

    IF lines( mt_error ) > 0 AND <ls_result>-code IN mt_error.
      lv_errty = c_error.
    ELSEIF lines( mt_warn ) > 0 AND <ls_result>-code IN mt_warn.
      lv_errty = c_warning.
    ELSEIF lines( mt_info ) > 0 AND <ls_result>-code IN mt_info.
      lv_errty = c_note.
    ELSEIF lines( mt_ignore ) > 0 AND <ls_result>-code IN mt_ignore.
      CONTINUE.
    ELSE.
      lv_errty = c_error.
    ENDIF.

    lv_obj_name = <ls_result>-src_incl.
    inform( p_sub_obj_type = c_type_include
            p_sub_obj_name = lv_obj_name
            p_line         = <ls_result>-src_line
            p_kind         = lv_errty
            p_test         = myname
            p_code         = '001'
            p_param_1      = <ls_result>-code
            p_param_2      = lv_text ).
  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Extended Program Check, Filterable'.    "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '002'.
  position       = '031'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_attributes.

  EXPORT
    mt_error = mt_error
    mt_warn = mt_warn
    mt_info = mt_info
    mt_ignore = mt_ignore
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

  DATA: lt_attributes TYPE sci_atttab,
        ls_attribute  LIKE LINE OF lt_attributes.

  DEFINE fill_att.
    get reference of &1 into ls_attribute-ref.
    ls_attribute-text = &2.
    ls_attribute-kind = &3.
    append ls_attribute to lt_attributes.
  END-OF-DEFINITION.


  fill_att mt_error 'Error' 'S'.                            "#EC NOTEXT
  fill_att mt_warn 'Warning' 'S'.                           "#EC NOTEXT
  fill_att mt_info 'Info' 'S'.                              "#EC NOTEXT
  fill_att mt_ignore 'Ignore' 'S'.                          "#EC NOTEXT

  cl_ci_query_attributes=>generic(
                        p_name       = myname
                        p_title      = 'Options'
                        p_attributes = lt_attributes
                        p_display    = p_display ).         "#EC NOTEXT
  attributes_ok = abap_true.

ENDMETHOD.


METHOD put_attributes.

  IMPORT
    mt_error = mt_error
    mt_warn = mt_warn
    mt_info = mt_info
    mt_ignore = mt_ignore
    FROM DATA BUFFER p_attributes.                   "#EC CI_USE_WANTED
  ASSERT sy-subrc = 0.

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