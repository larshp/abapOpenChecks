class ZCL_AOC_CHECK_03 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.
*"* public components of class ZCL_AOC_CHECK_03
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
*"* protected components of class ZCL_AOC_CHECK_03
*"* do not include other source files here!!!

  data MV_ERRTY type SCI_ERRTY .
private section.
*"* private components of class ZCL_AOC_CHECK_03
*"* do not include other source files here!!!

  constants C_MY_NAME type SEOCLSNAME value 'ZCL_AOC_CHECK_03'. "#EC NOTEXT
ENDCLASS.



CLASS ZCL_AOC_CHECK_03 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lv_include TYPE program,
        lv_found   TYPE abap_bool,
        lv_index   LIKE sy-tabix.

  FIELD-SYMBOLS: <ls_structure> LIKE LINE OF it_structures,
                 <ls_statement> LIKE LINE OF it_statements,
                 <ls_token>     LIKE LINE OF it_tokens.


  LOOP AT it_structures ASSIGNING <ls_structure>
      WHERE stmnt_type = scan_struc_stmnt_type-try.
    lv_index = sy-tabix.

    lv_found = abap_false.

    READ TABLE it_structures
      WITH KEY stmnt_type = scan_struc_stmnt_type-catch back = lv_index
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      lv_found = abap_true.
    ENDIF.

    READ TABLE it_structures
      WITH KEY stmnt_type = scan_struc_stmnt_type-cleanup back = lv_index
      TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      lv_found = abap_true.
    ENDIF.

    IF lv_found = abap_false.

      READ TABLE it_statements ASSIGNING <ls_statement> INDEX <ls_structure>-stmnt_from.
      ASSERT sy-subrc = 0.

      READ TABLE it_tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
      ASSERT sy-subrc = 0.

      lv_include = get_include( p_level = <ls_statement>-level ).

      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = lv_include
              p_line = <ls_token>-row
              p_kind = mv_errty
              p_test = c_my_name
              p_code = '001' ).
    ENDIF.

  ENDLOOP.

ENDMETHOD.


METHOD constructor .

  super->constructor( ).

  description    = 'TRY without CATCH'.                     "#EC NOTEXT
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
      p_text = 'TRY without CATCH'.                         "#EC NOTEXT
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

  IMPORT mv_errty = mv_errty FROM DATA BUFFER p_attributes. "#EC CI_USE_WANTED

ENDMETHOD.
ENDCLASS.