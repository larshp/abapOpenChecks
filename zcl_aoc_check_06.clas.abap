class ZCL_AOC_CHECK_06 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.
*"* public components of class ZCL_AOC_CHECK_06
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
*"* protected components of class ZCL_AOC_CHECK_06
*"* do not include other source files here!!!

  data MV_ONE_FINDING type FLAG .
  data MV_HIKEY type FLAG .
  data MV_LOWER type FLAG .
  data MV_UPPER type FLAG .
  data MV_LOKEY type FLAG .
private section.
*"* private components of class ZCL_AOC_CHECK_06
*"* do not include other source files here!!!

  constants C_MY_NAME type SEOCLSNAME value 'ZCL_AOC_CHECK_06'. "#EC NOTEXT
ENDCLASS.



CLASS ZCL_AOC_CHECK_06 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lt_code     TYPE string_table,
        lt_pretty   TYPE string_table,
        ls_rseumod  TYPE rseumod,
        lv_level    TYPE i,
        lv_row      TYPE i,
        lv_option   TYPE c LENGTH 5.

  FIELD-SYMBOLS: <ls_level>  LIKE LINE OF it_levels,
                 <lv_code>   LIKE LINE OF lt_code,
                 <lv_pretty> LIKE LINE OF lt_pretty.

* check workbench settings
  CALL FUNCTION 'RS_WORKBENCH_CUSTOMIZING'
    EXPORTING
      choice          = 'WB'
      suppress_dialog = 'X'
    IMPORTING
      setting         = ls_rseumod.
  IF ls_rseumod-lowercase = 'X'.
    lv_option = 'LOWER'.
  ELSEIF ls_rseumod-lowercase = 'G'.
    lv_option = 'HIKEY'.
  ELSEIF ls_rseumod-lowercase = 'L'.
    lv_option = 'LOKEY'.
  ELSE.
    lv_option = 'UPPER'.
  ENDIF.

  IF ( mv_lower = abap_true AND lv_option <> 'LOWER' )
      OR ( mv_hikey = abap_true AND lv_option <> 'HIKEY' )
      OR ( mv_lokey = abap_true AND lv_option <> 'LOKEY' )
      OR ( mv_upper = abap_true AND lv_option <> 'UPPER' ).
    inform( p_sub_obj_type = object_type
            p_sub_obj_name = object_name
            p_kind         = mv_errty
            p_test         = c_my_name
            p_code         = '002' ).
    RETURN.
  ENDIF.

  LOOP AT it_levels ASSIGNING <ls_level> WHERE type = scan_level_type-program.
    lv_level = sy-tabix.

* skip class definitions, they are auto generated(in most cases)
* todo: move this to check configuration
    IF strlen( <ls_level>-name ) = 32
        AND ( object_type = 'CLAS' OR object_type = 'INTF' )
        AND ( <ls_level>-name+30(2) = 'CU'
        OR <ls_level>-name+30(2) = 'CO'
        OR <ls_level>-name+30(2) = 'CI'
        OR <ls_level>-name+30(2) = 'CP'
        OR <ls_level>-name+30(2) = 'IP'
        OR <ls_level>-name+30(2) = 'IU' ).
      CONTINUE. " current loop
    ENDIF.
    IF <ls_level>-name(8) = '/1BCWDY/'.
* todo, web dynpro
      RETURN.
    ENDIF.

* make sure the source code is not empty, as it will cause the pretty
* printer to show an error message
    READ TABLE it_statements WITH KEY level = lv_level TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    lt_code = get_source( <ls_level> ).

    lt_pretty = lt_code.
    CALL FUNCTION 'CREATE_PRETTY_PRINT_FORMAT'
      EXPORTING
        mode          = lv_option
      TABLES
        source        = lt_pretty
      EXCEPTIONS
        syntax_errors = 1
        OTHERS        = 2.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    LOOP AT lt_code ASSIGNING <lv_code>.
      lv_row = sy-tabix.
      READ TABLE lt_pretty INDEX lv_row ASSIGNING <lv_pretty>.

      IF <lv_code> <> <lv_pretty>.
        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = <ls_level>-name
                p_line         = lv_row
                p_kind         = mv_errty
                p_test         = c_my_name
                p_code         = '001' ).
        IF mv_one_finding = abap_true.
          EXIT. " current loop, only one error per level
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Check pretty printer use'.              "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.
  mv_hikey = abap_true.
  mv_lokey = abap_false.
  mv_lower = abap_false.
  mv_upper = abap_false.
  mv_one_finding = abap_true.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_attributes.

  EXPORT
    mv_errty = mv_errty
    mv_hikey = mv_hikey
    mv_lokey = mv_lokey
    mv_lower = mv_lower
    mv_upper = mv_upper
    mv_one_finding = mv_one_finding
    TO DATA BUFFER p_attributes.

ENDMETHOD.


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Use pretty printer'.                        "#EC NOTEXT
    WHEN '002'.
      p_text = 'Pretty printer settings does not match'.    "#EC NOTEXT
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
    clear ls_attribute.
    get reference of &1 into ls_attribute-ref.
    ls_attribute-text = &2.
    ls_attribute-kind = &3.
    append ls_attribute to lt_attributes.
  END-OF-DEFINITION.

  DEFINE fill_att_rb.
    clear ls_attribute.
    get reference of &1 into ls_attribute-ref.
    ls_attribute-text = &2.
    ls_attribute-kind = &3.
    ls_attribute-button_group = &4.
    append ls_attribute to lt_attributes.
  END-OF-DEFINITION.


  fill_att mv_errty 'Error Type' ''.                        "#EC NOTEXT

  fill_att_rb mv_hikey 'Keywords upper case' 'R' 'TYPE'.    "#EC NOTEXT
  fill_att_rb mv_lokey 'Keywords lower case' 'R' 'TYPE'.    "#EC NOTEXT
  fill_att_rb mv_upper 'Upper case' 'R' 'TYPE'.    "#EC NOTEXT
  fill_att_rb mv_lower 'Lower case' 'R' 'TYPE'.    "#EC NOTEXT

  fill_att mv_one_finding 'Report one finding per include' 'C'. "#EC NOTEXT

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
    mv_hikey = mv_hikey
    mv_lokey = mv_lokey
    mv_lower = mv_lower
    mv_upper = mv_upper
    mv_one_finding = mv_one_finding
    FROM DATA BUFFER p_attributes.                   "#EC CI_USE_WANTED

ENDMETHOD.
ENDCLASS.