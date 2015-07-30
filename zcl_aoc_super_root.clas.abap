class ZCL_AOC_SUPER_ROOT definition
  public
  inheriting from CL_CI_TEST_ROOT
  abstract
  create public .

public section.

  types:
*"* public components of class ZCL_AOC_SUPER_ROOT
*"* do not include other source files here!!!
    tt_structures TYPE STANDARD TABLE OF sstruc WITH NON-UNIQUE DEFAULT KEY .

  methods GET_ATTRIBUTES
    redefinition .
  methods IF_CI_TEST~DISPLAY_DOCUMENTATION
    redefinition .
  methods IF_CI_TEST~QUERY_ATTRIBUTES
    redefinition .
  methods PUT_ATTRIBUTES
    redefinition .
protected section.

*"* protected components of class ZCL_AOC_SUPER_ROOT
*"* do not include other source files here!!!
  data MV_ERRTY type SCI_ERRTY .
private section.

  types:
*"* private components of class ZCL_AOC_SUPER_ROOT
*"* do not include other source files here!!!
    BEGIN OF st_source,
           name TYPE level_name,
           code TYPE string_table,
         END OF st_source .
  types:
    tt_source TYPE SORTED TABLE OF st_source WITH UNIQUE KEY name .
ENDCLASS.



CLASS ZCL_AOC_SUPER_ROOT IMPLEMENTATION.


METHOD get_attributes.

  EXPORT mv_errty = mv_errty TO DATA BUFFER p_attributes.

ENDMETHOD.


METHOD if_ci_test~display_documentation.

  DATA: lv_url TYPE string VALUE 'https://github.com/larshp/abapOpenChecks/wiki/'.


  CONCATENATE lv_url myname INTO lv_url.

  cl_gui_frontend_services=>execute( document = lv_url ).

ENDMETHOD.


METHOD if_ci_test~query_attributes.

  DATA: lt_attributes TYPE sci_atttab,
        ls_attribute  LIKE LINE OF lt_attributes.

  DEFINE fill_att.
    get reference of &1 into ls_attribute-ref.
    ls_attribute-text = &2.
    ls_attribute-kind = &3.
    append ls_attribute to lt_attributes.
  END-OF-DEFINITION.


  fill_att mv_errty 'Error Type' ''.                        "#EC NOTEXT

  cl_ci_query_attributes=>generic(
                        p_name       = myname
                        p_title      = 'Options'
                        p_attributes = lt_attributes
                        p_display    = p_display ).         "#EC NOTEXT
  IF mv_errty = c_error OR mv_errty = c_warning OR mv_errty = c_note.
    attributes_ok = abap_true.
  ELSE.
    attributes_ok = abap_false.
  ENDIF.

ENDMETHOD.


METHOD put_attributes.

  IMPORT
    mv_errty = mv_errty
    FROM DATA BUFFER p_attributes.                   "#EC CI_USE_WANTED

ENDMETHOD.
ENDCLASS.