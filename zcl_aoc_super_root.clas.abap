class ZCL_AOC_SUPER_ROOT definition
  public
  inheriting from CL_CI_TEST_ROOT
  abstract
  create public .

public section.
  type-pools ZZAOC .

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

  zzaoc_top.

  zzaoc_fill_att mv_errty 'Error Type' ''.                  "#EC NOTEXT

  zzaoc_popup.

ENDMETHOD.


METHOD put_attributes.

  IMPORT
    mv_errty = mv_errty
    FROM DATA BUFFER p_attributes.                   "#EC CI_USE_WANTED
  ASSERT sy-subrc = 0.

ENDMETHOD.
ENDCLASS.