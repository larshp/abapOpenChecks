CLASS zcl_aoc_super_root DEFINITION
  PUBLIC
  INHERITING FROM cl_ci_test_root
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    TYPE-POOLS zzaoc.

    METHODS get_attributes
        REDEFINITION.
    METHODS if_ci_test~display_documentation
        REDEFINITION.
    METHODS if_ci_test~query_attributes
        REDEFINITION.
    METHODS put_attributes
        REDEFINITION.
  PROTECTED SECTION.

    DATA mv_errty TYPE sci_errty.
  PRIVATE SECTION.
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

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.