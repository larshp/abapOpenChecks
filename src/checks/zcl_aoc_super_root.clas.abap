CLASS zcl_aoc_super_root DEFINITION
  PUBLIC
  INHERITING FROM cl_ci_test_root
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPE-POOLS zzaoc .

    METHODS constructor .

    METHODS get_attributes
         REDEFINITION .
    METHODS if_ci_test~display_documentation
         REDEFINITION .
    METHODS if_ci_test~query_attributes
         REDEFINITION .
    METHODS put_attributes
         REDEFINITION .
  PROTECTED SECTION.

    DATA mv_errty TYPE sci_errty.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_SUPER_ROOT IMPLEMENTATION.


  METHOD constructor.
    super->constructor( ).

    "get description of check class
    SELECT SINGLE descript FROM seoclasstx INTO description
      WHERE clsname = myname
      AND langu   = sy-langu.
    IF sy-subrc <> 0.
      SELECT SINGLE descript FROM seoclasstx INTO description
        WHERE clsname = myname.                           "#EC CI_SUBRC
    ENDIF.

    category = 'ZCL_AOC_CATEGORY'.
  ENDMETHOD.


  METHOD get_attributes.

    EXPORT mv_errty = mv_errty TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD if_ci_test~display_documentation.

    DATA: lv_url TYPE string VALUE 'http://docs.abapopenchecks.org/checks/' ##NO_TEXT,
          lv_len TYPE i.


    lv_len = strlen( myname ) - 2.

    CONCATENATE lv_url myname+lv_len(2) INTO lv_url.

    cl_gui_frontend_services=>execute(
      EXPORTING
        document               = lv_url
      EXCEPTIONS
        cntl_error             = 1
        error_no_gui           = 2
        bad_parameter          = 3
        file_not_found         = 4
        path_not_found         = 5
        file_extension_unknown = 6
        error_execute_failed   = 7
        synchronous_failed     = 8
        not_supported_by_gui   = 9
        OTHERS                 = 10 ).                    "#EC CI_SUBRC

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
