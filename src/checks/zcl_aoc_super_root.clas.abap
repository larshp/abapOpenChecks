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

    TYPES:
      ty_scimessage_text TYPE c LENGTH 255 .

    DATA mv_errty TYPE sci_errty .

    METHODS enable_rfc .
    METHODS set_kind .
    CLASS-METHODS get_destination
      RETURNING
        VALUE(rv_result) TYPE rfcdest .
    METHODS insert_scimessage
      IMPORTING
        !iv_code TYPE scimessage-code
        !iv_text TYPE ty_scimessage_text
        !iv_pcom TYPE scimessage-pcom OPTIONAL .
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
        WHERE clsname = myname.           "#EC CI_NOORDER "#EC CI_SUBRC
    ENDIF.

    category = 'ZCL_AOC_CATEGORY'.
    mv_errty = 'E'.

  ENDMETHOD.


  METHOD enable_rfc.
* RFC enable the check, new feature for central ATC on 7.51

    FIELD-SYMBOLS: <lv_rfc> TYPE abap_bool.


    ASSIGN ('REMOTE_RFC_ENABLED') TO <lv_rfc>.
    IF sy-subrc = 0.
      <lv_rfc> = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT mv_errty = mv_errty TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD get_destination.

    "get destination of calling system (RFC enabled checks only)
    "class, method and variable may not valid in 7.02 systems -> dynamic calls
    CONSTANTS lc_classname TYPE seoclsname VALUE 'CL_ABAP_SOURCE_ID'.
    CONSTANTS lc_methodname TYPE seocpdname VALUE 'GET_DESTINATION'.

    FIELD-SYMBOLS: <lv_srcid> TYPE sysuuid_x.

    ASSIGN ('SRCID') TO <lv_srcid>.

    IF NOT <lv_srcid> IS ASSIGNED.
      rv_result = |NONE|.
      RETURN.
    ENDIF.

    IF <lv_srcid> IS INITIAL.
      rv_result = |NONE|.
      RETURN.
    ENDIF.

    TRY.
        CALL METHOD (lc_classname)=>(lc_methodname)
          EXPORTING
            p_srcid       = <lv_srcid>
          RECEIVING
            p_destination = rv_result
          EXCEPTIONS
            not_found     = 1.

        IF sy-subrc <> 0.
          rv_result = |NONE|.
        ENDIF.

      CATCH cx_sy_dyn_call_illegal_class
            cx_sy_dyn_call_illegal_method.
        rv_result = |NONE|.
    ENDTRY.

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


  METHOD insert_scimessage.
* Insert entry into table scimessages, this table is used to determine the message text for a finding.
    DATA ls_scimessage LIKE LINE OF scimessages.

    ls_scimessage-test = myname.
    ls_scimessage-code = iv_code.
    ls_scimessage-kind = mv_errty.
    ls_scimessage-text = iv_text.
    ls_scimessage-pcom = iv_pcom.

    INSERT ls_scimessage INTO TABLE scimessages.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

    set_kind( ).

  ENDMETHOD.


  METHOD set_kind.

    FIELD-SYMBOLS: <ls_message> LIKE LINE OF scimessages.

    LOOP AT scimessages ASSIGNING <ls_message>.
      <ls_message>-kind = mv_errty.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
