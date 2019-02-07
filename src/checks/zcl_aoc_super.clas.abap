CLASS zcl_aoc_super DEFINITION
  PUBLIC
  INHERITING FROM cl_ci_test_scan
  ABSTRACT
  CREATE PUBLIC

  GLOBAL FRIENDS zcl_aoc_unit_test .

  PUBLIC SECTION.
    TYPE-POOLS zzaoc .

    TYPES:
      ty_structures_tt TYPE STANDARD TABLE OF sstruc WITH NON-UNIQUE DEFAULT KEY .

    METHODS constructor .
    METHODS check
      IMPORTING
        !it_tokens     TYPE stokesx_tab
        !it_statements TYPE sstmnt_tab
        !it_levels     TYPE slevel_tab
        !it_structures TYPE ty_structures_tt .
    METHODS set_source
      IMPORTING
        !iv_name TYPE level_name
        !it_code TYPE string_table .

    METHODS get_attributes
        REDEFINITION .
    METHODS if_ci_test~display_documentation
        REDEFINITION .
    METHODS if_ci_test~query_attributes
        REDEFINITION .
    METHODS put_attributes
        REDEFINITION .
    METHODS run
        REDEFINITION .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_position,
        row TYPE token_row,
        col TYPE token_col,
      END OF ty_position .
    TYPES:
      BEGIN OF ty_statement,
        str        TYPE string,
        start      TYPE ty_position,
        end        TYPE ty_position,
        include    TYPE programm,
        level      TYPE stmnt_levl,
        count      TYPE i,
        terminator TYPE stmnt_term,
        index      TYPE i,
      END OF ty_statement .
    TYPES:
      ty_statements TYPE STANDARD TABLE OF ty_statement WITH DEFAULT KEY .

    DATA mv_errty TYPE sci_errty .

    METHODS enable_rfc .
    CLASS-METHODS statement_keyword
      IMPORTING
        !iv_number       TYPE stmnt_nr
        !it_statements   TYPE sstmnt_tab
        !it_tokens       TYPE stokesx_tab
      RETURNING
        VALUE(rv_result) TYPE string .
    CLASS-METHODS statement_row
      IMPORTING
        !iv_number       TYPE stmnt_nr
        !it_statements   TYPE sstmnt_tab
        !it_tokens       TYPE stokesx_tab
      RETURNING
        VALUE(rv_result) TYPE token_row .
    METHODS get_source
      IMPORTING
        !is_level      TYPE slevel
      RETURNING
        VALUE(rt_code) TYPE string_table .
    METHODS build_statements
      IMPORTING
        !it_tokens           TYPE stokesx_tab
        !it_statements       TYPE sstmnt_tab
      RETURNING
        VALUE(rt_statements) TYPE ty_statements .
    METHODS is_class_pool
      IMPORTING
        !iv_include    TYPE level_name
      RETURNING
        VALUE(rv_bool) TYPE abap_bool .
    METHODS is_class_definition
      IMPORTING
        !iv_include    TYPE level_name
      RETURNING
        VALUE(rv_bool) TYPE abap_bool .

    METHODS get_include
        REDEFINITION .
    METHODS inform
        REDEFINITION .
  PRIVATE SECTION.

    TYPES:
      BEGIN OF ty_source,
        name TYPE level_name,
        code TYPE string_table,
      END OF ty_source .
    TYPES:
      ty_source_tt TYPE SORTED TABLE OF ty_source WITH UNIQUE KEY name .

    DATA mt_source TYPE ty_source_tt .

    CLASS-METHODS token_position
      IMPORTING
        !is_token          TYPE stokesx
      RETURNING
        VALUE(rs_position) TYPE ty_position .
    METHODS check_class
      IMPORTING
        !iv_sub_obj_name TYPE sobj_name
      RETURNING
        VALUE(rv_skip)   TYPE abap_bool .
    METHODS check_wdy
      IMPORTING
        !iv_sub_obj_type TYPE trobjtype
        !iv_sub_obj_name TYPE sobj_name
        !iv_line         TYPE token_row
      RETURNING
        VALUE(rv_skip)   TYPE abap_bool .
ENDCLASS.



CLASS ZCL_AOC_SUPER IMPLEMENTATION.


  METHOD build_statements.

    DATA: lv_str   TYPE string,
          ls_start TYPE ty_position,
          ls_end   TYPE ty_position,
          lv_index TYPE i,
          lv_count TYPE i.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                   <ls_token>     LIKE LINE OF it_tokens,
                   <ls_add>       LIKE LINE OF rt_statements.


    LOOP AT it_statements ASSIGNING <ls_statement>
        WHERE type <> scan_stmnt_type-empty
        AND type <> scan_stmnt_type-comment
        AND type <> scan_stmnt_type-comment_in_stmnt
        AND type <> scan_stmnt_type-pragma.
      lv_index = sy-tabix.

      CLEAR lv_str.
      lv_count = 0.

      LOOP AT it_tokens ASSIGNING <ls_token>
          FROM <ls_statement>-from TO <ls_statement>-to.
        IF lv_str IS INITIAL.
          lv_str = <ls_token>-str.
          ls_start = token_position( <ls_token> ).
        ELSE.
          CONCATENATE lv_str <ls_token>-str INTO lv_str SEPARATED BY space.
        ENDIF.
        lv_count = lv_count + 1.
        ls_end = token_position( <ls_token> ).
      ENDLOOP.
      IF sy-subrc = 0.
        APPEND INITIAL LINE TO rt_statements ASSIGNING <ls_add>.
        <ls_add>-str        = lv_str.
        <ls_add>-include    = get_include( p_level = <ls_statement>-level ).
        <ls_add>-level      = <ls_statement>-level.
        <ls_add>-start      = ls_start.
        <ls_add>-end        = ls_end.
        <ls_add>-count      = lv_count.
        <ls_add>-index      = lv_index.
        <ls_add>-terminator = <ls_statement>-terminator.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD check.

* add code here
    ASSERT 0 = 1.

  ENDMETHOD.


  METHOD check_class.

    DATA: lv_category TYPE seoclassdf-category,
          lv_proxy    TYPE seoclassdf-clsproxy,
          lv_abstract TYPE seoclassdf-clsabstrct,
          lv_super    TYPE seometarel-refclsname,
          ls_mtdkey   TYPE seocpdkey.


    IF object_type <> 'CLAS'
        AND object_type <> 'INTF'.
      RETURN.
    ENDIF.

    SELECT SINGLE category clsproxy clsabstrct FROM seoclassdf
      INTO (lv_category, lv_proxy, lv_abstract)
      WHERE clsname = object_name
      AND version = '1'.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

* skip persistent co-classes and web dynpro runtime objects
    IF lv_category = seoc_category_p_agent
        OR lv_category = seoc_category_webdynpro_class
        OR lv_proxy = abap_true.
      rv_skip = abap_true.
      RETURN.
    ENDIF.

* skip constructor in exception classes
    IF lv_category = seoc_category_exception.
      cl_oo_classname_service=>get_method_by_include(
        EXPORTING
          incname             = iv_sub_obj_name
        RECEIVING
          mtdkey              = ls_mtdkey
        EXCEPTIONS
          class_not_existing  = 1
          method_not_existing = 2
          OTHERS              = 3 ).
      IF sy-subrc = 0 AND ls_mtdkey-cpdname = 'CONSTRUCTOR'.
        rv_skip = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

* skip BOPF constants interfaces
    IF object_type = 'INTF' AND object_name CP '*_C'.
      SELECT SINGLE refclsname FROM seometarel INTO lv_super
        WHERE clsname = object_name AND reltype = '0'.
      IF sy-subrc = 0 AND lv_super = '/BOBF/IF_LIB_CONSTANTS'.
        rv_skip = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

* skip classes generated by Gateway Builder/SEGW
    IF ( lv_abstract = abap_true AND object_name CP '*_DPC' )
        OR object_name CP '*_MPC'.
      SELECT SINGLE refclsname FROM seometarel INTO lv_super
        WHERE clsname = object_name AND reltype = '2'.
      IF sy-subrc = 0
          AND ( lv_super = '/IWBEP/CL_MGW_PUSH_ABS_MODEL'
          OR lv_super = '/IWBEP/CL_MGW_PUSH_ABS_DATA' ).
        rv_skip = abap_true.
        RETURN.
      ENDIF.
    ENDIF.

* skip objects generated by SADL toolkit
    IF lv_super = 'CL_SADL_GTK_EXPOSURE_MPC'.
      rv_skip = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD check_wdy.

    DATA: ls_map_header TYPE wdy_wb_sourcemap,
          lo_tool_state TYPE REF TO cl_wdy_wb_vc_state,
          lv_inclname   TYPE program,
          ls_controller TYPE wdy_controller_key,
          lt_map        TYPE wdyrt_line_info_tab_type,
          lv_no_codepos TYPE seu_bool.


    IF iv_sub_obj_type <> 'PROG' OR iv_sub_obj_name(8) <> '/1BCWDY/'.
      RETURN.
    ENDIF.

    lv_inclname = iv_sub_obj_name.
    CALL FUNCTION 'WDY_WB_GET_SOURCECODE_MAPPING'
      EXPORTING
        p_include = lv_inclname
      IMPORTING
        p_map     = lt_map
        p_header  = ls_map_header
      EXCEPTIONS
        OTHERS    = 1.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    ls_controller-component_name  = ls_map_header-component_name.
    ls_controller-controller_name = ls_map_header-controller_name.
    cl_wdy_wb_error_handling=>create_tool_state_for_codepos(
      EXPORTING
        p_controller_key           = ls_controller
        p_controller_type          = ls_map_header-ctrl_type
        p_line                     = iv_line
        p_lineinfo                 = lt_map
      IMPORTING
        p_no_corresponding_codepos = lv_no_codepos
        p_tool_state               = lo_tool_state ).
    IF lv_no_codepos = abap_true OR lo_tool_state IS INITIAL.
      rv_skip = abap_true.
    ENDIF.

  ENDMETHOD.


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


  METHOD get_include.

    IF p_level = 0.
* in case INCLUDE doesnt exist in the system
      RETURN.
    ENDIF.

    IF ref_scan IS BOUND.
* not bound during unit testing
      p_result = super->get_include(
          p_ref_scan = p_ref_scan
          p_level    = p_level ).
    ENDIF.

  ENDMETHOD.


  METHOD get_source.

    DATA: ls_source LIKE LINE OF mt_source.

    FIELD-SYMBOLS: <ls_source> LIKE LINE OF mt_source.


    IF is_level-type = scan_level_type-macro_define
        OR is_level-type = scan_level_type-macro_trmac.
      RETURN.
    ENDIF.

    READ TABLE mt_source ASSIGNING <ls_source> WITH KEY name = is_level-name.
    IF sy-subrc = 0.
      rt_code = <ls_source>-code.
    ELSE.
      READ REPORT is_level-name INTO rt_code.          "#EC CI_READ_REP
      ASSERT sy-subrc = 0.

      ls_source-name = is_level-name.
      ls_source-code = rt_code.
      INSERT ls_source INTO TABLE mt_source.
    ENDIF.

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


  METHOD inform.

    DATA: lv_cnam TYPE reposrc-cnam,
          lv_area TYPE tvdir-area,
          lv_skip TYPE abap_bool.

    FIELD-SYMBOLS: <ls_message> LIKE LINE OF scimessages.


    IF p_sub_obj_type = 'PROG' AND p_sub_obj_name <> ''.
      IF p_sub_obj_name CP 'MP9+++BI' OR p_sub_obj_name CP 'MP9+++00'.
        RETURN. " custom HR infotype includes
      ENDIF.

      IF cl_enh_badi_def_utility=>is_sap_system( ) = abap_false.
        SELECT SINGLE cnam FROM reposrc INTO lv_cnam
          WHERE progname = p_sub_obj_name AND r3state = 'A'.
        IF sy-subrc = 0
            AND ( lv_cnam = 'SAP'
            OR lv_cnam = 'SAP*'
            OR lv_cnam = 'DDIC' ).
          RETURN.
        ENDIF.
      ENDIF.
    ENDIF.

    IF object_type = 'SSFO'
        AND p_sub_obj_type = 'PROG'
        AND ( p_sub_obj_name CP '/1BCDWB/LSF*'
        OR p_sub_obj_name CP '/1BCDWB/SAPL*' ).
      RETURN.
    ENDIF.

    IF object_type = 'FUGR'.
      IF p_sub_obj_name CP 'LY*UXX' OR p_sub_obj_name CP 'LZ*UXX'.
        RETURN.
      ENDIF.
      SELECT SINGLE area FROM tvdir INTO lv_area
        WHERE area = object_name ##WARN_OK.             "#EC CI_GENBUFF
      IF sy-subrc = 0.
        RETURN.
      ENDIF.
    ENDIF.

    lv_skip = check_class( p_sub_obj_name ).
    IF lv_skip = abap_true.
      RETURN.
    ENDIF.

    lv_skip = check_wdy( iv_sub_obj_type = p_sub_obj_type
                         iv_sub_obj_name = p_sub_obj_name
                         iv_line         = p_line ).
    IF lv_skip = abap_true.
      RETURN.
    ENDIF.

    READ TABLE scimessages ASSIGNING <ls_message>
      WITH KEY test = myname code = p_code.
    IF sy-subrc = 0.
      <ls_message>-kind = p_kind.
    ENDIF.
    IF sy-subrc = 0 AND NOT mt_source IS INITIAL.
      READ TABLE mt_source
        WITH KEY name = '----------------------------------------'
        TRANSPORTING NO FIELDS.
      IF sy-subrc = 0 AND lines( mt_source ) = 1.
* fix failing unit tests
        CLEAR <ls_message>-pcom.
      ENDIF.
    ENDIF.

    super->inform(
        p_sub_obj_type = p_sub_obj_type
        p_sub_obj_name = p_sub_obj_name
        p_position     = p_position
        p_line         = p_line
        p_column       = p_column
        p_errcnt       = p_errcnt
        p_kind         = p_kind
        p_test         = p_test
        p_code         = p_code
        p_suppress     = p_suppress
        p_param_1      = p_param_1
        p_param_2      = p_param_2
        p_param_3      = p_param_3
        p_param_4      = p_param_4
        p_inclspec     = p_inclspec ).
* parameters p_detail and p_checksum_1 does not exist in 730

  ENDMETHOD.


  METHOD is_class_definition.

    IF strlen( iv_include ) = 32
        AND ( object_type = 'CLAS' OR object_type = 'INTF' )
        AND ( iv_include+30(2) = 'CO'
        OR iv_include+30(2) = 'CI'
        OR iv_include+30(2) = 'CU'
        OR iv_include+30(2) = 'IU' ).
      rv_bool = abap_true.
    ELSE.
      rv_bool = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD is_class_pool.

    IF strlen( iv_include ) = 32
        AND ( ( object_type = 'CLAS'
        AND iv_include+30(2) = 'CP' )
        OR ( object_type = 'INTF'
        AND iv_include+30(2) = 'IP' ) ).
      rv_bool = abap_true.
    ELSE.
      rv_bool = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD run.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    CLEAR mt_source.  " limit memory use

    IF program_name IS INITIAL.
      RETURN.
    ENDIF.
    IF ref_scan IS INITIAL AND get( ) <> abap_true.
      RETURN.
    ENDIF.

    IF ref_include IS BOUND.
* ref_include is not set when running checks via RFC
      set_source( iv_name = ref_include->trdir-name
                  it_code = ref_include->lines ).
    ENDIF.

    check( it_tokens     = ref_scan->tokens
           it_statements = ref_scan->statements
           it_levels     = ref_scan->levels
           it_structures = ref_scan->structures ).

  ENDMETHOD.


  METHOD set_source.

* used for unit testing

    DATA: ls_source LIKE LINE OF mt_source.


    ls_source-name = iv_name.
    ls_source-code = it_code.

    INSERT ls_source INTO TABLE mt_source.

  ENDMETHOD.


  METHOD statement_keyword.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                   <ls_token>     LIKE LINE OF it_tokens.


    READ TABLE it_statements ASSIGNING <ls_statement> INDEX iv_number.
    ASSERT sy-subrc = 0.

    IF <ls_statement>-from <= <ls_statement>-to.
      READ TABLE it_tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
      ASSERT sy-subrc = 0.

      rv_result = <ls_token>-str.
    ENDIF.

  ENDMETHOD.


  METHOD statement_row.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF it_statements,
                   <ls_token>     LIKE LINE OF it_tokens.


    READ TABLE it_statements ASSIGNING <ls_statement> INDEX iv_number.
    ASSERT sy-subrc = 0.

    READ TABLE it_tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
    ASSERT sy-subrc = 0.

    rv_result = <ls_token>-row.

  ENDMETHOD.


  METHOD token_position.

    rs_position-col = is_token-col.
    rs_position-row = is_token-row.

  ENDMETHOD.
ENDCLASS.
