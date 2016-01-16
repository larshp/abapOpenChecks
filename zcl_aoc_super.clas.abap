class ZCL_AOC_SUPER definition
  public
  inheriting from CL_CI_TEST_SCAN
  abstract
  create public .

public section.
  type-pools ZZAOC .

  types:
*"* public components of class ZCL_AOC_SUPER
*"* do not include other source files here!!!
    ty_structures_tt TYPE STANDARD TABLE OF sstruc WITH NON-UNIQUE DEFAULT KEY .

  methods CHECK
    importing
      !IT_TOKENS type STOKESX_TAB
      !IT_STATEMENTS type SSTMNT_TAB
      !IT_LEVELS type SLEVEL_TAB
      !IT_STRUCTURES type TY_STRUCTURES_TT .
  methods SET_SOURCE
    importing
      !IV_NAME type LEVEL_NAME
      !IT_CODE type STRING_TABLE .
  class-methods GET_COMPILER
    returning
      value(RO_COMPILER) type ref to CL_ABAP_COMPILER .

  methods GET_ATTRIBUTES
    redefinition .
  methods IF_CI_TEST~DISPLAY_DOCUMENTATION
    redefinition .
  methods IF_CI_TEST~QUERY_ATTRIBUTES
    redefinition .
  methods PUT_ATTRIBUTES
    redefinition .
  methods RUN
    redefinition .
protected section.
*"* protected components of class ZCL_AOC_SUPER
*"* do not include other source files here!!!

  data MV_ERRTY type SCI_ERRTY .

  class-methods STATEMENT_KEYWORD
    importing
      !IV_NUMBER type STMNT_NR
      !IT_STATEMENTS type SSTMNT_TAB
      !IT_TOKENS type STOKESX_TAB
    returning
      value(RV_RESULT) type STRING .
  class-methods STATEMENT_ROW
    importing
      !IV_NUMBER type STMNT_NR
      !IT_STATEMENTS type SSTMNT_TAB
      !IT_TOKENS type STOKESX_TAB
    returning
      value(RV_RESULT) type TOKEN_ROW .
  methods GET_SOURCE
    importing
      !IS_LEVEL type SLEVEL
    returning
      value(RT_CODE) type STRING_TABLE .

  methods GET_INCLUDE
    redefinition .
  methods INFORM
    redefinition .
private section.

  types:
*"* private components of class ZCL_AOC_SUPER
*"* do not include other source files here!!!
    BEGIN OF ty_source,
           name TYPE level_name,
           code TYPE string_table,
         END OF ty_source .
  types:
    ty_source_tt TYPE SORTED TABLE OF ty_source WITH UNIQUE KEY name .

  data MT_SOURCE type ty_source_tt .

  methods CHECK_CLASS
    importing
      !IV_SUB_OBJ_NAME type SOBJ_NAME
    returning
      value(RV_SKIP) type ABAP_BOOL .
  methods CHECK_WDY
    importing
      !IV_SUB_OBJ_TYPE type TROBJTYPE
      !IV_SUB_OBJ_NAME type SOBJ_NAME
      !IV_LINE type TOKEN_ROW
    returning
      value(RV_SKIP) type ABAP_BOOL .
ENDCLASS.



CLASS ZCL_AOC_SUPER IMPLEMENTATION.


METHOD check.

* add code here
  ASSERT 1 = 1 + 1.

ENDMETHOD.


METHOD check_class.

  DATA: lv_category TYPE seoclassdf-category,
        lv_proxy    TYPE seoclassdf-clsproxy,
        ls_mtdkey   TYPE seocpdkey.


  IF object_type <> 'CLAS'
      AND object_type <> 'INTF'.
    RETURN.
  ENDIF.

  SELECT SINGLE category clsproxy FROM seoclassdf
    INTO (lv_category, lv_proxy)
    WHERE clsname = object_name
    AND version = '1'.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

* skip persistent co-classes and web dynpro runtime obects
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
    ENDIF.
  ENDIF.

ENDMETHOD.


METHOD check_wdy.

  DATA: ls_map_header  TYPE wdy_wb_sourcemap,
        lo_tool_state  TYPE REF TO cl_wdy_wb_vc_state,
        lv_inclname    TYPE program,
        ls_controller  TYPE wdy_controller_key,
        lt_map         TYPE wdyrt_line_info_tab_type,
        lv_no_codepos  TYPE seu_bool.


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


METHOD get_attributes.

  EXPORT mv_errty = mv_errty TO DATA BUFFER p_attributes.

ENDMETHOD.


METHOD get_compiler.

  DATA: lv_class TYPE seoclsname,
        lv_name  TYPE program.


  CASE object_type.
    WHEN 'PROG'.
      lv_name = object_name.
    WHEN 'CLAS'.
      lv_class = object_name.
      lv_name = cl_oo_classname_service=>get_classpool_name( lv_class ).
    WHEN 'FUGR'.
      CONCATENATE 'SAPL' object_name INTO lv_name.
    WHEN OTHERS.
      RETURN.
  ENDCASE.

  ro_compiler = cl_abap_compiler=>create(
    p_name             = lv_name
    p_no_package_check = abap_true ).

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
    READ REPORT is_level-name INTO rt_code.            "#EC CI_READ_REP
    ASSERT sy-subrc = 0.

    ls_source-name = is_level-name.
    ls_source-code = rt_code.
    INSERT ls_source INTO TABLE mt_source.
  ENDIF.

ENDMETHOD.


METHOD if_ci_test~display_documentation.

  DATA: lv_url TYPE string VALUE 'https://github.com/larshp/abapOpenChecks/wiki/'.


  CONCATENATE lv_url myname INTO lv_url.

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
      OTHERS                 = 10 ).                      "#EC CI_SUBRC

ENDMETHOD.


METHOD if_ci_test~query_attributes.

  zzaoc_top.

  zzaoc_fill_att mv_errty 'Error Type' ''.                  "#EC NOTEXT

  zzaoc_popup.

ENDMETHOD.


METHOD inform.

  DATA: lv_cnam TYPE reposrc-cnam,
        lv_area TYPE tvdir-area,
        lv_skip TYPE abap_bool.

  FIELD-SYMBOLS: <ls_message> LIKE LINE OF scimessages.


  IF p_sub_obj_type = 'PROG' AND p_sub_obj_name <> ''.
    SELECT SINGLE cnam FROM reposrc INTO lv_cnam
      WHERE progname = p_sub_obj_name AND r3state = 'A'.
    IF sy-subrc = 0
        AND ( lv_cnam = 'SAP'
        OR lv_cnam = 'SAP*'
        OR lv_cnam = 'DDIC' ).
      RETURN.
    ENDIF.
  ENDIF.

  IF object_type = 'FUGR'.
    SELECT SINGLE area FROM tvdir INTO lv_area
      WHERE area = object_name ##WARN_OK.
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


METHOD put_attributes.

  IMPORT
    mv_errty = mv_errty
    FROM DATA BUFFER p_attributes.                   "#EC CI_USE_WANTED
  ASSERT sy-subrc = 0.

ENDMETHOD.


METHOD run.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  CLEAR mt_source[].  " limit memory use

  IF program_name IS INITIAL.
    RETURN.
  ENDIF.
  IF ref_scan IS INITIAL AND get( ) <> abap_true.
    RETURN.
  ENDIF.

  set_source( iv_name = ref_include->trdir-name
              it_code = ref_include->lines ).

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
ENDCLASS.