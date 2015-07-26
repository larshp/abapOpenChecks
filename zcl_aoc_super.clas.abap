CLASS zcl_aoc_super DEFINITION
  PUBLIC
  INHERITING FROM cl_ci_test_scan
  ABSTRACT
  CREATE PUBLIC .

  PUBLIC SECTION.
*"* public components of class ZCL_AOC_SUPER
*"* do not include other source files here!!!

    TYPES:
      tt_structures TYPE STANDARD TABLE OF sstruc WITH NON-UNIQUE DEFAULT KEY.

    METHODS check
      IMPORTING
        !it_tokens TYPE stokesx_tab
        !it_statements TYPE sstmnt_tab
        !it_levels TYPE slevel_tab
        !it_structures TYPE tt_structures .
    METHODS set_source
      IMPORTING
        !iv_name TYPE level_name
        !it_code TYPE string_table .

    METHODS get_attributes
      REDEFINITION .
    METHODS if_ci_test~query_attributes
      REDEFINITION .
    METHODS put_attributes
      REDEFINITION .
    METHODS run
      REDEFINITION .
    METHODS if_ci_test~display_documentation
      REDEFINITION .
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
*"* private components of class ZCL_AOC_SUPER
*"* do not include other source files here!!!

  types:
    BEGIN OF st_source,
           name TYPE level_name,
           code TYPE string_table,
         END OF st_source .
  types:
    tt_source TYPE SORTED TABLE OF st_source WITH UNIQUE KEY name .

  data MT_SOURCE type TT_SOURCE .
ENDCLASS.



CLASS ZCL_AOC_SUPER IMPLEMENTATION.


METHOD check.

* add code here
  ASSERT 1 = 1 + 1.

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

  cl_gui_frontend_services=>execute( document = lv_url ).

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
                          p_name       = myname
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


METHOD inform.

  DATA: li_oref    TYPE REF TO if_oo_class_incl_naming,
        li_clif    TYPE REF TO if_oo_clif_incl_naming,
        lv_clsname TYPE seoclassdf-clsname,
        lv_cnam    TYPE reposrc-cnam,
        lv_method  TYPE seocpdname.


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

* skip constructor in exception classes
  IF p_sub_obj_type = 'PROG' AND strlen( p_sub_obj_name ) = 35.
    cl_oo_include_naming=>get_instance_by_include(
      EXPORTING
        progname = p_sub_obj_name
      RECEIVING
        cifref   = li_clif
      EXCEPTIONS
        OTHERS   = 1 ).
    IF sy-subrc = 0.
      li_oref ?= li_clif.

      SELECT SINGLE clsname FROM seoclassdf INTO lv_clsname
        WHERE clsname = li_oref->clskey-clsname
        AND category = '11'     " persistent co-class
        AND version = '1'.
      IF sy-subrc = 0.
        RETURN.
      ENDIF.

      SELECT SINGLE clsname FROM seoclassdf INTO lv_clsname
        WHERE clsname = li_oref->clskey-clsname
        AND category = '40'     " exception
        AND version = '1'.
      IF sy-subrc = 0.
        li_oref->get_mtdname_by_include(
          EXPORTING
            progname = p_sub_obj_name
          RECEIVING
            mtdname  = lv_method
          EXCEPTIONS
            OTHERS   = 1 ).
        IF sy-subrc = 0 AND lv_method = 'CONSTRUCTOR'.
          RETURN.
        ENDIF.
      ENDIF.
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
      p_inclspec     = p_inclspec
* parameters p_detail and p_checksum_1 does not exist in 730
      ).

ENDMETHOD.


METHOD put_attributes.

  IMPORT
    mv_errty = mv_errty
    FROM DATA BUFFER p_attributes.                   "#EC CI_USE_WANTED

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