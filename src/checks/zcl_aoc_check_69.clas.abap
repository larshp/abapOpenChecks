CLASS zcl_aoc_check_69 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
    METHODS get_attributes
        REDEFINITION .
    METHODS get_message_text
        REDEFINITION .
    METHODS if_ci_test~query_attributes
        REDEFINITION .
    METHODS put_attributes
        REDEFINITION .
protected section.

  methods DETERMINE_SCOPE_PREFIX
    returning
      value(RV_PREFIX) type STRING .
  methods COMPILER_RESOLVE_CLASS
    returning
      value(RO_CLASS) type ref to CL_ABAP_COMP_CLASS .
  methods COMPILER_RESOLVE
    importing
      !IV_NAME type STRING
    returning
      value(RO_GENERIC) type ref to CL_ABAP_COMP_DATA_GENERIC .
  methods COMPARE
    importing
      !IV_NAME type STRING
      !IV_REGEX type STRING
      !IV_RELATIVE type I .
  methods GET_STATEMENT
    returning
      value(RV_STRING) type STRING .
  methods QUALIFY_TOKENS
    returning
      value(RT_TOKENS) type STOKESX_TAB .
  methods REMOVE_VALUE
    importing
      !IV_INPUT type STRING
    returning
      value(RV_OUTPUT) type STRING .
  methods SKIP_FM_PARAMETERS
    importing
      !IS_PARAMETERS type RSFBINTFV
    returning
      value(RV_SKIP) type ABAP_BOOL .
  methods SKIP_FM_PARAMETERS_CHECK
    importing
      !IS_PARAMETERS type RSFBINTFV
      !IS_CHECK type RSFBINTFV
    returning
      value(RV_SKIP) type ABAP_BOOL .
  methods DETERMINE_TYPE_PREFIX
    importing
      !IO_GENERIC type ref to CL_ABAP_COMP_DATA_GENERIC
    returning
      value(RV_PREFIX) type STRING .
  methods CHECK_FM_PARAMETERS
    importing
      !IT_PARAMETERS type RSFB_PARA
      !IV_PREFIX type STRING .
  methods CHECK_AT .
  methods ANALYZE_STATEMENTS
    importing
      !IT_STATEMENTS type SSTMNT_TAB .
  methods CHECK_METHOD_DEFINITION .
  methods CHECK_METHOD_IMPLEMENTATION .
  methods CHECK_CLASS .
  methods CHECK_CONSTANT .
  methods CHECK_DATA .
  methods CHECK_FIELD_SYMBOL .
  methods CHECK_FORM .
  methods CHECK_FUNCTION .
  methods CHECK_FUNCTION_POOL .
  methods CHECK_INLINE_DEFS .
  methods CHECK_INTERFACE .
  methods CHECK_PARAMETER .
  methods CHECK_REPORT .
  methods CHECK_SELECT_OPTION .
  methods CHECK_TYPE .
  methods SET_DEFAULTS .
private section.

  data MS_NAMING type ZAOC_NAMING .
  data MO_COMPILER type ref to CL_ABAP_COMPILER .
  data MO_STACK type ref to LCL_STACK .
  data MV_BEGIN type ABAP_BOOL .
  data MV_AT type STRING .
ENDCLASS.



CLASS ZCL_AOC_CHECK_69 IMPLEMENTATION.


  METHOD analyze_statements.

    DATA: lv_define  TYPE abap_bool,
          lv_keyword TYPE string.


    LOOP AT it_statements INTO statement_wa.
      CHECK statement_wa-from <= statement_wa-to.

      lv_keyword = keyword( ).
      IF lv_define = abap_true.
        IF lv_keyword = 'END-OF-DEFINITION'.
          lv_define = abap_false.
        ENDIF.
        CONTINUE.
      ENDIF.

      CASE lv_keyword.
        WHEN 'DEFINE'.
          lv_define = abap_true.
        WHEN 'REPORT' OR 'PROGRAM'.
          check_report( ).
        WHEN 'FUNCTION-POOL'.
          check_function_pool( ).
        WHEN 'AT'.
          check_at( ).
        WHEN 'TYPES'.
          check_type( ).
        WHEN 'DATA' OR 'RANGES' OR 'STATICS' OR 'CLASS-DATA'.
          check_data( ).
        WHEN 'CONSTANTS'.
          check_constant( ).
        WHEN 'FIELD-SYMBOLS'.
          check_field_symbol( ).
        WHEN 'SELECT-OPTIONS'.
          check_select_option( ).
        WHEN 'PARAMETERS' OR 'PARAMETER'.
          check_parameter( ).
        WHEN 'CLASS'.
          check_class( ).
        WHEN 'INTERFACE'.
          check_interface( ).
        WHEN 'FORM'.
          check_form( ).
        WHEN 'FUNCTION'.
          check_function( ).
        WHEN 'METHODS' OR 'CLASS-METHODS'.
          check_method_definition( ).
        WHEN 'METHOD'.
          check_method_implementation( ).
        WHEN 'ENDCLASS' OR 'ENDMETHOD' OR 'ENDFORM' OR 'ENDINTERFACE'.
          mo_stack->pop( ).
        WHEN 'ENDFUNCTION'.
          IF object_type = 'FUGR'.
            mo_stack->set( '\PR:SAPL' && object_name ).
          ELSE.
            mo_stack->set( '\PR:' && object_name ).
          ENDIF.
        WHEN OTHERS.
          check_inline_defs( ).
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_subrc LIKE sy-subrc,
          lv_subc  TYPE reposrc-subc.


    IF object_type = 'WDYN'.
      RETURN. " todo
    ELSEIF object_type = 'PROG'.
      SELECT SINGLE subc FROM reposrc INTO lv_subc
        WHERE progname = object_name AND r3state = 'A'.
      IF sy-subrc <> 0 OR lv_subc = 'I' OR lv_subc = 'S'.
        RETURN.
      ENDIF.
    ENDIF.

    CREATE OBJECT mo_stack.
    mo_compiler = cl_abap_compiler=>create( program_name ).

    mo_compiler->get_check_infos( IMPORTING p_subrc = lv_subrc ).
    IF lv_subrc <> 0.
      inform( p_kind = mv_errty
              p_test = myname
              p_code = '005' ).
      RETURN.
    ENDIF.

    analyze_statements( it_statements ).

  ENDMETHOD.


  METHOD check_at.

    IF get_token_rel( 2 ) <> 'SELECTION-SCREEN'.
      RETURN.
    ENDIF.

    CASE get_token_rel( 3 ).
      WHEN 'OUTPUT'.
        mv_at = cl_abap_compiler=>tag_at_selection_screen_output.
      WHEN 'ON'.
        CASE get_token_rel( 4 ).
          WHEN 'END'.
            mv_at = |{ cl_abap_compiler=>tag_at_selection_screen_on_end }:{ get_token_rel( 6 ) }|.
          WHEN 'BLOCK'.
            mv_at = |{ cl_abap_compiler=>tag_at_selection_screen_block }:{ get_token_rel( 5 ) }|.
          WHEN 'RADIOBUTTON'.
            mv_at = |{ cl_abap_compiler=>tag_at_selection_screen_radio }:{ get_token_rel( 6 ) }|.
          WHEN 'HELP-REQUEST'.
            mv_at = |{ cl_abap_compiler=>tag_at_selection_screen_help }:{ get_token_rel( 6 ) }|.
          WHEN 'VALUE-REQUEST'.
            mv_at = |{ cl_abap_compiler=>tag_at_selection_screen_value }:{ get_token_rel( 6 ) }|.
          WHEN 'EXIT-COMMAND'.
            mv_at = cl_abap_compiler=>tag_at_selection_screen_exit.
          WHEN OTHERS.
            mv_at = |{ cl_abap_compiler=>tag_at_selection_screen_on }:{ get_token_rel( 4 ) }|.
        ENDCASE.
      WHEN OTHERS.
        mv_at = cl_abap_compiler=>tag_at_selection_screen.
    ENDCASE.

    mv_at = '\' && mv_at.

  ENDMETHOD.


  METHOD check_class.

    DATA: lv_name      TYPE string,
          lv_statement TYPE string,
          lo_super     TYPE REF TO cl_abap_comp_class,
          lv_regex     TYPE string.


    CASE get_token_rel( 4 ).
      WHEN 'LOAD' OR 'DEFERRED'.
        RETURN.
    ENDCASE.

    lv_name = get_token_rel( 2 ).

    IF object_type = 'CLAS' AND lv_name = object_name.
      mo_stack->set( '\TY:' && lv_name ).
    ELSEIF object_type = 'CLAS'.
      mo_stack->set( '\PR:' && program_name && '\TY:' && lv_name ).
    ELSE.
      mo_stack->push( '\TY:' && lv_name ).
    ENDIF.

    IF get_token_rel( 3 ) = 'IMPLEMENTATION'.
      RETURN.
    ENDIF.

    lo_super = compiler_resolve_class( ).
    IF lo_super IS INITIAL.
      RETURN.
    ENDIF.
    WHILE NOT lo_super->super_class IS INITIAL.
      lo_super = lo_super->super_class.
    ENDWHILE.

    IF object_name = lv_name AND object_type = 'CLAS'.
      IF lo_super->full_name =  '\TY:CX_ROOT'.
        lv_regex = ms_naming-globals_clasx.
      ELSE.
        lv_regex = ms_naming-globals_clas.
      ENDIF.
      REPLACE FIRST OCCURRENCE OF '[:nspace:]' IN lv_regex WITH ms_naming-globals_nspace.
    ELSE.
      lv_regex = ms_naming-oo_oolcla.
      lv_statement = get_statement( ).
      IF lv_statement CS 'FOR TESTING'.
        lv_regex = ms_naming-oo_ooltcl.
      ENDIF.
    ENDIF.

    compare( iv_name     = lv_name
             iv_regex    = lv_regex
             iv_relative = 2 ).

  ENDMETHOD.


  METHOD check_constant.

* todo

  ENDMETHOD.


  METHOD check_data.

    DATA: lo_generic TYPE REF TO cl_abap_comp_data_generic,
          lv_regex   TYPE string,
          lv_offset  TYPE string,
          lv_name    TYPE string.


    lv_name = get_token_rel( 2 ).

    IF lv_name = 'BEGIN' AND get_token_rel( 3 ) = 'OF' AND mv_begin = abap_false.
      lv_name = get_token_rel( 4 ).
      mv_begin = abap_true.
      IF get_token_rel( 4 ) = 'COMMON' AND get_token_rel( 5 ) = 'PART'.
        RETURN.
      ENDIF.
    ELSEIF lv_name = 'END' AND get_token_rel( 3 ) = 'OF'.
      mv_begin = abap_false.
      RETURN.
    ELSEIF mv_begin = abap_true.
      RETURN.
    ENDIF.

    FIND '(' IN lv_name MATCH OFFSET lv_offset.
    IF sy-subrc = 0.
      lv_name = lv_name(lv_offset).
    ENDIF.

    lo_generic = compiler_resolve( '\DA:' && lv_name ).
    IF lo_generic IS INITIAL.
      RETURN.
    ENDIF.

    lv_regex = determine_scope_prefix( ).

    REPLACE FIRST OCCURRENCE OF '[:type:]'
      IN lv_regex
      WITH determine_type_prefix( lo_generic ).

    compare( iv_name     = lv_name
             iv_regex    = lv_regex
             iv_relative = 2 ).

  ENDMETHOD.


  METHOD check_field_symbol.

    DATA: lo_generic TYPE REF TO cl_abap_comp_data_generic,
          lv_regex   TYPE string,
          lv_offset  TYPE string,
          lv_name    TYPE string.


    lv_name = get_token_rel( 2 ).

    lo_generic = compiler_resolve( '\DA:' && lv_name ).
    IF lo_generic IS INITIAL.
      RETURN.
    ENDIF.

    IF mo_stack->concatenate( ) CS '\ME:'
        OR mo_stack->concatenate( ) CS '\FO:'
        OR mo_stack->concatenate( ) CS '\FU:'.
      lv_regex = ms_naming-locals_fsymbo.
    ELSE.
      lv_regex = ms_naming-proc_pgfisy.
    ENDIF.

    REPLACE FIRST OCCURRENCE OF '[:type:]'
      IN lv_regex
      WITH determine_type_prefix( lo_generic ).

    compare( iv_name     = lv_name
             iv_regex    = lv_regex
             iv_relative = 2 ).

  ENDMETHOD.


  METHOD check_fm_parameters.

    DATA: lv_name      TYPE string,
          lv_regex     TYPE string,
          lo_generic   TYPE REF TO cl_abap_comp_data_generic,
          ls_parameter LIKE LINE OF it_parameters.


    LOOP AT it_parameters INTO ls_parameter.
      lv_regex = iv_prefix.
      lv_name = ls_parameter-parameter.

      lo_generic = compiler_resolve( '\DA:' && lv_name ).
      IF lo_generic IS INITIAL.
        CONTINUE.
      ENDIF.

      REPLACE FIRST OCCURRENCE OF '[:type:]'
        IN lv_regex
        WITH determine_type_prefix( lo_generic ).

      compare( iv_name     = lv_name
               iv_regex    = lv_regex
               iv_relative = 2 ).
    ENDLOOP.

  ENDMETHOD.


  METHOD check_form.

    DATA: lt_tokens  TYPE stokesx_tab,
          lo_generic TYPE REF TO cl_abap_comp_data_generic,
          lv_scope   TYPE string,
          lv_type    TYPE string,
          lv_regex   TYPE string,
          lv_name    TYPE string,
          ls_token   LIKE LINE OF lt_tokens.


    CLEAR mv_at.

    lv_name = get_token_rel( 2 ).

    mo_stack->push( '\FO:' && lv_name ).

    lt_tokens = qualify_tokens( ).
    LOOP AT lt_tokens INTO ls_token.
      CASE ls_token-type.
        WHEN sana_tok_field_def.
          lv_name = remove_value( ls_token-str ).
          lo_generic = compiler_resolve( '\DA:' && lv_name ).
          IF NOT lo_generic IS BOUND.
            CONTINUE.
          ENDIF.

          lv_type = determine_type_prefix( lo_generic ).
          lv_regex = lv_scope.
          REPLACE FIRST OCCURRENCE OF '[:type:]' IN lv_regex WITH lv_type.

          compare( iv_name     = lv_name
                   iv_regex    = lv_regex
                   iv_relative = 2 ).

        WHEN sana_tok_word.
          CASE ls_token-str.
            WHEN 'USING'.
              lv_scope = ms_naming-proc_fousin.
            WHEN 'CHANGING'.
              lv_scope = ms_naming-proc_fochan.
            WHEN 'TABLES'.
              lv_scope = ms_naming-proc_fotabl.
          ENDCASE.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_function.

    DATA: lv_name      TYPE eu_lname,
          ls_interface TYPE rsfbintfv,
          ls_parameter TYPE rsfbpara.


    lv_name = get_token_rel( 2 ).
    mo_stack->set( '\FU:' && lv_name ).


    cl_fb_function_utility=>meth_get_interface(
      EXPORTING
        im_name             = lv_name
      IMPORTING
        ex_interface        = ls_interface
      EXCEPTIONS
        error_occured       = 1
        object_not_existing = 2
        OTHERS              = 3 ).
    IF sy-subrc <> 0.
      inform( p_kind = mv_errty
              p_test = myname
              p_code = '006' ).
      RETURN.
    ENDIF.

    IF skip_fm_parameters( ls_interface ) = abap_true.
      RETURN.
    ENDIF.

    check_fm_parameters(
      it_parameters = ls_interface-import
      iv_prefix     = ms_naming-proc_fimpor ).

    check_fm_parameters(
      it_parameters = ls_interface-export
      iv_prefix     = ms_naming-proc_fexpor ).

    check_fm_parameters(
      it_parameters = ls_interface-change
      iv_prefix     = ms_naming-proc_fchang ).

    check_fm_parameters(
      it_parameters = ls_interface-tables
      iv_prefix     = ms_naming-proc_ftable ).

  ENDMETHOD.


  METHOD check_function_pool.

    DATA: lv_name  TYPE string,
          lv_regex TYPE string.


    lv_name = get_token_rel( 2 ).

    lv_regex = ms_naming-globals_fugr.
    REPLACE FIRST OCCURRENCE OF '[:nspace:]' IN lv_regex WITH ms_naming-globals_nspace.

    compare( iv_name     = lv_name
             iv_regex    = lv_regex
             iv_relative = 2 ).

    mo_stack->push( '\PR:' && 'SAPL' && lv_name ).

  ENDMETHOD.


  METHOD check_inline_defs.

* todo

  ENDMETHOD.


  METHOD check_interface.

    DATA: lv_name  TYPE string,
          lv_regex TYPE string.


    CASE get_token_rel( 3 ).
      WHEN 'LOAD' OR 'DEFERRED'.
        RETURN.
    ENDCASE.

    lv_name = get_token_rel( 2 ).

    IF object_type = 'INTF' AND lv_name = object_name.
      mo_stack->set( '\TY:' && lv_name ).
    ELSEIF object_type = 'INTF'.
      mo_stack->set( '\PR:' && program_name && '\TY:' && lv_name ).
    ELSE.
      mo_stack->push( '\TY:' && lv_name ).
    ENDIF.


    lv_regex = ms_naming-globals_intf.
    REPLACE FIRST OCCURRENCE OF '[:nspace:]' IN lv_regex WITH ms_naming-globals_nspace.

    compare( iv_name     = lv_name
             iv_regex    = lv_regex
             iv_relative = 2 ).

  ENDMETHOD.


  METHOD check_method_definition.

    DATA: lt_tokens  TYPE stokesx_tab,
          lo_generic TYPE REF TO cl_abap_comp_data_generic,
          lo_super   TYPE REF TO cl_abap_comp_class,
          lv_scope   TYPE string,
          lv_type    TYPE string,
          lv_regex   TYPE string,
          lv_name    TYPE string,
          ls_token   LIKE LINE OF lt_tokens.


    lo_super = compiler_resolve_class( ).
    IF lo_super IS INITIAL.
      RETURN.
    ENDIF.
    WHILE NOT lo_super->super_class IS INITIAL.
      lo_super = lo_super->super_class.
    ENDWHILE.

    IF object_type = 'CLAS'
        AND ms_naming-set_excpar = abap_true
        AND get_token_rel( 2 ) = 'CONSTRUCTOR'
        AND lo_super->full_name = '\TY:CX_ROOT'.
      RETURN.
    ENDIF.

**********

    mo_stack->push( '\ME:' && get_token_rel( 2 ) ).

    lt_tokens = qualify_tokens( ).

    LOOP AT lt_tokens INTO ls_token.
      CASE ls_token-type.
        WHEN sana_tok_field_def.
          lv_name = remove_value( ls_token-str ).

          lo_generic = compiler_resolve( '\DA:' && lv_name ).
          IF NOT lo_generic IS BOUND.
            CONTINUE.
          ENDIF.

          lv_type = determine_type_prefix( lo_generic ).

          lv_regex = lv_scope.
          REPLACE FIRST OCCURRENCE OF '[:type:]' IN lv_regex WITH lv_type.

          compare( iv_name     = lv_name
                   iv_regex    = lv_regex
                   iv_relative = 2 ).

        WHEN sana_tok_word.
          CASE ls_token-str.
            WHEN 'IMPORTING'.
              lv_scope = ms_naming-oo_ooimpo.
            WHEN 'EXPORTING'.
              lv_scope = ms_naming-oo_ooexpo.
            WHEN 'CHANGING'.
              lv_scope = ms_naming-oo_oochan.
            WHEN 'RETURNING'.
              lv_scope = ms_naming-oo_ooretu.
          ENDCASE.
      ENDCASE.

    ENDLOOP.

    mo_stack->pop( ).

  ENDMETHOD.


  METHOD check_method_implementation.

    DATA: lv_method    TYPE string,
          lv_interface TYPE string.


    lv_method = get_token_rel( 2 ).

    IF lv_method CS '~'.
      SPLIT lv_method AT '~' INTO lv_interface lv_method.
      mo_stack->push( '\IN:' && lv_interface && '\ME:' && lv_method ).
    ELSE.
      mo_stack->push( '\ME:' && lv_method ).
    ENDIF.

  ENDMETHOD.


  METHOD check_parameter.

    DATA: lv_name  TYPE string,
          lv_regex TYPE string.


    lv_name = get_token_rel( 2 ).
    lv_regex = ms_naming-proc_pgpara.

    compare( iv_name     = lv_name
             iv_regex    = lv_regex
             iv_relative = 2 ).

  ENDMETHOD.


  METHOD check_report.

    DATA: lv_name  TYPE string,
          lv_regex TYPE string.


    mo_stack->push( '\PR:' && program_name ).

    lv_name = get_token_rel( 2 ).

    lv_regex = ms_naming-globals_prog.
    REPLACE FIRST OCCURRENCE OF '[:nspace:]' IN lv_regex WITH ms_naming-globals_nspace.

    compare( iv_name     = lv_name
             iv_regex    = lv_regex
             iv_relative = 2 ).

  ENDMETHOD.


  METHOD check_select_option.

    DATA: lv_name  TYPE string,
          lv_regex TYPE string.


    lv_name = get_token_rel( 2 ).
    lv_regex = ms_naming-proc_pgselo.

    compare( iv_name     = lv_name
             iv_regex    = lv_regex
             iv_relative = 2 ).

  ENDMETHOD.


  METHOD check_type.

* todo

  ENDMETHOD.


  METHOD compare.

    DATA: lv_regex   TYPE string,
          lv_include TYPE sobj_name.


    lv_regex = |^{ iv_regex }|.

    FIND REGEX lv_regex IN iv_name IGNORING CASE.
    IF sy-subrc <> 0.
      lv_include = get_include( p_level = statement_wa-level ).
      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = lv_include
              p_line         = get_line_rel( iv_relative )
              p_column       = get_column_rel( iv_relative )
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '001'
              p_param_1      = iv_regex
              p_param_2      = iv_name ).
    ENDIF.

  ENDMETHOD.


  METHOD compiler_resolve.

    DATA: lv_full TYPE string.


    lv_full = mo_stack->concatenate( iv_name ).

    ro_generic ?= mo_compiler->get_symbol_entry( lv_full ).

    IF ro_generic IS INITIAL AND NOT mv_at IS INITIAL.
      lv_full = mo_stack->concatenate( mv_at && iv_name ).
      ro_generic ?= mo_compiler->get_symbol_entry( lv_full ).
    ENDIF.

    IF ro_generic IS INITIAL.
      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = get_include( p_level = statement_wa-level )
              p_line         = get_line_rel( 2 )
              p_column       = get_column_rel( 2 )
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '002'
              p_param_1      = lv_full ).
    ENDIF.

  ENDMETHOD.


  METHOD compiler_resolve_class.

    DATA: lv_full TYPE string.


    lv_full = mo_stack->concatenate( ).

    ro_class ?= mo_compiler->get_symbol_entry( lv_full ).
    IF ro_class IS INITIAL.
      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = get_include( p_level = statement_wa-level )
              p_line         = get_line_rel( 2 )
              p_column       = get_column_rel( 2 )
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '004'
              p_param_1      = lv_full ).
    ENDIF.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    description = 'Naming Conventions'.                     "#EC NOTEXT
    category    = 'ZCL_AOC_CATEGORY'.
    version     = '002'.
    position    = '069'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

    set_defaults( ).

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD determine_scope_prefix.

    IF keyword( ) = 'STATICS'.
      rv_prefix = ms_naming-locals_static.
    ELSEIF mo_stack->concatenate( ) CS '\ME:'
        OR mo_stack->concatenate( ) CS '\FO:'
        OR mo_stack->concatenate( ) CS '\FU:'.
      rv_prefix = ms_naming-locals_data.
    ELSEIF mo_stack->concatenate( ) CS '\TY:'
        AND keyword( ) = 'DATA'.
      rv_prefix = ms_naming-oo_oodata.
    ELSEIF mo_stack->concatenate( ) CS '\TY:'
        AND keyword( ) = 'CLASS-DATA'.
      rv_prefix = ms_naming-oo_oocdat.
    ELSEIF mo_stack->concatenate( ) CS '\TY:'
        AND keyword( ) = 'CONSTANTS'.
      rv_prefix = ms_naming-oo_oocons.
    ELSEIF mo_stack->concatenate( ) CS '\TY:'
        AND keyword( ) = 'TYPES'.
      rv_prefix = ms_naming-oo_ootype.
    ELSE.
      rv_prefix = ms_naming-proc_pgdata.
    ENDIF.

  ENDMETHOD.


  METHOD determine_type_prefix.

    DATA: lo_table_symbol      TYPE REF TO cl_abap_comp_table_type,
          lo_symbol_simple     TYPE REF TO cl_abap_comp_data,
          lo_type_symbol       TYPE REF TO cl_abap_comp_type,
          lo_type_symbol_class TYPE REF TO cl_abap_comp_class,
          lo_symbol_tab        TYPE REF TO cl_abap_comp_table_with_head,
          lo_type_symbol_ref   TYPE REF TO cl_abap_comp_ref_type,
          lo_type_alias_symbol TYPE REF TO cl_abap_comp_alias_type.


    IF io_generic->node_kind = cl_abap_comp_data_generic=>data_node_kind_table_with_head.
      lo_symbol_tab ?= io_generic.
      lo_symbol_simple = lo_symbol_tab->table.
    ELSE.
      lo_symbol_simple ?= io_generic.
    ENDIF.

    lo_type_symbol = lo_symbol_simple->type.

    WHILE lo_type_symbol->type_kind = cl_abap_comp_type=>type_kind_alias.
      lo_type_alias_symbol ?= lo_type_symbol.
      lo_type_symbol        = lo_type_alias_symbol->alias_type.
    ENDWHILE.

    CASE lo_type_symbol->type_kind.
      WHEN cl_abap_comp_type=>type_kind_elementary.
        CASE lo_type_symbol->full_name.
          WHEN '\PT:ANY' OR '\PT:DATA'.
            rv_prefix = ms_naming-prefix_generi.
          WHEN OTHERS.
            rv_prefix = ms_naming-prefix_elemen.
        ENDCASE.
      WHEN cl_abap_comp_type=>type_kind_structure OR cl_abap_comp_type=>type_kind_ddic_dbtab.
        rv_prefix = ms_naming-prefix_struct.
      WHEN cl_abap_comp_type=>type_kind_table.
        lo_table_symbol ?= lo_type_symbol.
        CASE lo_table_symbol->index_kind.
          WHEN cl_abap_comp_table_type=>index_kind_hashed.
            rv_prefix = ms_naming-prefix_thash.
          WHEN cl_abap_comp_table_type=>index_kind_sorted.
            rv_prefix = ms_naming-prefix_tsort.
          WHEN cl_abap_comp_table_type=>index_kind_index.
            rv_prefix = ms_naming-prefix_tindex.
          WHEN cl_abap_comp_table_type=>index_kind_standard.
            rv_prefix = ms_naming-prefix_tstand.
          WHEN cl_abap_comp_table_type=>index_kind_any.
            rv_prefix = ms_naming-prefix_tany.
          WHEN OTHERS.
            ASSERT 0 = 1.
        ENDCASE.
      WHEN cl_abap_comp_type=>type_kind_reference.
        lo_type_symbol_ref ?= lo_type_symbol.
        CASE lo_type_symbol_ref->ref_type->type_kind.
          WHEN cl_abap_comp_table_type=>type_kind_interface.
            rv_prefix = ms_naming-prefix_rinter.
          WHEN cl_abap_comp_table_type=>type_kind_class.

            lo_type_symbol_class ?= lo_type_symbol_ref->ref_type.
            WHILE lo_type_symbol_class->super_class IS BOUND.
              lo_type_symbol_class = lo_type_symbol_class->super_class.
            ENDWHILE.

            CASE lo_type_symbol_class->full_name.
              WHEN '\TY:CX_ROOT'.
                rv_prefix = ms_naming-prefix_rexcep.
              WHEN '\TY:CL_BADI_BASE'.
                rv_prefix = ms_naming-prefix_rbadi.
              WHEN OTHERS.
                rv_prefix = ms_naming-prefix_rclass.
            ENDCASE.

          WHEN OTHERS.
            rv_prefix = ms_naming-prefix_rdata.
        ENDCASE.
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      ms_naming = ms_naming
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Bad naming, expected &1, got &2'.         "#EC NOTEXT
      WHEN '002'.
        p_text = 'Unable to resolve &1'.                    "#EC NOTEXT
      WHEN '003'.
        p_text = 'Error qualifying tokens'.                 "#EC NOTEXT
      WHEN '004'.
        p_text = 'Unable to resolve &1'.                    "#EC NOTEXT
      WHEN '005'.
        p_text = 'Syntax error'.                            "#EC NOTEXT
      WHEN '006'.
        p_text = 'Error reading FM parameters'.             "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.


  METHOD get_statement.

    DATA: ls_token LIKE LINE OF ref_scan->tokens.


    LOOP AT ref_scan->tokens INTO ls_token FROM statement_wa-from TO statement_wa-to.
      rv_string = |{ rv_string }{ ls_token-str } |.
    ENDLOOP.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    ms_naming-set_errty = mv_errty.

    CALL FUNCTION 'Z_AOC_NAMING'
      EXPORTING
        iv_read_only = p_display
      CHANGING
        cs_data      = ms_naming.

    mv_errty = ms_naming-set_errty.
    attributes_ok = abap_true.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      ms_naming = ms_naming
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD qualify_tokens.

    INSERT LINES OF ref_scan->tokens FROM statement_wa-from
      TO statement_wa-to INTO TABLE rt_tokens.

    CALL FUNCTION 'RS_QUALIFY_ABAP_TOKENS_STR'
      EXPORTING
        statement_type        = statement_wa-type
        index_from            = 1
        index_to              = lines( rt_tokens )
      CHANGING
        stokesx_tab           = rt_tokens
      EXCEPTIONS
        error_load_pattern    = 1
        unknown_keyword       = 2
        no_matching_statement = 3
        meaningless_statement = 4
        OTHERS                = 5.
    IF sy-subrc <> 0.
      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = get_include( p_level = statement_wa-level )
              p_line         = get_line_rel( 2 )
              p_column       = get_column_rel( 2 )
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '003' ).
    ENDIF.

  ENDMETHOD.


  METHOD remove_value.

    rv_output = iv_input.

    IF rv_output CP 'VALUE(*)'.
      rv_output = rv_output+6.
      TRANSLATE rv_output USING ') '.
      rv_output = condense( rv_output ).
    ENDIF.

  ENDMETHOD.


  METHOD set_defaults.

    ms_naming-prefix_elemen = 'V'.
    ms_naming-prefix_generi = 'G'.
    ms_naming-prefix_struct = 'S'.
    ms_naming-prefix_tany   = 'T'.
    ms_naming-prefix_thash  = 'T'.
    ms_naming-prefix_tindex = 'T'.
    ms_naming-prefix_tstand = 'T'.
    ms_naming-prefix_tsort  = 'T'.
    ms_naming-prefix_rdata  = 'R'.
    ms_naming-prefix_rclass = 'O'.
    ms_naming-prefix_rbadi  = 'B'.
    ms_naming-prefix_rexcep = 'X'.
    ms_naming-prefix_rinter = 'I'.

    ms_naming-globals_nspace = 'Z'.
    ms_naming-globals_fugr   = '[:nspace:]'.
    ms_naming-globals_prog   = '[:nspace:]'.
    ms_naming-globals_clas   = '[:nspace:]CL_'.
    ms_naming-globals_clasx  = '[:nspace:]CX_'.
    ms_naming-globals_clast  = '[:nspace:]CL_'.
    ms_naming-globals_clasa  = '[:nspace:]CL_'.
    ms_naming-globals_intf   = '[:nspace:]IF_'.

    ms_naming-locals_data   = 'L[:type:]_' ##NO_TEXT.
    ms_naming-locals_static = 'S[:type:]_' ##NO_TEXT.
    ms_naming-locals_fsymbo = '<L[:type:]_'.
    ms_naming-locals_lconst = 'LC_'.
    ms_naming-locals_ltypes = ''.

    ms_naming-proc_fimpor = 'I[:type:]_' ##NO_TEXT.
    ms_naming-proc_fexpor = 'E[:type:]_' ##NO_TEXT.
    ms_naming-proc_fchang = 'C[:type:]_' ##NO_TEXT.
    ms_naming-proc_ftable = 'T[:type:]_' ##NO_TEXT.
    ms_naming-proc_fousin = 'P[:type:]_' ##NO_TEXT.
    ms_naming-proc_fochan = 'C[:type:]_' ##NO_TEXT.
    ms_naming-proc_fotabl = 'T[:type:]_' ##NO_TEXT.
    ms_naming-proc_pgdata = 'G[:type:]_' ##NO_TEXT.
    ms_naming-proc_pgfisy = '<G[:type:]_'.
    ms_naming-proc_pgcons = 'GC_'.
    ms_naming-proc_pgtype = ''.
    ms_naming-proc_pgselo = 'S_'.
    ms_naming-proc_pgpara = 'P_'.

    ms_naming-oo_oodata = 'M[:type:]_' ##NO_TEXT.
    ms_naming-oo_oocdat = 'G[:type:]_' ##NO_TEXT.
    ms_naming-oo_oocons = 'C_'.
    ms_naming-oo_ootype = ''.
    ms_naming-oo_ooimpo = 'I[:type:]_' ##NO_TEXT.
    ms_naming-oo_ooexpo = 'E[:type:]_' ##NO_TEXT.
    ms_naming-oo_oochan = 'C[:type:]_' ##NO_TEXT.
    ms_naming-oo_ooretu = 'R[:type:]_' ##NO_TEXT.
    ms_naming-oo_oolcla = 'LCL_'.
    ms_naming-oo_ooltcl = 'LTCL_'.
    ms_naming-oo_oolint = 'LIF_'.

    ms_naming-set_excpar = abap_true.
    ms_naming-set_cfunc  = abap_true.
    ms_naming-set_idocfm = abap_true.
    ms_naming-set_bwext  = abap_true.

  ENDMETHOD.


  METHOD skip_fm_parameters.

    DATA: ls_check     TYPE rsfbintfv,
          ls_parameter TYPE rsfbpara.


    DEFINE _append.
      ls_parameter-parameter = &2.
      APPEND ls_parameter TO ls_check-&1.
    END-OF-DEFINITION.

* idoc processing function module
    CLEAR ls_check.
    _append import 'INPUT_METHOD'.
    _append import 'MASS_PROCESSING'.
    _append export 'WORKFLOW_RESULT'.
    _append export 'APPLICATION_VARIABLE'.
    _append export 'IN_UPDATE_TASK'.
    _append export 'CALL_TRANSACTION_DONE'.
    _append tables 'IDOC_CONTRL'.
    _append tables 'IDOC_DATA'.
    _append tables 'IDOC_STATUS'.
    _append tables 'RETURN_VARIABLES'.
    _append tables 'SERIALIZATION_INFO'.

    rv_skip = skip_fm_parameters_check( is_parameters = is_parameters
                                        is_check      = ls_check ).
    IF rv_skip = abap_true.
      RETURN.
    ENDIF.

* add more here

  ENDMETHOD.


  METHOD skip_fm_parameters_check.

    DATA: ls_parameter LIKE LINE OF is_check-import.


    IF lines( is_parameters-import ) <> lines( is_check-import )
        OR lines( is_parameters-export ) <> lines( is_check-export )
        OR lines( is_parameters-change ) <> lines( is_check-change )
        OR lines( is_parameters-tables ) <> lines( is_check-tables ).
      RETURN.
    ENDIF.

    LOOP AT is_check-import INTO ls_parameter.
      READ TABLE is_parameters-import WITH KEY parameter = ls_parameter-parameter
        TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDLOOP.
    LOOP AT is_check-export INTO ls_parameter.
      READ TABLE is_parameters-export WITH KEY parameter = ls_parameter-parameter
        TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDLOOP.
    LOOP AT is_check-change INTO ls_parameter.
      READ TABLE is_parameters-change WITH KEY parameter = ls_parameter-parameter
        TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDLOOP.
    LOOP AT is_check-tables INTO ls_parameter.
      READ TABLE is_parameters-tables WITH KEY parameter = ls_parameter-parameter
        TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDLOOP.

    rv_skip = abap_true.

  ENDMETHOD.
ENDCLASS.
