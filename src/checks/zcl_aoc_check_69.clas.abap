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

  methods QUALIFY_TOKENS
    returning
      value(RT_TOKENS) type STOKESX_TAB .
  methods GET_STATEMENT
    returning
      value(RV_STRING) type STRING .
  methods DETERMINE_TYPE_PREFIX
    importing
      !IO_GENERIC type ref to CL_ABAP_COMP_DATA_GENERIC
    returning
      value(RV_PREFIX) type STRING .
  methods DETERMINE_SCOPE_PREFIX
    returning
      value(RV_PREFIX) type STRING .
  methods COMPARE
    importing
      !IV_NAME type STRING
      !IV_REGEX type STRING
      !IV_RELATIVE type I .
  methods COMPILER_RESOLVE
    importing
      !IV_NAME type STRING
    returning
      value(RO_GENERIC) type ref to CL_ABAP_COMP_DATA_GENERIC .
  methods COMPILER_RESOLVE_CLASS
    returning
      value(RO_CLASS) type ref to CL_ABAP_COMP_CLASS .
  methods CHECK_CLASS .
  methods CHECK_METHOD_DEFINITION .
  methods CHECK_METHOD_IMPLEMENTATION .
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
ENDCLASS.



CLASS ZCL_AOC_CHECK_69 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    CREATE OBJECT mo_stack.

    mo_compiler = cl_abap_compiler=>create( program_name ).

    LOOP AT it_statements INTO statement_wa.

      CASE keyword( ).
        WHEN 'REPORT'.
          check_report( ).
        WHEN 'FUNCTION-POOL'.
          check_function_pool( ).
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
        WHEN 'ENDCLASS' OR 'ENDMETHOD' OR 'ENDFORM'.
          mo_stack->pop( ).
        WHEN 'ENDFUNCTION'.
          mo_stack->set( '\PR:' && 'SAPL' && object_name ).
        WHEN OTHERS.
          check_inline_defs( ).
      ENDCASE.

    ENDLOOP.

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

    IF lv_name = 'BEGIN' AND get_token_rel( 3 ) = 'OF'.
      lv_name = get_token_rel( 4 ).
      mv_begin = abap_true.
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

* todo

  ENDMETHOD.


  METHOD check_form.

    DATA: lt_tokens  TYPE stokesx_tab,
          lo_generic TYPE REF TO cl_abap_comp_data_generic,
          lv_scope   TYPE string,
          lv_type    TYPE string,
          lv_regex   TYPE string,
          lv_name    TYPE string,
          ls_token   LIKE LINE OF lt_tokens.


    lv_name = get_token_rel( 2 ).

    mo_stack->push( '\FO:' && lv_name ).

    lt_tokens = qualify_tokens( ).
    LOOP AT lt_tokens INTO ls_token.
      CASE ls_token-type.
        WHEN sana_tok_field_def.
          lv_name = ls_token-str.
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

    DATA: lv_name TYPE string.


    lv_name = get_token_rel( 2 ).
    mo_stack->set( '\FU:' && lv_name ).

* todo, check function module parameter names

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
          lv_name = ls_token-str.

          IF lv_name CP 'VALUE(*)'.
            lv_name = lv_name+6.
            TRANSLATE lv_name USING ') '.
            lv_name = condense( lv_name ).
          ENDIF.

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
              p_code         = '002'
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
        rv_prefix = ms_naming-prefix_elemen.
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

          WHEN cl_abap_comp_table_type=>type_kind_elementary.
            rv_prefix = ms_naming-prefix_rdata.
          WHEN OTHERS.
            ASSERT 0 = 1.
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
    ms_naming-locals_lconst = 'LC[:type:]_'.
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
ENDCLASS.
