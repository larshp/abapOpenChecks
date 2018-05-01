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
  methods DETERMINE_TYPE_PREFIX
    importing
      !IO_GENERIC type ref to CL_ABAP_COMP_DATA_GENERIC
    returning
      value(RV_PREFIX) type STRING .
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

    DATA: ls_statement LIKE LINE OF it_statements,
          lv_keyword   TYPE string.


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
        WHEN OTHERS.
          check_inline_defs( ).
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD check_class.

    DATA: lv_name  TYPE string,
          lv_index TYPE i,
          lv_regex TYPE string.


    CASE get_token_rel( 4 ).
      WHEN 'LOAD' OR 'DEFERRED'.
        RETURN.
    ENDCASE.

* todo: loop through statement tokens
*        WHEN 'PUBLIC'.
    lv_regex = |^{ ms_naming-globals_clas }|.
*          EXIT.
** FOR TESTING
*        WHEN ''.
*          lv_regex = |^{ ms_naming-oo_oolcla }|.
*          EXIT.
*      ENDCASE.

    lv_name = get_token_rel( 2 ).

    IF object_type = 'CLAS' AND lv_name = object_name.
      mo_stack->set( '\TY:' && lv_name ).
    ELSEIF object_type = 'CLAS'.
      mo_stack->set( '\PR:' && program_name && '\TY:' && lv_name ).
    ELSE.
      mo_stack->push( '\TY:' && lv_name ).
    ENDIF.

    REPLACE FIRST OCCURRENCE OF '[:nspace:]' IN lv_regex WITH ms_naming-globals_nspace.

    FIND REGEX lv_regex IN lv_name IGNORING CASE.
    IF sy-subrc <> 0.
      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = get_include(  )
              p_line         = get_line_rel( 2 )
              p_column       = get_column_rel( 2 )
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '001'
              p_param_1      = lv_regex
              p_param_2      = lv_name ).
    ENDIF.

  ENDMETHOD.


  METHOD check_constant.

* todo

  ENDMETHOD.


  METHOD check_data.

    DATA:
      lo_symbol_generic TYPE REF TO cl_abap_comp_data_generic,
      lv_regex          TYPE string,
      lv_name           TYPE string,
      lv_full           TYPE string.


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

    lv_full = mo_stack->concatenate( '\DA:' && lv_name ).

********************

    lo_symbol_generic ?= mo_compiler->get_symbol_entry( lv_full ).

    IF lo_symbol_generic IS INITIAL.
      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = get_include( p_level = statement_wa-level )
              p_line         = get_line_rel( 2 )
              p_column       = get_column_rel( 2 )
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '002'
              p_param_1      = lv_full ).
      RETURN.
    ENDIF.

    lv_regex = determine_scope_prefix( ).

    REPLACE FIRST OCCURRENCE OF '[:type:]'
      IN lv_regex
      WITH determine_type_prefix( lo_symbol_generic ).

    FIND REGEX lv_regex IN lv_name IGNORING CASE.
    IF sy-subrc <> 0.
      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = get_include( p_level = statement_wa-level )
              p_line         = get_line_rel( 2 )
              p_column       = get_column_rel( 2 )
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '001'
              p_param_1      = lv_regex
              p_param_2      = lv_name ).
    ENDIF.

  ENDMETHOD.


  METHOD check_field_symbol.

* todo

  ENDMETHOD.


  METHOD check_form.

    DATA: lv_name TYPE string.

    lv_name = get_token_rel( 2 ).

    mo_stack->push( '\FO:' && lv_name ).

* todo, check parameters

  ENDMETHOD.


  METHOD check_function.

* todo

  ENDMETHOD.


  METHOD check_function_pool.

    DATA: lv_name  TYPE string,
          lv_regex TYPE string.


    lv_name = get_token_rel( 2 ).

    lv_regex = |^{ ms_naming-globals_fugr }|.
    REPLACE FIRST OCCURRENCE OF '[:nspace:]' IN lv_regex WITH ms_naming-globals_nspace.

    FIND REGEX lv_regex IN lv_name IGNORING CASE.
    IF sy-subrc <> 0.
      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = get_include( )
              p_line         = get_line_rel( 2 )
              p_column       = get_column_rel( 2 )
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '001'
              p_param_1      = lv_regex
              p_param_2      = lv_name ).
    ENDIF.

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


    lv_regex = |^{ ms_naming-globals_intf }|.
    REPLACE FIRST OCCURRENCE OF '[:nspace:]' IN lv_regex WITH ms_naming-globals_nspace.

    FIND REGEX lv_regex IN lv_name IGNORING CASE.
    IF sy-subrc <> 0.
      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = get_include( )
              p_line         = get_line_rel( 2 )
              p_column       = get_column_rel( 2 )
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '001'
              p_param_1      = lv_regex
              p_param_2      = lv_name ).
    ENDIF.

  ENDMETHOD.


  METHOD check_method_definition.

* todo, check parameters

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

* todo

  ENDMETHOD.


  METHOD check_report.

    DATA: lv_name  TYPE string,
          lv_regex TYPE string.


    mo_stack->push( '\PR:' && program_name ).

    lv_name = get_token_rel( 2 ).

    lv_regex = |^{ ms_naming-globals_prog }|.
    REPLACE FIRST OCCURRENCE OF '[:nspace:]' IN lv_regex WITH ms_naming-globals_nspace.

    FIND REGEX lv_regex IN lv_name IGNORING CASE.
    IF sy-subrc <> 0.
      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = get_include( )
              p_line         = get_line_rel( 2 )
              p_column       = get_column_rel( 2 )
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '001'
              p_param_1      = lv_regex
              p_param_2      = lv_name ).
    ENDIF.

  ENDMETHOD.


  METHOD check_select_option.

* todo

  ENDMETHOD.


  METHOD check_type.

* todo

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
      rv_prefix = |^{ ms_naming-locals_static }|.
    ELSEIF mo_stack->concatenate( ) CS '\ME:' OR mo_stack->concatenate( ) CS '\FO:'.
      rv_prefix = |^{ ms_naming-locals_data }|.
    ELSEIF mo_stack->concatenate( ) CS '\TY:' AND keyword( ) = 'DATA'.
      rv_prefix = |^{ ms_naming-oo_oodata }|.
    ELSEIF mo_stack->concatenate( ) CS '\TY:' AND keyword( ) = 'CLASS-DATA'.
      rv_prefix = |^{ ms_naming-oo_oocdat }|.
    ELSEIF mo_stack->concatenate( ) CS '\TY:' AND keyword( ) = 'CONSTANTS'.
      rv_prefix = |^{ ms_naming-oo_oocons }|.
    ELSEIF mo_stack->concatenate( ) CS '\TY:' AND keyword( ) = 'TYPES'.
      rv_prefix = |^{ ms_naming-oo_ootype }|.
    ELSE.
      rv_prefix = |^{ ms_naming-proc_pgdata }|.
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
            BREAK-POINT.
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
            BREAK-POINT.
        ENDCASE.
      WHEN OTHERS.
        BREAK-POINT.
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
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

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

    ms_naming-locals_data   = 'L[:type:]_'.
    ms_naming-locals_static = 'S[:type:]_'.
    ms_naming-locals_fsymbo = '<L[:type:]_'.
    ms_naming-locals_lconst = 'LC[:type:]_'.
    ms_naming-locals_ltypes = ''.

    ms_naming-proc_fimpor = 'I[:type:]_'.
    ms_naming-proc_fexpor = 'E[:type:]_'.
    ms_naming-proc_fchang = 'C[:type:]_'.
    ms_naming-proc_ftable = 'T[:type:]_'.
    ms_naming-proc_fousin = 'P[:type:]_'.
    ms_naming-proc_fochan = 'C[:type:]_'.
    ms_naming-proc_fotabl = 'T[:type:]_'.
    ms_naming-proc_pgdata = 'G[:type:]_'.
    ms_naming-proc_pgfisy = '<G[:type:]_'.
    ms_naming-proc_pgcons = 'GC_'.
    ms_naming-proc_pgtype = ''.
    ms_naming-proc_pgselo = 'S_'.
    ms_naming-proc_pgpara = 'P_'.

    ms_naming-oo_oodata = 'M[:type:]_'.
    ms_naming-oo_oocdat = 'G[:type:]_'.
    ms_naming-oo_oocons = 'C_'.
    ms_naming-oo_ootype = ''.
    ms_naming-oo_ooimpo = 'I[:type:]_'.
    ms_naming-oo_ooexpo = 'E[:type:]_'.
    ms_naming-oo_oochan = 'C[:type:]_'.
    ms_naming-oo_ooretu = 'R[:type:]_'.
    ms_naming-oo_oolcla = 'LCL_'.
    ms_naming-oo_ooltcl = 'LTCL_'.
    ms_naming-oo_oolint = 'LIF_'.

    ms_naming-set_excpar = abap_true.
    ms_naming-set_cfunc  = abap_true.
    ms_naming-set_idocfm = abap_true.
    ms_naming-set_bwext  = abap_true.

  ENDMETHOD.
ENDCLASS.
