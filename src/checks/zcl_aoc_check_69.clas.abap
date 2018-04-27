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
  PROTECTED SECTION.

    METHODS check_class .
    METHODS check_constant .
    METHODS check_data .
    METHODS check_field_symbol .
    METHODS check_form .
    METHODS check_function .
    METHODS check_function_pool .
    METHODS check_inline_defs .
    METHODS check_interface .
    METHODS check_parameter .
    METHODS check_report .
    METHODS check_select_option .
    METHODS check_type .
    METHODS set_defaults .
  PRIVATE SECTION.

    DATA ms_naming TYPE zaoc_naming .
    DATA mo_compiler TYPE REF TO cl_abap_compiler .
ENDCLASS.



CLASS ZCL_AOC_CHECK_69 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: ls_statement LIKE LINE OF it_statements,
          lv_keyword   TYPE string.


    mo_compiler = cl_abap_compiler=>create( program_name ).

    LOOP AT it_statements INTO statement_wa.

      CASE keyword( ).
        WHEN 'REPORT'.
          check_report( ).
        WHEN 'FUNCTION-POOL'.
          check_function_pool( ).
        WHEN 'TYPES'.
          check_type( ).
        WHEN 'DATA' OR 'RANGES'.
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
        WHEN OTHERS.
          check_inline_defs( ).
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD check_class.

* todo

  ENDMETHOD.


  METHOD check_constant.

* todo

  ENDMETHOD.


  METHOD check_data.

    DATA: lo_table_symbol   TYPE REF TO cl_abap_comp_table_type,
          lo_symbol_generic TYPE REF TO cl_abap_comp_data_generic,
          lo_symbol_simple  TYPE REF TO cl_abap_comp_data,
          lo_type_symbol    TYPE REF TO cl_abap_comp_type,
          lo_symbol_tab     TYPE REF TO cl_abap_comp_table_with_head,
          lv_regex          TYPE string,
          lv_name           TYPE string,
          lv_full           TYPE string.


    lv_name = get_token_rel( 2 ).

* todo
    lv_full = '\PR:' && program_name && '\DA:' && lv_name.

    lo_symbol_generic ?= mo_compiler->get_symbol_entry( lv_full ).

    IF NOT lo_symbol_generic IS INITIAL.
* todo
      IF lo_symbol_generic->node_kind = cl_abap_comp_data_generic=>data_node_kind_table_with_head.
        lo_symbol_tab ?= lo_symbol_generic.
        lo_symbol_simple = lo_symbol_tab->table.
      ELSE.
        lo_symbol_simple ?= lo_symbol_generic.
      ENDIF.

      lo_type_symbol = lo_symbol_simple->type.

      CASE lo_type_symbol->type_kind.
        WHEN cl_abap_comp_type=>type_kind_elementary.
          lv_regex = |^{ ms_naming-proc_pgdata }|.
          REPLACE FIRST OCCURRENCE OF '[:type:]' IN lv_regex WITH ms_naming-prefix_elemen.

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

        WHEN cl_abap_comp_type=>type_kind_structure OR cl_abap_comp_type=>type_kind_ddic_dbtab.
* todo
        WHEN cl_abap_comp_type=>type_kind_table.
* todo
        WHEN cl_abap_comp_type=>type_kind_reference.
* todo
        WHEN OTHERS.
* todo, ?
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD check_field_symbol.

* todo

  ENDMETHOD.


  METHOD check_form.

* todo

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
              p_param_1      = lv_regex ).
    ENDIF.

  ENDMETHOD.


  METHOD check_inline_defs.

* todo

  ENDMETHOD.


  METHOD check_interface.

* todo

  ENDMETHOD.


  METHOD check_parameter.

* todo

  ENDMETHOD.


  METHOD check_report.

    DATA: lv_name  TYPE string,
          lv_regex TYPE string.


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
