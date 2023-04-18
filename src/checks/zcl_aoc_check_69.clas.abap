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
    METHODS if_ci_test~query_attributes
        REDEFINITION .
    METHODS put_attributes
        REDEFINITION .
  PROTECTED SECTION.

    METHODS is_parallel_method
      IMPORTING
        !it_tokens     TYPE stokesx_tab
      RETURNING
        VALUE(rv_bool) TYPE abap_bool .
    METHODS field_symbol
      IMPORTING
        !iv_name TYPE string .
    METHODS data
      IMPORTING
        !iv_name  TYPE string
        !iv_scope TYPE string OPTIONAL .
    METHODS is_global_exception_class
      RETURNING
        VALUE(rv_bool) TYPE abap_bool .
    METHODS analyze_statements
      IMPORTING
        !it_statements TYPE sstmnt_tab .
    METHODS check_at .
    METHODS check_class .
    METHODS check_constant .
    METHODS check_data .
    METHODS check_field_symbol .
    METHODS check_fm_parameters
      IMPORTING
        !it_parameters TYPE rsfb_para
        !iv_prefix     TYPE string .
    METHODS check_form .
    METHODS check_function .
    METHODS check_function_pool .
    METHODS check_inline_defs .
    METHODS check_interface .
    METHODS check_method_definition .
    METHODS check_method_implementation .
    METHODS check_parameter .
    METHODS check_report .
    METHODS check_select_option .
    METHODS check_type .
    METHODS compare
      IMPORTING
        !iv_name     TYPE string
        !iv_regex    TYPE string
        !iv_relative TYPE i .
    METHODS compiler_resolve
      IMPORTING
        !iv_name          TYPE string
      RETURNING
        VALUE(ro_generic) TYPE REF TO cl_abap_comp_data_generic .
    METHODS compiler_resolve_class
      RETURNING
        VALUE(ro_class) TYPE REF TO cl_abap_comp_class .
    METHODS determine_scope_prefix
      RETURNING
        VALUE(rv_prefix) TYPE string .
    METHODS determine_type_prefix
      IMPORTING
        !io_generic      TYPE REF TO cl_abap_comp_data_generic
      RETURNING
        VALUE(rv_prefix) TYPE string .
    METHODS get_statement
      RETURNING
        VALUE(rv_string) TYPE string .
    METHODS qualify_tokens
      RETURNING
        VALUE(rt_tokens) TYPE stokesx_tab .
    METHODS remove_value
      IMPORTING
        !iv_input        TYPE string
      RETURNING
        VALUE(rv_output) TYPE string .
    METHODS set_defaults .
    METHODS skip_fm_parameters
      IMPORTING
        !iv_name       TYPE eu_lname
        !is_parameters TYPE rsfbintfv
      RETURNING
        VALUE(rv_skip) TYPE abap_bool .
    METHODS skip_fm_parameters_check
      IMPORTING
        !is_parameters TYPE rsfbintfv
        !is_check      TYPE rsfbintfv
      RETURNING
        VALUE(rv_skip) TYPE abap_bool .
    METHODS is_unresolved_exception_class
        IMPORTING
          !iv_class_fullname TYPE string
        RETURNING
          VALUE(rv_is_an_excpcls) TYPE xfeld .
  PRIVATE SECTION.

    DATA mo_scan TYPE REF TO zcl_aoc_scan .
    DATA ms_naming TYPE zaoc_naming .
    DATA mo_compiler TYPE REF TO cl_abap_compiler .
    DATA mo_stack TYPE REF TO lcl_stack .
    DATA mv_begin TYPE i .
    DATA mv_at TYPE string .
    DATA mv_position TYPE i .
ENDCLASS.



CLASS zcl_aoc_check_69 IMPLEMENTATION.


  METHOD analyze_statements.

    DATA: lv_define    TYPE abap_bool,
          lv_keyword   TYPE string,
          ls_object_ns TYPE zcl_aoc_util_reg_atc_namespace=>ty_ns_object.


    LOOP AT it_statements INTO statement_wa.
      mv_position = sy-tabix.
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
          IF object_type = 'FUGR' OR
              object_type = 'FUGS' OR
              object_type = 'FUGX'.
            IF zcl_aoc_util_reg_atc_namespace=>is_in_namespace( iv_pgmid    = 'R3TR'
                                                                iv_object   = 'FUGR'
                                                                iv_obj_name = object_name ) = abap_true.

              ls_object_ns = zcl_aoc_util_reg_atc_namespace=>split_ns_object( iv_pgmid    = 'R3TR'
                                                                              iv_object   = 'FUGR'
                                                                              iv_obj_name = object_name ).

              mo_stack->set( '\PR:'
                          && ls_object_ns-namespace
                          && 'SAPL'
                          && ls_object_ns-object ).
            ELSE.
              mo_stack->set( '\PR:SAPL' && object_name ).
            ENDIF.
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


    mo_scan = io_scan.

    IF object_type = 'WDYN'.
      RETURN. " todo, WDYN
    ELSEIF object_type = 'PROG'.
      SELECT SINGLE subc FROM reposrc INTO lv_subc
        WHERE progname = object_name AND r3state = 'A'.
      IF sy-subrc <> 0 OR lv_subc = 'I' OR lv_subc = 'S'.
        RETURN.
      ENDIF.
    ENDIF.

    cl_abap_compiler=>clear_cache( ).
    CREATE OBJECT mo_stack.
    mo_compiler = cl_abap_compiler=>create( program_name ).

    mo_compiler->get_check_infos( IMPORTING p_subrc = lv_subrc ).
    IF lv_subrc <> 0 AND ms_naming-set_syntax = abap_true.
      inform( p_kind = mv_errty
              p_test = myname
              p_code = '005' ).
      RETURN.
    ENDIF.

    analyze_statements( io_scan->statements ).

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
          lv_abstract  TYPE abap_bool,
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
    lv_abstract = lo_super->is_abstract.
    WHILE NOT lo_super->super_class IS INITIAL.
      lo_super = lo_super->super_class.
    ENDWHILE.

    IF object_name = lv_name AND object_type = 'CLAS'.
      IF lo_super->full_name = '\TY:CX_ROOT'.
        lv_regex = ms_naming-globals_clasx.
      ELSEIF lv_abstract = abap_true.
        lv_regex = ms_naming-globals_clasa.
      ELSE.
        lv_regex = ms_naming-globals_clas.
      ENDIF.
      REPLACE FIRST OCCURRENCE OF '[:nspace:]' IN lv_regex WITH ms_naming-globals_nspace.
    ELSE.
      lv_regex = ms_naming-oo_oolcla.
      lv_statement = get_statement( ).
      IF lv_statement CS 'FOR TESTING'.
        lv_regex = ms_naming-oo_ooltcl.
      ELSEIF lo_super->full_name = '\TY:CX_ROOT'.
        lv_regex = ms_naming-oo_oolxcl.
      ENDIF.
    ENDIF.

    compare( iv_name     = lv_name
             iv_regex    = lv_regex
             iv_relative = 2 ).

  ENDMETHOD.


  METHOD check_constant.

    DATA: lv_regex   TYPE string,
          lv_name    TYPE string,
          lv_offset  TYPE i,
          lo_generic TYPE REF TO cl_abap_comp_data_generic.


    lv_name = get_token_rel( 2 ).

* remove old style length definitions
    FIND '(' IN lv_name MATCH OFFSET lv_offset.
    IF sy-subrc = 0.
      lv_name = lv_name(lv_offset).
    ENDIF.

    IF lv_name = 'BEGIN' AND get_token_rel( 3 ) = 'OF' AND mv_begin = abap_false.
      lv_name = get_token_rel( 4 ).
      mv_begin = 1.
    ELSEIF lv_name = 'BEGIN' AND get_token_rel( 3 ) = 'OF'.
      mv_begin = mv_begin + 1.
      RETURN.
    ELSEIF lv_name = 'END' AND get_token_rel( 3 ) = 'OF'.
      mv_begin = mv_begin - 1.
      RETURN.
    ELSEIF mv_begin > 0.
      RETURN.
    ENDIF.

    IF mo_stack->concatenate( ) CS '\ME:'
        OR mo_stack->concatenate( ) CS '\FO:'
        OR mo_stack->concatenate( ) CS '\FU:'.
      lv_regex = ms_naming-locals_lconst.
    ELSEIF mo_stack->concatenate( ) CS '\TY:'.
      lv_regex = ms_naming-oo_oocons.
      IF ms_naming-set_exccon = abap_true AND is_global_exception_class( ) = abap_true.
        RETURN.
      ENDIF.
    ELSE.
      lv_regex = ms_naming-proc_pgcons.
    ENDIF.

    lo_generic = compiler_resolve( '\DA:' && lv_name ).
    IF lo_generic IS INITIAL.
      RETURN.
    ENDIF.

    REPLACE ALL OCCURRENCES OF '[:type:]'
      IN lv_regex
      WITH determine_type_prefix( lo_generic ).

    compare( iv_name     = lv_name
             iv_regex    = lv_regex
             iv_relative = 2 ).

  ENDMETHOD.


  METHOD check_data.

    DATA: lv_offset TYPE string,
          lv_name   TYPE string.


    lv_name = get_token_rel( 2 ).

    IF lv_name = 'BEGIN' AND get_token_rel( 3 ) = 'OF' AND mv_begin = 0.
      lv_name = get_token_rel( 4 ).
      mv_begin = 1.
      IF get_token_rel( 4 ) = 'COMMON' AND get_token_rel( 5 ) = 'PART'.
        RETURN.
      ENDIF.
    ELSEIF lv_name = 'BEGIN' AND get_token_rel( 3 ) = 'OF'.
      mv_begin = mv_begin + 1.
      RETURN.
    ELSEIF lv_name = 'END' AND get_token_rel( 3 ) = 'OF'.
      mv_begin = mv_begin - 1.
      RETURN.
    ELSEIF mv_begin > 0.
      RETURN.
    ENDIF.

* remove old style length definitions
    FIND '(' IN lv_name MATCH OFFSET lv_offset.
    IF sy-subrc = 0.
      lv_name = lv_name(lv_offset).
    ENDIF.

    IF ms_naming-set_excatt = abap_true
        AND mo_stack->concatenate( ) = |\\TY:{ object_name }|
        AND is_global_exception_class( ) = abap_true.
      RETURN.
    ENDIF.

    data( lv_name ).

  ENDMETHOD.


  METHOD check_field_symbol.

    DATA: lo_generic TYPE REF TO cl_abap_comp_data_generic,
          lv_regex   TYPE string,
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

    REPLACE ALL OCCURRENCES OF '[:type:]'
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

      REPLACE ALL OCCURRENCES OF '[:type:]'
        IN lv_regex
        WITH determine_type_prefix( lo_generic ).

      compare( iv_name     = lv_name
               iv_regex    = lv_regex
               iv_relative = 2 ).
    ENDLOOP.

  ENDMETHOD.


  METHOD check_form.

    DATA: lt_tokens TYPE stokesx_tab,
          lv_scope  TYPE string,
          lv_name   TYPE string,
          ls_token  LIKE LINE OF lt_tokens.


    CLEAR mv_at.

    lv_name = get_token_rel( 2 ).

    mo_stack->push( '\FO:' && lv_name ).

    lt_tokens = qualify_tokens( ).
    LOOP AT lt_tokens INTO ls_token.
      CASE ls_token-type.
        WHEN sana_tok_field_def.
          lv_name = remove_value( ls_token-str ).

          data( iv_name  = lv_name
                iv_scope = lv_scope ).
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
          ls_interface TYPE rsfbintfv.


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

    IF skip_fm_parameters(
        iv_name       = lv_name
        is_parameters = ls_interface ) = abap_true.
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
          lv_regex TYPE string,
          ls_object_ns TYPE zcl_aoc_util_reg_atc_namespace=>ty_ns_object.


    lv_name = get_token_rel( 2 ).

    lv_regex = ms_naming-globals_fugr.
    REPLACE FIRST OCCURRENCE OF '[:nspace:]' IN lv_regex WITH ms_naming-globals_nspace.

    compare( iv_name     = lv_name
             iv_regex    = lv_regex
             iv_relative = 2 ).


    IF zcl_aoc_util_reg_atc_namespace=>is_in_namespace( iv_pgmid    = 'R3TR'
                                                        iv_object   = 'FUGR'
                                                        iv_obj_name = lv_name ) = abap_true.

      ls_object_ns = zcl_aoc_util_reg_atc_namespace=>split_ns_object( iv_pgmid    = 'R3TR'
                                                                      iv_object   = 'FUGR'
                                                                      iv_obj_name = lv_name ).

      mo_stack->push( '\PR:'
                  && ls_object_ns-namespace
                  && 'SAPL'
                  && ls_object_ns-object ).
    ELSE.
      mo_stack->push( '\PR:' && 'SAPL' && lv_name ).
    ENDIF.

  ENDMETHOD.


  METHOD check_inline_defs.

    DATA: ls_token LIKE LINE OF ref_scan->tokens,
          lv_name  TYPE string.


    LOOP AT ref_scan->tokens FROM statement_wa-from TO statement_wa-to INTO ls_token.

      FIND REGEX '^DATA\((\w+)\)$' IN ls_token-str SUBMATCHES lv_name.
      IF sy-subrc = 0.
        data( lv_name ).
      ENDIF.

      FIND REGEX '^FIELD-SYMBOL\((<\w+>)\)$' IN ls_token-str SUBMATCHES lv_name ##NO_TEXT.
      IF sy-subrc = 0.
        field_symbol( lv_name ).
      ENDIF.

    ENDLOOP.

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
    ELSEIF object_type = 'INTF' OR object_type = 'CLAS'.
      mo_stack->set( '\PR:' && program_name && '\TY:' && lv_name ).
    ELSE.
      mo_stack->push( '\TY:' && lv_name ).
    ENDIF.


    IF object_type = 'INTF'.
      lv_regex = ms_naming-globals_intf.
    ELSE.
      lv_regex = ms_naming-oo_oolint.
    ENDIF.
    REPLACE FIRST OCCURRENCE OF '[:nspace:]' IN lv_regex WITH ms_naming-globals_nspace.

    compare( iv_name     = lv_name
             iv_regex    = lv_regex
             iv_relative = 2 ).

  ENDMETHOD.


  METHOD check_method_definition.

    DATA: lt_tokens TYPE stokesx_tab,
          lv_scope  TYPE string,
          lv_name   TYPE string,
          ls_token  LIKE LINE OF lt_tokens.


    IF is_global_exception_class( ) = abap_true
        AND ms_naming-set_excpar = abap_true
        AND get_token_rel( 2 ) = 'CONSTRUCTOR'.
      RETURN.
    ENDIF.

    mo_stack->push( '\ME:' && get_token_rel( 2 ) ).

    lt_tokens = qualify_tokens( ).

    LOOP AT lt_tokens INTO ls_token.
      CASE ls_token-type.
        WHEN sana_tok_field_def.
          lv_name = remove_value( ls_token-str ).

          IF ms_naming-set_pmeth = abap_true AND is_parallel_method( lt_tokens ) = abap_true.
            CONTINUE.
          ENDIF.

          data( iv_name  = lv_name
                iv_scope = lv_scope ).
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

    DATA: lv_regex TYPE string,
          lv_name  TYPE string.


    lv_name = get_token_rel( 2 ).

    IF lv_name = 'BEGIN' AND get_token_rel( 3 ) = 'OF' AND mv_begin = 0.
      lv_name = get_token_rel( 4 ).
      mv_begin = 1.
    ELSEIF lv_name = 'BEGIN' AND get_token_rel( 3 ) = 'OF'.
      mv_begin = mv_begin + 1.
      RETURN.
    ELSEIF lv_name = 'END' AND get_token_rel( 3 ) = 'OF'.
      mv_begin = mv_begin - 1.
      RETURN.
    ELSEIF mv_begin > 0.
      RETURN.
    ENDIF.

    IF mo_stack->concatenate( ) CS '\ME:'
        OR mo_stack->concatenate( ) CS '\FO:'
        OR mo_stack->concatenate( ) CS '\FU:'.
      lv_regex = ms_naming-locals_ltypes.
    ELSEIF mo_stack->concatenate( ) CS '\TY:'.
      lv_regex = ms_naming-oo_ootype.
    ELSE.
      lv_regex = ms_naming-proc_pgtype.
    ENDIF.

    compare( iv_name     = lv_name
             iv_regex    = lv_regex
             iv_relative = 2 ).

  ENDMETHOD.


  METHOD compare.

    DATA: lv_regex   TYPE string,
          lv_include TYPE sobj_name.


    lv_regex = |^{ iv_regex }|.

    FIND REGEX lv_regex IN iv_name IGNORING CASE.
    IF sy-subrc <> 0.
      lv_include = mo_scan->get_include( statement_wa-level ).
      inform( p_sub_obj_name = lv_include
              p_line         = get_line_rel( iv_relative )
              p_column       = get_column_rel( iv_relative )
              p_position     = mv_position
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
      inform( p_sub_obj_name = mo_scan->get_include( statement_wa-level )
              p_line         = get_line_rel( 2 )
              p_column       = get_column_rel( 2 )
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '002'
              p_param_1      = lv_full ).
    ENDIF.

  ENDMETHOD.


  METHOD compiler_resolve_class.

    DATA: lv_full    TYPE string,
          lv_include TYPE sobj_name.


    lv_full = mo_stack->concatenate( ).

    IF lv_full IS INITIAL.
      RETURN.
    ENDIF.

    TRY.
        ro_class ?= mo_compiler->get_symbol_entry( lv_full ).
      CATCH cx_sy_move_cast_error ##NO_HANDLER.
    ENDTRY.
    IF ro_class IS INITIAL.
      lv_include = mo_scan->get_include( statement_wa-level ).
      inform( p_sub_obj_name = lv_include
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

    version     = '002'.
    position    = '069'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    set_defaults( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Bad naming, expected &1, got &2'(m01)
        iv_pcom = '"#EC CI_NAMING' ).

    insert_scimessage(
        iv_code = '002'
        iv_text = 'Unable to resolve &1'(m05) ).

    insert_scimessage(
        iv_code = '003'
        iv_text = 'Error qualifying tokens'(m02) ).

    insert_scimessage(
        iv_code = '004'
        iv_text = 'Unable to resolve &1'(m05) ).

    insert_scimessage(
        iv_code = '005'
        iv_text = 'Syntax error'(m03) ).

    insert_scimessage(
        iv_code = '006'
        iv_text = 'Error reading FM parameters'(m04) ).

  ENDMETHOD.


  METHOD data.

    DATA: lo_generic TYPE REF TO cl_abap_comp_data_generic,
          lv_regex   TYPE string.


    lo_generic = compiler_resolve( '\DA:' && iv_name ).
    IF lo_generic IS INITIAL.
      RETURN.
    ENDIF.

    IF iv_scope IS INITIAL.
      lv_regex = determine_scope_prefix( ).
    ELSE.
      lv_regex = iv_scope.
    ENDIF.

    REPLACE ALL OCCURRENCES OF '[:type:]'
      IN lv_regex
      WITH determine_type_prefix( lo_generic ).

    compare( iv_name     = iv_name
             iv_regex    = lv_regex
             iv_relative = 2 ).

  ENDMETHOD.


  METHOD determine_scope_prefix.

    DATA: lv_keyword TYPE string.

    lv_keyword = keyword( ).

    IF lv_keyword = 'STATICS'.
      rv_prefix = ms_naming-locals_static.
    ELSEIF mo_stack->concatenate( ) CS '\ME:'
        OR mo_stack->concatenate( ) CS '\FO:'
        OR mo_stack->concatenate( ) CS '\FU:'.
      rv_prefix = ms_naming-locals_data.
    ELSEIF mo_stack->concatenate( ) CS '\TY:'
        AND lv_keyword = 'DATA'.
      rv_prefix = ms_naming-oo_oodata.
    ELSEIF mo_stack->concatenate( ) CS '\TY:'
        AND lv_keyword = 'CLASS-DATA'.
      rv_prefix = ms_naming-oo_oocdat.
    ELSE.
      rv_prefix = ms_naming-proc_pgdata.
    ENDIF.

  ENDMETHOD.


  METHOD determine_type_prefix.


    CONSTANTS:
      "! cl_abap_comp_type=>type_kind_ddic_dbtab does not exists in 731
      lc_type_kind_ddic_dbtab TYPE scr_typekind VALUE `7`,
      "! cl_abap_comp_type=>type_kind_enum does not exists before 751
      lc_type_kind_enum TYPE scr_typekind VALUE `8`,
      lc_type_kind_ddic_entity TYPE scr_typekind VALUE `9`.

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
      WHEN cl_abap_comp_type=>type_kind_elementary OR lc_type_kind_enum.
        CASE lo_type_symbol->full_name.
          WHEN '\PT:ANY' OR '\PT:DATA'.
            rv_prefix = ms_naming-prefix_generi.
          WHEN OTHERS.
            rv_prefix = ms_naming-prefix_elemen.
        ENDCASE.
      WHEN cl_abap_comp_type=>type_kind_structure OR lc_type_kind_ddic_dbtab OR lc_type_kind_ddic_entity.
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
                "further check, if class is an unresolved exception class
                IF is_unresolved_exception_class( lo_type_symbol_class->full_name ) = abap_true.
                  rv_prefix = ms_naming-prefix_rexcep.
                ENDIF.
            ENDCASE.

          WHEN OTHERS.
            rv_prefix = ms_naming-prefix_rdata.
        ENDCASE.
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.


  METHOD field_symbol.

    DATA: lo_generic TYPE REF TO cl_abap_comp_data_generic,
          lv_regex   TYPE string.


    lo_generic = compiler_resolve( '\DA:' && iv_name ).
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

    REPLACE ALL OCCURRENCES OF '[:type:]'
      IN lv_regex
      WITH determine_type_prefix( lo_generic ).

    compare( iv_name     = iv_name
             iv_regex    = lv_regex
             iv_relative = 2 ).

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      ms_naming = ms_naming
      TO DATA BUFFER p_attributes.

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


  METHOD is_global_exception_class.

    DATA: lo_super TYPE REF TO cl_abap_comp_class,
          lv_name  TYPE seoclsname.


    IF object_type <> 'CLAS'.
      RETURN.
    ENDIF.

    lv_name = object_name.
    IF cl_oo_classname_service=>get_ccdef_name( lv_name )
          = mo_scan->get_include( statement_wa-level )
        OR cl_oo_classname_service=>get_ccimp_name( lv_name )
          = mo_scan->get_include( statement_wa-level ).
      RETURN.
    ENDIF.

    lo_super = compiler_resolve_class( ).
    IF lo_super IS INITIAL.
      RETURN.
    ENDIF.
    WHILE NOT lo_super->super_class IS INITIAL.
      lo_super = lo_super->super_class.
    ENDWHILE.

    rv_bool = boolc( lo_super->full_name = '\TY:CX_ROOT' ).

  ENDMETHOD.


  METHOD is_parallel_method.

    DATA: ls_token LIKE LINE OF it_tokens.

    IF lines( it_tokens ) <> 6.
      RETURN.
    ENDIF.

    READ TABLE it_tokens INDEX 3 INTO ls_token.
    IF sy-subrc <> 0 OR ls_token-str <> 'IMPORTING'.
      RETURN.
    ENDIF.
    READ TABLE it_tokens INDEX 4 INTO ls_token.
    IF sy-subrc <> 0 OR ls_token-str <> 'P_TASK'.
      RETURN.
    ENDIF.
    READ TABLE it_tokens INDEX 5 INTO ls_token.
    IF sy-subrc <> 0 OR ls_token-str <> 'TYPE'.
      RETURN.
    ENDIF.
    READ TABLE it_tokens INDEX 6 INTO ls_token.
    IF sy-subrc <> 0 OR ls_token-str <> 'CLIKE'.
      RETURN.
    ENDIF.

    rv_bool = abap_true.

  ENDMETHOD.


  METHOD is_unresolved_exception_class.

    DATA lv_name   TYPE program.
    DATA lv_prefix TYPE string.
    DATA lo_compiler TYPE REF TO cl_abap_compiler.
    DATA lo_class TYPE REF TO cl_abap_comp_class.


    TRY.
        rv_is_an_excpcls = abap_false.
        SPLIT iv_class_fullname AT '\TY:' INTO lv_prefix lv_name.
        OVERLAY lv_name WITH '==============================CP'.
        lo_compiler = cl_abap_compiler=>create( lv_name ).
        IF lo_compiler IS NOT BOUND.
          RETURN.
        ENDIF.
        lo_class ?= lo_compiler->get_symbol_entry( iv_class_fullname ).
        IF lo_class IS BOUND.
          WHILE lo_class->super_class IS BOUND.
            lo_class = lo_class->super_class.
          ENDWHILE.
          IF lo_class->full_name = '\TY:CX_ROOT'.
            rv_is_an_excpcls = abap_true.
          ENDIF.
        ENDIF.
      CATCH cx_sy_move_cast_error ##NO_HANDLER.
    ENDTRY.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      ms_naming = ms_naming
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD qualify_tokens.

    DATA:
      lv_lines TYPE i.

    INSERT LINES OF ref_scan->tokens FROM statement_wa-from
      TO statement_wa-to INTO TABLE rt_tokens.

    lv_lines = lines( rt_tokens ).

    CALL FUNCTION 'RS_QUALIFY_ABAP_TOKENS_STR'
      EXPORTING
        statement_type        = statement_wa-type
        index_from            = 1
        index_to              = lv_lines
      CHANGING
        stokesx_tab           = rt_tokens
      EXCEPTIONS
        error_load_pattern    = 1
        unknown_keyword       = 2
        no_matching_statement = 3
        meaningless_statement = 4
        OTHERS                = 5.
    IF sy-subrc <> 0.
      inform( p_sub_obj_name = mo_scan->get_include( statement_wa-level )
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
    ms_naming-oo_oolxcl = 'LCX_'.
    ms_naming-oo_oolint = 'LIF_'.

    ms_naming-set_excpar = abap_true.
    ms_naming-set_excatt = abap_true.
    ms_naming-set_exccon = abap_true.
    ms_naming-set_cfunc  = abap_true.
    ms_naming-set_idocfm = abap_true.
    ms_naming-set_bwext  = abap_true.
    ms_naming-set_syntax = abap_true.
    ms_naming-set_pmeth  = abap_true.
    ms_naming-set_shlp   = abap_true.

  ENDMETHOD.


  METHOD skip_fm_parameters.

    DATA: ls_check     TYPE rsfbintfv,
          ls_parameter TYPE rsfbpara.


    DEFINE _append.
      ls_parameter-parameter = &2.
      APPEND ls_parameter TO ls_check-&1.
    END-OF-DEFINITION.

* idoc processing function module
    IF ms_naming-set_idocfm = abap_true
        AND rv_skip = abap_false.
      "IDOC Input
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
      "IDOC Output
      IF rv_skip = abap_false.
        CLEAR ls_check.
        _append import 'OBJECT'.
        _append import 'CONTROL_RECORD_IN'.
        _append export 'OBJECT_TYPE'.
        _append export 'CONTROL_RECORD_OUT'.
        _append tables 'INT_EDIDD'.

        rv_skip = skip_fm_parameters_check( is_parameters = is_parameters
                                            is_check      = ls_check ).
      ENDIF.
    ENDIF.

* conversion exits
    IF ms_naming-set_cfunc = abap_true
        AND rv_skip = abap_false
        AND ( iv_name CP 'CONVERSION_EXIT_*_INPUT'
        OR iv_name CP 'CONVERSION_EXIT_*_OUTPUT' ).

      CLEAR ls_check.
      _append import 'INPUT'.
      _append export 'OUTPUT'.

      rv_skip = skip_fm_parameters_check( is_parameters = is_parameters
                                          is_check      = ls_check ).
    ENDIF.

* BW extractors
    IF ms_naming-set_bwext = abap_true
        AND rv_skip = abap_false.
      CLEAR ls_check.
      _append import 'I_REQUNR'.
      _append import 'I_DSOURCE'.
      _append import 'I_ISOURCE'.
      _append import 'I_MAXSIZE'.
      _append import 'I_INITFLAG'.
      _append import 'I_UPDMODE'.
      _append import 'I_DATAPAKID'.
      _append import 'I_PRIVATE_MODE'.
      _append import 'I_CALLMODE'.
      _append import 'I_REMOTE_CALL'.
      _append tables 'I_T_SELECT'.
      _append tables 'I_T_FIELDS'.
      _append tables 'E_T_DATA'.

      rv_skip = skip_fm_parameters_check( is_parameters = is_parameters
                                          is_check      = ls_check ).

      IF rv_skip = abap_false.
        CLEAR ls_check.
        _append import 'I_REQUNR'.
        _append import 'I_DSOURCE'.
        _append import 'I_CHABASNM'.
        _append import 'I_MAXSIZE'.
        _append import 'I_INITFLAG'.
        _append import 'I_UPDMODE'.
        _append import 'I_DATAPAKID'.
        _append import 'I_S_TIMEINT'.
        _append import 'I_PRIVATE_MODE'.
        _append tables 'I_T_SELECT'.
        _append tables 'I_T_FIELDS'.
        _append tables 'E_T_DATA'.

        rv_skip = skip_fm_parameters_check( is_parameters = is_parameters
                                            is_check      = ls_check ).
      ENDIF.

      IF rv_skip = abap_false.
        CLEAR ls_check.
        _append import 'I_REQUNR'.
        _append import 'I_ISOURCE'.
        _append import 'I_MAXSIZE'.
        _append import 'I_INITFLAG'.
        _append import 'I_UPDMODE'.
        _append import 'I_DATAPAKID'.
        _append import 'I_PRIVATE_MODE'.
        _append import 'I_CALLMODE'.
        _append import 'I_REMOTE_CALL'.
        _append tables 'I_T_SELECT'.
        _append tables 'I_T_FIELDS'.
        _append tables 'E_T_DATA'.
        _append tables 'E_T_SOURCE_STRUCTURE_NAME'.

        rv_skip = skip_fm_parameters_check( is_parameters = is_parameters
                                            is_check      = ls_check ).
      ENDIF.

      IF rv_skip = abap_false.
        CLEAR ls_check.
        _append import 'I_REQUNR'.
        _append import 'I_DSOURCE'.
        _append import 'I_MAXSIZE'.
        _append import 'I_INITFLAG'.
        _append import 'I_READ_ONLY'.
        _append import 'I_REMOTE_CALL'.
        _append import 'I_PRIVATE_MODE'.
        _append tables 'I_T_SELECT'.
        _append tables 'I_T_FIELDS'.
        _append tables 'E_T_DATA'.

        rv_skip = skip_fm_parameters_check( is_parameters = is_parameters
                                            is_check      = ls_check ).
      ENDIF.

      IF rv_skip = abap_false.
        CLEAR ls_check.
        _append import 'I_REQUNR'.
        _append import 'I_ISOURCE'.
        _append import 'I_MAXSIZE'.
        _append import 'I_INITFLAG'.
        _append import 'I_UPDMODE'.
        _append import 'I_DATAPAKID'.
        _append import 'I_PRIVATE_MODE'.
        _append import 'I_CALLMODE'.
        _append import 'I_REMOTE_CALL'.
        _append tables 'I_T_SELECT'.
        _append tables 'I_T_FIELDS'.
        _append tables 'E_T_DATA'.
        _append tables 'E_T_SELECT'.

        rv_skip = skip_fm_parameters_check( is_parameters = is_parameters
                                            is_check      = ls_check ).
      ENDIF.

      IF rv_skip = abap_false.
        CLEAR ls_check.
        _append import 'I_REQUNR'.
        _append import 'I_CHABASNM'.
        _append import 'I_MAXSIZE'.
        _append import 'I_INITFLAG'.
        _append import 'I_UPDMODE'.
        _append import 'I_DATAPAKID'.
        _append import 'I_S_TIMEINT'.
        _append import 'I_REMOTE_CALL'.
        _append tables 'I_T_SELECT'.
        _append tables 'I_T_FIELDS'.
        _append tables 'E_T_SOURCE_STRUCTURE_NAME'.

        rv_skip = skip_fm_parameters_check( is_parameters = is_parameters
                                            is_check      = ls_check ).
      ENDIF.

      IF rv_skip = abap_false.
        CLEAR ls_check.
        _append import 'I_REQUNR'.
        _append import 'I_RLOGSYS'.
        _append import 'I_UPDMODE'.
        _append import 'I_ISOURCE'.
        _append import 'I_S_PARAMS'.
        _append import 'I_INITFLAG'.
        _append import 'I_DATAPAKID'.
        _append import 'I_READ_ONLY'.
        _append import 'I_REMOTE_CALL'.
        _append tables 'I_T_SELECT'.
        _append tables 'I_T_FIELDS'.
        _append tables 'E_T_DATA'.

        rv_skip = skip_fm_parameters_check( is_parameters = is_parameters
                                            is_check      = ls_check ).
      ENDIF.

      IF rv_skip = abap_false.
        CLEAR ls_check.
        _append import 'I_REQUNR'.
        _append import 'I_CHABASNM'.
        _append import 'I_MAXSIZE'.
        _append import 'I_INITFLAG'.
        _append import 'I_UPDMODE'.
        _append import 'I_DATAPAKID'.
        _append import 'I_S_TIMEINT'.
        _append import 'I_REMOTE_CALL'.
        _append tables 'I_T_LANGU'.
        _append tables 'I_T_SELECT'.
        _append tables 'E_T_TEXTS'.

        rv_skip = skip_fm_parameters_check( is_parameters = is_parameters
                                            is_check      = ls_check ).
      ENDIF.
    ENDIF.

* search help exits
    IF ms_naming-set_shlp = abap_true
        AND rv_skip = abap_false.

      CLEAR ls_check.
      _append tables 'SHLP_TAB'.
      _append tables 'RECORD_TAB'.
      _append change 'SHLP'.
      _append change 'CALLCONTROL'.

      rv_skip = skip_fm_parameters_check( is_parameters = is_parameters
                                          is_check      = ls_check ).

    ENDIF.

* idoc port function module, txn WE21 -> ABAP-PI
    IF ms_naming-set_port = abap_true
        AND rv_skip = abap_false.

      CLEAR ls_check.
      _append import 'I_WAIT'.
      _append tables 'I_EDIDC'.

      rv_skip = skip_fm_parameters_check( is_parameters = is_parameters
                                          is_check      = ls_check ).

    ENDIF.

* idoc master idoc distribution, TBDME-IDOCFBNAME
    IF ms_naming-set_port = abap_true
        AND rv_skip = abap_false.

      CLEAR ls_check.
      _append import 'MESSAGE_TYPE'.

      rv_skip = skip_fm_parameters_check( is_parameters = is_parameters
                                          is_check      = ls_check ).

    ENDIF.

  ENDMETHOD.


  METHOD skip_fm_parameters_check.

    DATA ls_parameter LIKE LINE OF is_check-import.


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
