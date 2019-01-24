CLASS zcl_aoc_check_64 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super_root
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS get_message_text
        REDEFINITION .
    METHODS if_ci_test~query_attributes
        REDEFINITION .
    METHODS put_attributes
        REDEFINITION .
    METHODS run
        REDEFINITION .
    METHODS get_attributes
        REDEFINITION .
  PROTECTED SECTION.

    DATA mv_covered TYPE abap_bool .
    DATA mi_result TYPE REF TO if_scv_result .
    DATA mv_risk TYPE saunit_d_allowed_risk_level .
    DATA mv_duration TYPE saunit_d_allowed_rt_duration .

    METHODS node
      IMPORTING
        !ii_node TYPE REF TO if_scv_result_node .
    METHODS walk
      IMPORTING
        !ii_node TYPE REF TO if_scv_result_node .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_64 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '064'.

    has_documentation = c_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty    = c_error.
    mv_duration = if_aunit_attribute_enums=>c_duration-medium.
    mv_risk     = if_aunit_attribute_enums=>c_risk_level-harmless.

    add_obj_type( 'CLAS' ).

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mv_duration = mv_duration
      mv_risk = mv_risk
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Unit test not covering class'.            "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_duration 'Duration' ''.               "#EC NOTEXT
    zzaoc_fill_att mv_risk 'Risk' ''.                       "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD node.

    DATA: lv_pb_type     TYPE cvd_pb_type,
          lv_pb_name     TYPE cvd_pb_name,
          lv_prog_class  TYPE cvd_prog_class,
          lv_prog_type   TYPE cvd_prog_type,
          lv_prog_name   TYPE cvd_prog_name,
          lo_pb_info     TYPE REF TO cl_scov_pb_info,
          lt_tkey_selops TYPE cvt_test_key_selops,
          ls_tkey_selops LIKE LINE OF lt_tkey_selops,
          lo_ui_factory  TYPE REF TO cl_scov_stmnt_cov_ui_factory,
          li_container   TYPE REF TO if_scov_stmnt_data_container,
          lt_meta        TYPE cvt_stmnt_cov_meta_data,
          lo_insp        TYPE REF TO cl_scv_pblock_inspector.


    CASE ii_node->subtype.
      WHEN 'METH'.
        lo_insp = cl_scv_pblock_inspector=>create( ii_node ).
        lv_pb_type    = 'METH'.
        lv_pb_name    = lo_insp->get_method_name( ).
        lv_prog_class = lo_insp->get_class_name( ).
        lv_prog_type  = lo_insp->get_program_subtype( ).
        lv_prog_name  = lo_insp->get_program_name( ).
      WHEN OTHERS.
        lv_pb_type   = ii_node->subtype.
        lv_pb_name   = ii_node->name.
        CLEAR lv_prog_class.
        lv_prog_type = ii_node->get_parent( )->subtype.
        lv_prog_name = ii_node->get_parent( )->name.
    ENDCASE.

    CREATE OBJECT lo_ui_factory.

    lo_pb_info = lo_ui_factory->create_pb_info(
      im_pb_type    = lv_pb_type
      im_pb_name    = lv_pb_name
      im_prog_class = lv_prog_class
      im_prog_type  = lv_prog_type
      im_prog_name  = lv_prog_name ).

    ls_tkey_selops-option = 'EQ'.
    ls_tkey_selops-sign   = 'I'.
    ls_tkey_selops-low    = mi_result->get_measurement( )->get_testkey( ).

    APPEND ls_tkey_selops TO lt_tkey_selops.

    li_container = lo_ui_factory->create_stmnt_dcon_factory( lt_tkey_selops
      )->create_stmnt_data_container( lo_pb_info ).

    lt_meta = li_container->get_stmnt_cov_meta_data( ).

* 102 = covered
    READ TABLE lt_meta WITH KEY color = '102' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      mv_covered = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mv_duration = mv_duration
      mv_risk = mv_risk
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD run.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: li_node     TYPE REF TO if_scv_result_node,
          lt_keys     TYPE sabp_t_tadir_keys,
          ls_key      LIKE LINE OF lt_keys,
          lv_clsname  TYPE seoclsname,
          lo_runner   TYPE REF TO cl_aucv_test_runner_abstract,
          li_coverage TYPE REF TO if_aucv_cvrg_rslt_provider,
          li_aunit    TYPE REF TO if_saunit_internal_result,
          lo_aunit    TYPE REF TO cl_saunit_internal_result,
          ls_info     TYPE if_aunit_prog_info_types=>ty_s_program,
          lo_passport TYPE REF TO object.


    IF object_type <> 'CLAS'.
      RETURN.
    ENDIF.

    ls_info = cl_aunit_prog_info=>get_program_info(
      allow_commit = abap_true
      obj_type = object_type
      obj_name = object_name ).
    IF ls_info-has_tests = abap_false.
      RETURN.
    ENDIF.

    CALL METHOD ('\PROGRAM=SAPLSAUCV_GUI_RUNNER\CLASS=PASSPORT')=>get
      RECEIVING
        result = lo_passport.

    lo_runner = cl_aucv_test_runner_coverage=>create( lo_passport ).

    ls_key-obj_name = object_name.
    ls_key-obj_type = object_type.
    APPEND ls_key TO lt_keys.

    lo_runner->run_for_program_keys(
      EXPORTING
        i_limit_on_duration_category = mv_duration
        i_limit_on_risk_level        = mv_risk
        i_program_keys               = lt_keys
      IMPORTING
        e_coverage_result            = li_coverage
        e_aunit_result               = li_aunit ).

    lo_aunit ?= li_aunit.
    IF lo_aunit->f_task_data-info-has_skipped = abap_true.
* Some or all unit tests skipped, could not determine coverage
      RETURN.
    ENDIF.

    TRY.
        mi_result = li_coverage->build_coverage_result( ).
      CATCH cx_scv_execution_error cx_scv_call_error.
        RETURN.
    ENDTRY.

    mv_covered = abap_false.

    LOOP AT mi_result->get_root_node( )->get_children( ) INTO li_node.
      walk( li_node ).
    ENDLOOP.

    IF mv_covered = abap_false.
      lv_clsname = object_name.
      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = cl_oo_classname_service=>get_ccau_name( lv_clsname )
              p_line         = 1
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '001' ).
    ENDIF.

  ENDMETHOD.


  METHOD walk.

    DATA: li_node TYPE REF TO if_scv_result_node.

    IF ii_node->has_children( ) = abap_false.
      node( ii_node ).
    ENDIF.

    LOOP AT ii_node->get_children( ) INTO li_node.
      walk( li_node ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
