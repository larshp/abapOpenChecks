CLASS zcl_aoc_check_64 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super_root
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS get_message_text
        REDEFINITION .
    METHODS run
        REDEFINITION .
  PROTECTED SECTION.
    DATA mv_covered TYPE abap_bool .
    DATA mi_result TYPE REF TO if_scv_result .

    METHODS node
      IMPORTING
        !ii_node TYPE REF TO if_scv_result_node .
    METHODS walk
      IMPORTING
        !ir_node TYPE REF TO if_scv_result_node .

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_64 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    description    = 'Unit test not covering class'.        "#EC NOTEXT
    category       = 'ZCL_AOC_CATEGORY'.
    version        = '001'.
    position       = '064'.

    has_documentation = c_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

    add_obj_type( 'CLAS' ).

  ENDMETHOD.                    "CONSTRUCTOR


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


  METHOD node.

    DATA: lv_pb_type    TYPE cvd_pb_type,
          lv_pb_name    TYPE cvd_pb_name,
          lv_prog_class TYPE cvd_prog_class,
          lv_prog_type  TYPE cvd_prog_type,
          lv_prog_name  TYPE cvd_prog_name.


    CASE ii_node->subtype.
      WHEN 'METH'.
        DATA(lo_insp) = cl_scv_pblock_inspector=>create( ii_node ).
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

    DATA(lo_ui_factory) = NEW cl_scov_stmnt_cov_ui_factory( ).

    DATA(lo_pb_info) = lo_ui_factory->create_pb_info(
      im_pb_type    = lv_pb_type
      im_pb_name    = lv_pb_name
      im_prog_class = lv_prog_class
      im_prog_type  = lv_prog_type
      im_prog_name  = lv_prog_name ).

    DATA(lt_tkey_selops) = VALUE cvt_test_key_selops( (
      option = 'EQ'
      sign   = 'I'
      low    = mi_result->get_measurement( )->get_testkey( ) ) ).

    DATA(li_container) = lo_ui_factory->create_stmnt_dcon_factory( lt_tkey_selops
      )->create_stmnt_data_container( lo_pb_info ).

*    DATA(lt_source) = li_container->get_source( ).

    DATA(lt_meta) = li_container->get_stmnt_cov_meta_data( ).

* 102 = covered
    READ TABLE lt_meta WITH KEY color = '102' TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      mv_covered = abap_true.
    ENDIF.

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
          lo_passport TYPE REF TO object.


    IF object_type <> 'CLAS'.
      RETURN.
    ENDIF.

    DATA(ls_info) = cl_aunit_prog_info=>get_program_info(
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
        i_limit_on_duration_category = if_aunit_attribute_enums=>c_duration-medium
        i_limit_on_risk_level        = if_aunit_attribute_enums=>c_risk_level-harmless
        i_program_keys               = lt_keys
      IMPORTING
        e_coverage_result            = li_coverage ).

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

    IF ir_node->has_children( ) = abap_false.
      node( ir_node ).
    ENDIF.

    LOOP AT ir_node->get_children( ) INTO DATA(li_node).
      walk( li_node ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
