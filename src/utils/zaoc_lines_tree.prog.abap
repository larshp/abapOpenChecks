REPORT zaoc_lines_tree.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

TABLES: tdevc.

SELECT-OPTIONS: s_devc FOR tdevc-devclass DEFAULT '$AOC' OBLIGATORY.

PARAMETERS: p_comm TYPE c AS CHECKBOX DEFAULT 'X',
            p_vfug TYPE c AS CHECKBOX DEFAULT 'X',
            p_loca TYPE c AS CHECKBOX DEFAULT 'X',
            p_gate TYPE c AS CHECKBOX DEFAULT 'X'.

DATA: gv_ok_code LIKE sy-ucomm.

START-OF-SELECTION.
  CALL SCREEN 2000.

CLASS lcl_logic DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_result,
             package TYPE devclass,
             parent  TYPE devclass,
             count   TYPE i,
           END OF ty_result.

    TYPES: ty_result_tt TYPE STANDARD TABLE OF ty_result WITH DEFAULT KEY.

    CLASS-METHODS: run RETURNING VALUE(rt_result) TYPE ty_result_tt.

  PRIVATE SECTION.
    CLASS-DATA: gt_result TYPE ty_result_tt.

    CLASS-METHODS:
      run_package
        IMPORTING iv_devclass     TYPE devclass
                  iv_parent       TYPE devclass
        RETURNING VALUE(rv_lines) TYPE i,
      count_package
        IMPORTING iv_devclass     TYPE devclass
        RETURNING VALUE(rv_lines) TYPE i,
      find_subpackages
        IMPORTING iv_devclass   TYPE devclass
        RETURNING VALUE(rt_sub) TYPE cl_pak_package_queries=>tt_subpackage_info.

ENDCLASS.

CLASS lcl_logic IMPLEMENTATION.

  METHOD run.

    DATA: lt_tdevc TYPE STANDARD TABLE OF tdevc WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_tdevc> LIKE LINE OF lt_tdevc.


    SELECT * FROM tdevc INTO TABLE lt_tdevc
      WHERE devclass IN s_devc
      AND parentcl = ''
      ORDER BY PRIMARY KEY.

    CLEAR gt_result.
    LOOP AT lt_tdevc ASSIGNING <ls_tdevc>.
      run_package( iv_devclass = <ls_tdevc>-devclass
                   iv_parent   = '' ).
    ENDLOOP.
    rt_result = gt_result.

  ENDMETHOD.

  METHOD count_package.

    DATA: lt_includes TYPE scit_program,
          lt_source   TYPE TABLE OF abaptxt255,
          lv_include  LIKE LINE OF lt_includes.


    lt_includes = zcl_aoc_util_programs=>get_programs_in_package(
      iv_devclass           = iv_devclass
      iv_ignore_mview_fugr  = p_vfug
      iv_ignore_local_tests = p_loca
      iv_ignore_gateway     = p_gate ).

    LOOP AT lt_includes INTO lv_include.
      IF sy-tabix MOD 100 = 0.
        cl_progress_indicator=>progress_indicate(
            i_text               = iv_devclass
            i_processed          = sy-tabix
            i_total              = lines( lt_includes )
            i_output_immediately = abap_true ).
      ENDIF.

      CALL FUNCTION 'RPY_PROGRAM_READ'
        EXPORTING
          program_name     = lv_include
          with_includelist = abap_false
          only_source      = abap_true
          with_lowercase   = abap_true
        TABLES
          source_extended  = lt_source
        EXCEPTIONS
          cancelled        = 1
          not_found        = 2
          permission_error = 3
          OTHERS           = 4.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF p_comm = abap_true.
        DELETE lt_source WHERE line CP '#**'.
      ENDIF.

      rv_lines = rv_lines + lines( lt_source ).

    ENDLOOP.

  ENDMETHOD.

  METHOD run_package.

    DATA: lt_sub TYPE cl_pak_package_queries=>tt_subpackage_info,
          ls_sub LIKE LINE OF lt_sub.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF gt_result.


    APPEND INITIAL LINE TO gt_result ASSIGNING <ls_result>.
    <ls_result>-package = iv_devclass.
    <ls_result>-parent  = iv_parent.

    lt_sub = find_subpackages( iv_devclass ).
    LOOP AT lt_sub INTO ls_sub USING KEY package.
      rv_lines = rv_lines + run_package( iv_devclass = ls_sub-package
                                         iv_parent   = iv_devclass ).
    ENDLOOP.

    rv_lines = rv_lines + count_package( iv_devclass ).

    <ls_result>-count = rv_lines.

  ENDMETHOD.

  METHOD find_subpackages.

    cl_pak_package_queries=>get_direct_subpackages(
      EXPORTING
        im_package                    = iv_devclass
        im_also_local_packages        = abap_true
      IMPORTING
        et_subpackages                = rt_sub
      EXCEPTIONS
        no_package_specified          = 1
        package_has_no_tdevc_record   = 2
        package_has_no_tadir_record   = 3
        package_does_not_exist        = 4
        invalid_superpackage          = 5
        no_output_parameter_requested = 6
        OTHERS                        = 7 ).              "#EC CI_SUBRC

  ENDMETHOD.

ENDCLASS.

CLASS lcl_gui DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      init,
      refresh_data.

  PRIVATE SECTION.
    CLASS-DATA:
      go_container TYPE REF TO cl_gui_custom_container,
      go_tree      TYPE REF TO cl_gui_column_tree.

ENDCLASS.

CLASS lcl_gui IMPLEMENTATION.

  METHOD init.

    DATA: ls_hierarchy_header TYPE treev_hhdr.

    IF NOT go_container IS INITIAL.
      RETURN.
    ENDIF.

    CREATE OBJECT go_container
      EXPORTING
        container_name              = 'CUSTOM_2000'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5.
    ASSERT sy-subrc = 0.

    ls_hierarchy_header-heading = 'Package'(c01).
    ls_hierarchy_header-width = 100.

    CREATE OBJECT go_tree
      EXPORTING
        parent                      = go_container
        node_selection_mode         = cl_gui_column_tree=>node_sel_mode_single
        item_selection              = abap_true
        hierarchy_column_name       = 'DEVCLASS'
        hierarchy_header            = ls_hierarchy_header
      EXCEPTIONS
        cntl_system_error           = 1
        create_error                = 2
        failed                      = 3
        illegal_node_selection_mode = 4
        illegal_column_name         = 5
        lifetime_error              = 6.
    ASSERT sy-subrc = 0.

    go_tree->add_column(
      EXPORTING
        name                         = 'LINES'
        width                        = 100
        alignment                    = cl_gui_column_tree=>align_right
        header_text                  = 'Lines'(c02)
      EXCEPTIONS
        column_exists                = 1
        illegal_column_name          = 2
        too_many_columns             = 3
        illegal_alignment            = 4
        different_column_types       = 5
        cntl_system_error            = 6
        failed                       = 7
        predecessor_column_not_found = 8 ).               "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

    refresh_data( ).

  ENDMETHOD.

  METHOD refresh_data.

    TYPES: item_table_type TYPE STANDARD TABLE OF mtreeitm WITH DEFAULT KEY.

    DATA: lt_node_table TYPE treev_ntab,
          ls_node       LIKE LINE OF lt_node_table,
          lt_item_table TYPE item_table_type,
          ls_item       LIKE LINE OF lt_item_table,
          lt_result     TYPE lcl_logic=>ty_result_tt,
          lv_key        LIKE ls_node-node_key,
          lv_parent     LIKE ls_node-node_key,
          lv_total      TYPE i,
          lv_string     TYPE string,
          ls_result     LIKE LINE OF lt_result.


    lt_result = lcl_logic=>run( ).

    LOOP AT lt_result INTO ls_result WHERE parent = ''.
      lv_total = lv_total + ls_result-count.
    ENDLOOP.
    lv_string = |Grand Total: { lv_total }|.
    MESSAGE lv_string TYPE 'S'.

    LOOP AT lt_result INTO ls_result.
      lv_key = sy-tabix.

      CLEAR ls_node.
      ls_node-node_key = lv_key.
      READ TABLE lt_result WITH KEY package = ls_result-parent TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        lv_parent = sy-tabix.
        ls_node-relatkey = lv_parent.
      ENDIF.
      READ TABLE lt_result WITH KEY parent = ls_result-package TRANSPORTING NO FIELDS.
      IF sy-subrc = 0.
        ls_node-isfolder = abap_true.
      ENDIF.
      APPEND ls_node TO lt_node_table.

      CLEAR ls_item.
      ls_item-node_key = lv_key.
      ls_item-item_name = 'DEVCLASS'.
      ls_item-class = cl_gui_column_tree=>item_class_text.
      ls_item-text = ls_result-package.
      APPEND ls_item TO lt_item_table.

      CLEAR ls_item.
      ls_item-node_key = lv_key.
      ls_item-item_name = 'LINES'.
      ls_item-class = cl_gui_column_tree=>item_class_text.
      ls_item-text = ls_result-count.
      APPEND ls_item TO lt_item_table.

    ENDLOOP.

    go_tree->add_nodes_and_items(
      EXPORTING
        node_table                     = lt_node_table
        item_table                     = lt_item_table
        item_table_structure_name      = 'MTREEITM'
      EXCEPTIONS
        failed                         = 1
        cntl_system_error              = 3
        error_in_tables                = 4
        dp_error                       = 5
        table_structure_name_not_found = 6 ).             "#EC CI_SUBRC
    ASSERT sy-subrc = 0.

    LOOP AT lt_node_table INTO ls_node WHERE isfolder = abap_true AND relatkey IS INITIAL.
      go_tree->expand_node(
        EXPORTING
          node_key            = ls_node-node_key
        EXCEPTIONS
          failed              = 1
          illegal_level_count = 2
          cntl_system_error   = 3
          node_not_found      = 4
          cannot_expand_leaf  = 5 ).                      "#EC CI_SUBRC
      ASSERT sy-subrc = 0.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2000 INPUT.

  CASE gv_ok_code.
    WHEN 'BACK'.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*& Module STATUS_2000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE status_2000 OUTPUT.
  SET PF-STATUS 'STATUS_2000'.
  SET TITLEBAR 'TITLE_2000'.
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module INIT_2000 OUTPUT
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
MODULE init_2000 OUTPUT.
  lcl_gui=>init( ).
ENDMODULE.
