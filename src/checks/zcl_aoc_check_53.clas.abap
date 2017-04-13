CLASS zcl_aoc_check_53 DEFINITION PUBLIC INHERITING FROM zcl_aoc_super CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
    METHODS get_attributes
        REDEFINITION .
    METHODS get_message_text
        REDEFINITION .
    METHODS put_attributes
        REDEFINITION .
    METHODS if_ci_test~query_attributes
        REDEFINITION .
  PROTECTED SECTION.

    DATA mv_reuse_alv_grid_display TYPE sap_bool .
    DATA mv_so_new_document_att_send TYPE sap_bool .
    DATA mv_sapgui_progress_indicator TYPE sap_bool .
    DATA mv_subst_get_file_list TYPE sap_bool .
    DATA mv_gui TYPE sap_bool .
    DATA mv_job TYPE sap_bool .
    DATA mv_popup_to_decide TYPE sap_bool .
    DATA mv_round TYPE sap_bool .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_53 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_statements TYPE ty_statements,
          lt_split      TYPE STANDARD TABLE OF string WITH DEFAULT KEY,
          lv_name       TYPE string,
          lv_code       TYPE sci_errc.

    FIELD-SYMBOLS: <ls_statement> LIKE LINE OF lt_statements.


    lt_statements = build_statements(
        it_tokens     = it_tokens
        it_statements = it_statements
        it_levels     = it_levels ).

    LOOP AT lt_statements ASSIGNING <ls_statement>.
      CLEAR lv_code.

      IF <ls_statement>-str NP 'CALL FUNCTION *'.
        CONTINUE.
      ENDIF.

      SPLIT <ls_statement>-str AT '''' INTO TABLE lt_split.
      IF lines( lt_split ) < 2.
        CONTINUE.
      ENDIF.

      READ TABLE lt_split INDEX 2 INTO lv_name.
      ASSERT sy-subrc = 0.

      CASE lv_name.
        WHEN 'REUSE_ALV_GRID_DISPLAY'.
          IF mv_reuse_alv_grid_display = abap_true.
            lv_code = '001'.
          ENDIF.
        WHEN 'SO_NEW_DOCUMENT_ATT_SEND_API1'.
          IF mv_so_new_document_att_send = abap_true.
            lv_code = '002'.
          ENDIF.
        WHEN 'SAPGUI_PROGRESS_INDICATOR'.
          IF mv_sapgui_progress_indicator = abap_true.
            lv_code = '003'.
          ENDIF.
        WHEN 'SUBST_GET_FILE_LIST'.
          IF mv_subst_get_file_list = abap_true.
            lv_code = '004'.
          ENDIF.
        WHEN 'JOB_CREATE' OR 'JOB_SUBMIT'.
          IF mv_job = abap_true.
            lv_code = '005'.
          ENDIF.
        WHEN 'POPUP_TO_DECIDE'.
          IF mv_popup_to_decide = abap_true.
            lv_code = '006'.
          ENDIF.
        WHEN 'ROUND'.
          IF mv_round = abap_true.
            lv_code = '007'.
          ENDIF.
        WHEN 'GUI_DOWNLOAD' OR 'GUI_UPLOAD'.
          IF mv_gui = abap_true.
            lv_code = '008'.
          ENDIF.
      ENDCASE.

      IF NOT lv_code IS INITIAL.
        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = <ls_statement>-include
                p_line         = <ls_statement>-start-row
                p_param_1      = lv_name
                p_kind         = mv_errty
                p_test         = myname
                p_code         = lv_code ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    description    = 'Function module recommendations'.     "#EC NOTEXT
    category       = 'ZCL_AOC_CATEGORY'.
    version        = '001'.
    position       = '053'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

    mv_reuse_alv_grid_display    = abap_true.
    mv_so_new_document_att_send  = abap_true.
    mv_sapgui_progress_indicator = abap_true.
    mv_subst_get_file_list       = abap_true.
    mv_job                       = abap_true.
    mv_popup_to_decide           = abap_true.
    mv_round                     = abap_true.
    mv_gui                       = abap_true.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_attributes.

    EXPORT
      mv_errty                     = mv_errty
      mv_reuse_alv_grid_display    = mv_reuse_alv_grid_display
      mv_so_new_document_att_send  = mv_so_new_document_att_send
      mv_sapgui_progress_indicator = mv_sapgui_progress_indicator
      mv_subst_get_file_list       = mv_subst_get_file_list
      mv_job                       = mv_job
      mv_popup_to_decide           = mv_popup_to_decide
      mv_round                     = mv_round
      mv_gui                       = mv_gui
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    p_text = 'Function &1 used, see Wiki'.                  "#EC NOTEXT

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_reuse_alv_grid_display
      'REUSE_ALV_GRID_DISPLAY' ''.                          "#EC NOTEXT
    zzaoc_fill_att mv_so_new_document_att_send
      'SO_NEW_DOCUMENT_ATT_SEND_API1' ''.                   "#EC NOTEXT
    zzaoc_fill_att mv_sapgui_progress_indicator
      'SAPGUI_PROGRESS_INDICATOR' ''.                       "#EC NOTEXT
    zzaoc_fill_att mv_subst_get_file_list
      'SUBST_GET_FILE_LIST' ''.                             "#EC NOTEXT
    zzaoc_fill_att mv_job 'JOB_CREATE, JOB_SUBMIT' ''.      "#EC NOTEXT
    zzaoc_fill_att mv_popup_to_decide 'POPUP_TO_DECIDE' ''. "#EC NOTEXT
    zzaoc_fill_att mv_round 'ROUND' ''.                     "#EC NOTEXT
    zzaoc_fill_att mv_gui 'GUI_DOWNLOAD and GUI_UPLOAD' ''. "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty                     = mv_errty
      mv_reuse_alv_grid_display    = mv_reuse_alv_grid_display
      mv_so_new_document_att_send  = mv_so_new_document_att_send
      mv_sapgui_progress_indicator = mv_sapgui_progress_indicator
      mv_subst_get_file_list       = mv_subst_get_file_list
      mv_job                       = mv_job
      mv_popup_to_decide           = mv_popup_to_decide
      mv_round                     = mv_round
      mv_gui                       = mv_gui
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.