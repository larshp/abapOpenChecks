CLASS zcl_aoc_check_31 DEFINITION
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
    METHODS get_message_text
         REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS gc_english TYPE slin_desc_t-language VALUE 'E'.

    DATA mt_error            TYPE zaoc_slin_desc_key_range_tt .
    DATA mt_warn             TYPE zaoc_slin_desc_key_range_tt .
    DATA mt_info             TYPE zaoc_slin_desc_key_range_tt .
    DATA mt_ignore           TYPE zaoc_slin_desc_key_range_tt .
    DATA mv_default_error    TYPE flag .
    DATA mv_default_standard TYPE flag .
    DATA mv_default_atc      TYPE flag .

    METHODS set_flags
      RETURNING
        VALUE(rs_flags) TYPE rslin_test_flags .
ENDCLASS.



CLASS ZCL_AOC_CHECK_31 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_obj_name TYPE sobj_name,
          lv_text     TYPE string,
          lv_tmp      TYPE string,
          ls_flags    TYPE rslin_test_flags,
          lv_code     TYPE sci_errc,
          lv_errty    TYPE sci_errty,
          lv_todo     TYPE slin_desc-todo_overlay,
          lt_result   TYPE slin_result.

    FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_result,
                   <ls_line>   LIKE LINE OF <ls_result>-lines.

    ls_flags = set_flags( ).
    CALL FUNCTION 'EXTENDED_PROGRAM_CHECK'
      EXPORTING
        program    = program_name
        test_flags = ls_flags
      IMPORTING
        result     = lt_result.

    LOOP AT lt_result ASSIGNING <ls_result>.            "#EC CI_SORTSEQ

      CLEAR lv_text.
      LOOP AT <ls_result>-lines ASSIGNING <ls_line>.
        CONCATENATE LINES OF cl_slin_io=>old_line_to_src( <ls_line> ) INTO lv_tmp.
        IF lv_text IS INITIAL.
          lv_text = lv_tmp.
        ELSE.
          CONCATENATE lv_text cl_abap_char_utilities=>newline lv_tmp INTO lv_text.
        ENDIF.
      ENDLOOP.

      lv_tmp = |(SLIN code key: { lv_code })|.
      CONCATENATE lv_text cl_abap_char_utilities=>newline lv_tmp INTO lv_text.

      IF lines( mt_error ) > 0 AND <ls_result>-code IN mt_error.
        lv_errty = c_error.
      ELSEIF lines( mt_warn ) > 0 AND <ls_result>-code IN mt_warn.
        lv_errty = c_warning.
      ELSEIF lines( mt_info ) > 0 AND <ls_result>-code IN mt_info.
        lv_errty = c_note.
      ELSEIF lines( mt_ignore ) > 0 AND <ls_result>-code IN mt_ignore.
        CONTINUE.
      ELSE.
        CASE abap_true.
          WHEN mv_default_error.
            lv_errty = c_error.
          WHEN mv_default_standard.
            CASE <ls_result>-kind.
              WHEN slin_errlv-warn.
                lv_errty = c_warning.
              WHEN slin_errlv-info.
                lv_errty = c_note.
              WHEN OTHERS.
                lv_errty = c_error.
            ENDCASE.
          WHEN mv_default_atc.
            SELECT SINGLE todo_overlay
              FROM slin_desc INTO lv_todo
              WHERE code_nr = <ls_result>-code.           "#EC CI_SUBRC
            CASE lv_todo.
              WHEN slin_todo-prio1.
                lv_errty = c_error.
              WHEN slin_todo-prio2.
                lv_errty = c_warning.
              WHEN slin_todo-prio3.
                lv_errty = c_note.
              WHEN slin_todo-noprio.
                CONTINUE. " current loop
            ENDCASE.
          WHEN OTHERS.
            ASSERT 0 = 1.
        ENDCASE.
      ENDIF.

      lv_obj_name = <ls_result>-src_incl.
      lv_code = <ls_result>-code.
      inform( p_sub_obj_name = lv_obj_name
              p_line         = <ls_result>-src_line
              p_kind         = lv_errty
              p_test         = myname
              p_code         = lv_code
              p_param_1      = lv_text ).
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    DATA lt_slin_desc_t     TYPE STANDARD TABLE OF slin_desc_t WITH EMPTY KEY.
    DATA ls_slin_desc_t     TYPE slin_desc_t.
    DATA lv_code            TYPE sci_errc.
    DATA lv_scimessage_text TYPE ty_scimessage_text.

    super->constructor( ).

    version  = '003'.
    position = '031'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_default_error = abap_true.

    SELECT * FROM slin_desc_t INTO TABLE lt_slin_desc_t
      WHERE language = sy-langu.
    IF sy-subrc <> 0.
      SELECT * FROM slin_desc_t INTO TABLE lt_slin_desc_t
        WHERE language = gc_english.
    ENDIF.

    LOOP AT lt_slin_desc_t INTO ls_slin_desc_t.
      lv_code            = ls_slin_desc_t-code_nr.
      lv_scimessage_text = ls_slin_desc_t-description.
      insert_scimessage(
        iv_code = lv_code
        iv_text = lv_scimessage_text ).
    ENDLOOP.

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT
      mt_error = mt_error
      mt_warn = mt_warn
      mt_info = mt_info
      mt_ignore = mt_ignore
      mv_default_error = mv_default_error
      mv_default_standard = mv_default_standard
      mv_default_atc = mv_default_atc
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD get_message_text.

    p_text = '&1'.                                          "#EC NOTEXT

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mt_error 'Error' 'S'.                    "#EC NOTEXT
    zzaoc_fill_att mt_warn 'Warning' 'S'.                   "#EC NOTEXT
    zzaoc_fill_att mt_info 'Info' 'S'.                      "#EC NOTEXT
    zzaoc_fill_att mt_ignore 'Ignore' 'S'.                  "#EC NOTEXT
    zzaoc_fill_att_rb mv_default_error 'Default error' 'R' 'RADIO'. "#EC NOTEXT
    zzaoc_fill_att_rb mv_default_standard 'Default standard' 'R' 'RADIO'. "#EC NOTEXT
    zzaoc_fill_att_rb mv_default_atc 'Default ATC' 'R' 'RADIO'. "#EC NOTEXT

    zzaoc_popup.
    attributes_ok = abap_true.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mt_error = mt_error
      mt_warn = mt_warn
      mt_info = mt_info
      mt_ignore = mt_ignore
      mv_default_error = mv_default_error
      mv_default_standard = mv_default_standard
      mv_default_atc = mv_default_atc
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD set_flags.

    rs_flags-x_per = abap_true.
    rs_flags-x_cal = abap_true.
    rs_flags-x_dat = abap_true.
    rs_flags-x_opf = abap_true.
    rs_flags-x_unr = abap_true.
    rs_flags-x_ges = abap_true.
    rs_flags-x_mes = abap_true.
    rs_flags-x_pfs = abap_true.
    rs_flags-x_bre = abap_true.
    rs_flags-x_woo = abap_true.
    rs_flags-x_wrn = abap_true.
    rs_flags-x_ste = abap_true.
    rs_flags-x_txt = abap_true.
    rs_flags-x_aut = abap_true.
    rs_flags-x_sub = abap_true.
    rs_flags-x_loa = abap_true.
    rs_flags-x_mls = abap_true.
    rs_flags-x_put = abap_true.
    rs_flags-x_hel = abap_true.

  ENDMETHOD.
ENDCLASS.
