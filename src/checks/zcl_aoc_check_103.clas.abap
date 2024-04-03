CLASS zcl_aoc_check_103 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC.

  PUBLIC SECTION.

    CONSTANTS:
      BEGIN OF gc_code,
        table_has_replacement_object   TYPE sci_errc VALUE '001',
        table_has_replacement_proposal TYPE sci_errc VALUE '002',
      END OF gc_code.
    CONSTANTS gc_pseudo_comment TYPE sci_pcom VALUE 'AOC_103_SELECT_OK' ##NO_TEXT.
    TYPES: BEGIN OF ty_replace_proposal,
             from     TYPE tabname,
             sequence TYPE n LENGTH 1,
             to       TYPE tabname,
             oss_note TYPE c LENGTH 10,
           END OF ty_replace_proposal,
           tty_proposals TYPE SORTED TABLE OF ty_replace_proposal WITH UNIQUE KEY from sequence.
    TYPES: BEGIN OF ty_message_detail,
             tabname            TYPE tabname,
             replacement_object TYPE tabname,
             proposed_table     TYPE tabname,
             message_code       TYPE sci_errc,
             oss_note           TYPE c LENGTH 10,
           END OF ty_message_detail.
    CLASS-DATA gt_replace_proposals TYPE tty_proposals READ-ONLY.
    METHODS constructor.
    METHODS check REDEFINITION.
    CLASS-METHODS class_constructor.

  PRIVATE SECTION.

    CLASS-DATA gt_proxy_objects TYPE HASHED TABLE OF dd02v WITH UNIQUE KEY tabname.
    METHODS get_tokens_for_statement
      IMPORTING is_statement     TYPE sstmnt
                it_tokens        TYPE stokesx_tab
      RETURNING VALUE(rt_tokens) TYPE stokesx_tab.

    METHODS get_table_info
      IMPORTING iv_tabname           TYPE tabname
      RETURNING VALUE(rs_table_info) TYPE dd02v.
    METHODS get_message_detail
      IMPORTING
        iv_tabname               TYPE tabname
        iv_replacement_object    TYPE dd02v-viewref
      RETURNING
        VALUE(rs_message_detail) TYPE ty_message_detail.
    METHODS check_table_as_used
      IMPORTING
        it_statement_tokens     TYPE stokesx_tab
        iv_tabix                TYPE sy-tabix
      RETURNING
        VALUE(rv_table_as_used) TYPE abap_bool.
ENDCLASS.


CLASS zcl_aoc_check_103 IMPLEMENTATION.
  METHOD constructor.
    DATA: lv_message_text TYPE zcl_aoc_super=>ty_scimessage_text.
    super->constructor( ).

    version  = '001'.
    position = '103'.

    has_documentation = abap_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).



    insert_scimessage( iv_code = gc_code-table_has_replacement_object
                       iv_text = 'Table/View &1 has replacement object &2'(m01)
                       iv_pcom = gc_pseudo_comment ).

    insert_scimessage( iv_code = gc_code-table_has_replacement_proposal
                       iv_text = 'Table/View &1 has replacement object &2. Consider using &3 instead. OSS Note &4'(m02)
                       iv_pcom = gc_pseudo_comment ).


  ENDMETHOD.

  METHOD check.
    DATA lv_include          TYPE sobj_name.
    DATA lv_tabname          TYPE tabname.
    DATA lv_table_as_used    TYPE abap_bool.
    DATA lt_statement_tokens TYPE stokesx_tab.
    DATA ls_next             TYPE stokesx.
    DATA ls_table_info TYPE dd02v.
    DATA lv_position TYPE int4.
    DATA lv_detail TYPE xstring.
    DATA lv_message_detail TYPE ty_message_detail.
    DATA lt_source TYPE string_table.
    FIELD-SYMBOLS <ls_statement> LIKE LINE OF io_scan->statements.
    FIELD-SYMBOLS <ls_token>     LIKE LINE OF io_scan->tokens.
    FIELD-SYMBOLS <ls_level> TYPE slevel.

    LOOP AT io_scan->statements ASSIGNING <ls_statement>.
      lv_position = sy-tabix.
      READ TABLE io_scan->tokens ASSIGNING <ls_token> INDEX <ls_statement>-from.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF <ls_token>-str <> 'SELECT'.
        CONTINUE.
      ENDIF.

      lt_statement_tokens = get_tokens_for_statement( is_statement = <ls_statement>
                                                      it_tokens    = io_scan->tokens ).

      READ TABLE lt_statement_tokens WITH KEY str = 'FROM' TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.
      READ TABLE lt_statement_tokens INDEX sy-tabix + 1 INTO ls_next.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      lv_tabname = ls_next-str.

      lv_table_as_used = check_table_as_used( it_statement_tokens = lt_statement_tokens
                                              iv_tabix            = sy-tabix + 1 ).

      ls_table_info = get_table_info( lv_tabname ).



      IF ls_table_info-viewref IS NOT INITIAL AND ls_table_info-viewref <> space.
        lv_include = io_scan->get_include( <ls_statement>-level ).

        READ TABLE io_scan->levels ASSIGNING <ls_level> WITH KEY name = lv_include.
        IF sy-subrc = 0.
          lt_source = get_source( <ls_level> ).
        ENDIF.

        lv_detail = lcl_quickfix=>get_quick_fixes( iv_current_tab_name = lv_tabname
                                                   iv_new_tab_name     = ls_table_info-viewref
                                                   iv_include          = lv_include
                                                   iv_col              = ls_next-col
                                                   iv_source           = lt_source
                                                   iv_line             = ls_next-row
                                                   iv_table_as_used    = lv_table_as_used ).

        lv_message_detail = get_message_detail( iv_tabname            = lv_tabname
                                                iv_replacement_object = ls_table_info-viewref ).
        inform( p_sub_obj_name = lv_include
                p_line         = <ls_token>-row
                p_column       = <ls_token>-col
                p_position     = lv_position
                p_kind         = mv_errty
                p_test         = myname
                p_param_1      = lv_message_detail-tabname
                p_param_2      = lv_message_detail-replacement_object
                p_param_3      = lv_message_detail-proposed_table
                p_param_4      = lv_message_detail-oss_note
                p_code         = lv_message_detail-message_code
                p_suppress     = gc_pseudo_comment
                p_detail       = lv_detail ).

      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD check_table_as_used.
    DATA ls_next             TYPE stokesx.

    READ TABLE it_statement_tokens INDEX iv_tabix INTO ls_next.
    IF sy-subrc = 0 AND ls_next-str = 'AS'.
      READ TABLE it_statement_tokens INDEX iv_tabix + 1 INTO ls_next.
      IF sy-subrc = 0.
        rv_table_as_used = abap_true.
      ENDIF.
    ENDIF.

  ENDMETHOD.



  METHOD get_tokens_for_statement.
    FIELD-SYMBOLS <ls_token> LIKE LINE OF it_tokens.

    LOOP AT it_tokens FROM is_statement-from TO is_statement-to ASSIGNING <ls_token>.
      APPEND <ls_token> TO rt_tokens.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_table_info.
    DATA lv_destination TYPE rfcdest.
    READ TABLE gt_proxy_objects WITH KEY tabname = iv_tabname INTO rs_table_info.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.


    lv_destination = get_destination( ).
    CALL FUNCTION 'DD_TABL_GET'
      DESTINATION lv_destination
      EXPORTING
        tabl_name             = iv_tabname
      IMPORTING
        dd02v_wa_a            = rs_table_info
      EXCEPTIONS
        access_failure        = 1
        communication_failure = 2
        system_failure        = 3
        OTHERS                = 4.

    IF sy-subrc <> 0 OR rs_table_info IS INITIAL.
      RETURN.
    ENDIF.
    INSERT rs_table_info INTO TABLE gt_proxy_objects.
  ENDMETHOD.

  METHOD class_constructor.
    gt_replace_proposals = lcl_quickfix=>build_proposals_list( ).
  ENDMETHOD.


  METHOD get_message_detail.
    DATA lv_proposals TYPE ty_replace_proposal.

    READ TABLE gt_replace_proposals INTO lv_proposals WITH KEY from = iv_tabname sequence = 1.
    IF sy-subrc = 0.
      rs_message_detail-message_code = gc_code-table_has_replacement_proposal.
      rs_message_detail-proposed_table = lv_proposals-to.
      rs_message_detail-oss_note = lv_proposals-oss_note.
    ELSE.
      rs_message_detail-message_code = gc_code-table_has_replacement_object.
    ENDIF.
    rs_message_detail-tabname = iv_tabname.
    rs_message_detail-replacement_object = iv_replacement_object.

  ENDMETHOD.

ENDCLASS.
