CLASS zcl_aoc_check_103 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC.

  PUBLIC SECTION.
    CONSTANTS:
      BEGIN OF gc_code,
        table_has_replacement_object TYPE sci_errc VALUE '001',
      END OF gc_code.

    METHODS constructor.
    METHODS check REDEFINITION.

  PRIVATE SECTION.
    CLASS-DATA mt_proxy_objects TYPE HASHED TABLE OF dd02v WITH UNIQUE KEY tabname.

    METHODS get_tokens_for_statement
      IMPORTING is_statement     TYPE sstmnt
                it_tokens        TYPE stokesx_tab
      RETURNING VALUE(rt_tokens) TYPE stokesx_tab.

    METHODS get_table_info
      IMPORTING VALUE(iv_tabname)   TYPE tabname
      RETURNING VALUE(r_table_info) TYPE dd02v.
ENDCLASS.


CLASS zcl_aoc_check_103 IMPLEMENTATION.
  METHOD constructor.
    super->constructor( ).

    version  = '001'.
    position = '103'.

    has_documentation = abap_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    insert_scimessage( iv_code = gc_code-table_has_replacement_object
                       iv_text = 'Table/View &1 has replacement object &2'(m01) ).
  ENDMETHOD.

  METHOD check.
    DATA lv_include          TYPE sobj_name.
    DATA lv_tabname          TYPE tabname.
    DATA lt_statement_tokens TYPE stokesx_tab.
    DATA ls_next             LIKE LINE OF lt_statement_tokens.

    FIELD-SYMBOLS <ls_statement> LIKE LINE OF io_scan->statements.
    FIELD-SYMBOLS <ls_token>     LIKE LINE OF io_scan->tokens.

    LOOP AT io_scan->statements ASSIGNING <ls_statement>.

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

      DATA ls_table_info TYPE dd02v.
      ls_table_info = get_table_info( lv_tabname ).

      IF ls_table_info-viewref IS NOT INITIAL AND ls_table_info-viewref <> space.
        lv_include = io_scan->get_include( <ls_statement>-level ).
        inform( p_sub_obj_name = lv_include
                p_line         = <ls_token>-row
                p_column       = <ls_token>-col
                p_kind         = mv_errty
                p_test         = myname
                p_param_1      = lv_tabname
                p_param_2      = ls_table_info-viewref
                p_code         = gc_code-table_has_replacement_object ).

      ENDIF.

    ENDLOOP.
  ENDMETHOD.

  METHOD get_tokens_for_statement.
    FIELD-SYMBOLS <ls_token> LIKE LINE OF it_tokens.

    LOOP AT it_tokens FROM is_statement-from TO is_statement-to ASSIGNING <ls_token>.
      APPEND <ls_token> TO rt_tokens.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_table_info.
    READ TABLE mt_proxy_objects WITH KEY tabname = iv_tabname INTO r_table_info.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    DATA lv_destination TYPE rfcdest.
    lv_destination = get_destination( ).
    CALL FUNCTION 'DD_TABL_GET'
      DESTINATION lv_destination
      EXPORTING  tabl_name             = iv_tabname
      IMPORTING  dd02v_wa_a            = r_table_info
      EXCEPTIONS access_failure        = 1
                 communication_failure = 2
                 system_failure        = 3
                 OTHERS                = 4.

    IF sy-subrc <> 0 OR r_table_info IS INITIAL.
      RETURN.
    ENDIF.
    INSERT r_table_info INTO TABLE mt_proxy_objects.
  ENDMETHOD.
ENDCLASS.
