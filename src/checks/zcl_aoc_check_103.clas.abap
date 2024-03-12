class zcl_aoc_check_103 definition
  public
  inheriting from zcl_aoc_super
  create public.

  public section.
    constants:
      begin of gc_code,
        table_has_replacement_object type sci_errc value '001',
      end of gc_code.

    methods constructor.
    methods check redefinition.

  private section.
    class-data mt_proxy_objects type hashed table of dd02v with unique key tabname.

    methods get_tokens_for_statement
      importing is_statement     type sstmnt
                it_tokens        type stokesx_tab
      returning value(rt_tokens) type stokesx_tab.

    methods get_table_info
      importing value(iv_tabname)   type tabname
      returning value(r_table_info) type dd02v.
endclass.


class zcl_aoc_check_103 implementation.
  method constructor.
    super->constructor( ).

    version  = '001'.
    position = '103'.

    has_documentation = abap_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    insert_scimessage( iv_code = gc_code-table_has_replacement_object
                       iv_text = 'Table/View &1 has replacement object &2'(m01) ).
  endmethod.

  method check.
    data lv_include          type sobj_name.
    data lv_tabname          type tabname.
    data lt_statement_tokens type stokesx_tab.
    data ls_next             like line of lt_statement_tokens.

    field-symbols <ls_statement> like line of io_scan->statements.
    field-symbols <ls_token>     like line of io_scan->tokens.

    loop at io_scan->statements assigning <ls_statement>.

      read table io_scan->tokens assigning <ls_token> index <ls_statement>-from.
      if sy-subrc <> 0.
        continue.
      endif.

      if <ls_token>-str <> 'SELECT'.
        continue.
      endif.

      lt_statement_tokens = get_tokens_for_statement( is_statement = <ls_statement>
                                                      it_tokens    = io_scan->tokens ).

      read table lt_statement_tokens with key str = 'FROM' transporting no fields.
      if sy-subrc <> 0.
        continue.
      endif.
      read table lt_statement_tokens index sy-tabix + 1 into ls_next.
      if sy-subrc <> 0.
        continue.
      endif.
      lv_tabname = ls_next-str.

      data ls_table_info type dd02v.
      ls_table_info = get_table_info( lv_tabname ).

      if ls_table_info-viewref is not initial and ls_table_info-viewref <> space.
        lv_include = io_scan->get_include( <ls_statement>-level ).
        inform( p_sub_obj_name = lv_include
                p_line         = <ls_token>-row
                p_column       = <ls_token>-col
                p_kind         = mv_errty
                p_test         = myname
                p_param_1      = lv_tabname
                p_param_2      = ls_table_info-viewref
                p_code         = gc_code-table_has_replacement_object ).

      endif.

    endloop.
  endmethod.

  method get_tokens_for_statement.
    field-symbols <ls_token> like line of it_tokens.

    loop at it_tokens from is_statement-from to is_statement-to assigning <ls_token>.
      append <ls_token> to rt_tokens.
    endloop.
  endmethod.

  method get_table_info.
    read table mt_proxy_objects with key tabname = iv_tabname into r_table_info.
    if sy-subrc = 0.
      return.
    endif.

    data lv_destination type rfcdest.
    lv_destination = get_destination( ).
    call function 'DD_TABL_GET'
      destination lv_destination
      exporting
        tabl_name             = iv_tabname
      importing
        dd02v_wa_a            = r_table_info
      exceptions
        access_failure        = 1
        communication_failure = 2
        system_failure        = 3
        others                = 4.

    if sy-subrc <> 0 or r_table_info is initial.
      return.
    endif.
    insert r_table_info into table mt_proxy_objects.
  endmethod.
endclass.
