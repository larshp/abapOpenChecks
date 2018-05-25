class ZCL_AOC_CHECK_71 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.

  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_ATTRIBUTES
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
  methods IF_CI_TEST~QUERY_ATTRIBUTES
    redefinition .
  methods PUT_ATTRIBUTES
    redefinition .
  protected section.

    data mv_unreachable type sap_bool.
  private section.
ENDCLASS.



CLASS ZCL_AOC_CHECK_71 IMPLEMENTATION.


  method check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    data: lt_statements type ty_statements,
          lv_index      type i,
          lv_code       type sci_errc,
          ls_prev       like line of lt_statements.

    field-symbols: <ls_statement> like line of lt_statements.

    lt_statements = build_statements(
      it_tokens     = it_tokens
      it_statements = it_statements
      it_levels     = it_levels ).

    loop at lt_statements assigning <ls_statement>.
      lv_index = sy-tabix - 1.
      clear ls_prev.
      read table lt_statements index lv_index into ls_prev. "#EC CI_SUBRC

      find regex 'MESSAGE.ID.SY-MSGID.TYPE.SY-MSGTY.NUMBER.SY-MSGNO.WITH.SY-MSGV.' in <ls_statement>-str.
      if sy-subrc ne 0.
        continue.
      endif.

      if <ls_statement>-str cp 'MESSAGE *'
          and ( ( mv_unreachable = abap_true and ls_prev-str = 'IF 1 = 2' )
          or ( mv_unreachable = abap_true and ls_prev-str = 'IF 0 = 1' ) ).
        continue.
      endif.

      if <ls_statement>-str cp 'MESSAGE *'.
        lv_code = '001'.
      else.
        assert 0 = 1.
      endif.

      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = <ls_statement>-include
              p_line         = <ls_statement>-start-row
              p_kind         = mv_errty
              p_test         = myname
              p_code         = lv_code ).
    endloop.

  endmethod.


  method constructor.

    super->constructor( ).

    description = 'MESSAGE using sytem variables SY-MSGTY, SY-MSGNO, etc'. "#EC NOTEXT
    category    = 'ZCL_AOC_CATEGORY'.
    version     = '001'.
    position    = '071'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    enable_rfc( ).

    mv_errty   = c_error.
    mv_unreachable = abap_true.

  endmethod.                    "CONSTRUCTOR


  method get_attributes.

    export
      mv_errty = mv_errty
      mv_unreachable = mv_unreachable
      to data buffer p_attributes.

  endmethod.


  method get_message_text.

    clear p_text.

    case p_code.
      when '001'.
        p_text = 'MESSAGE using standard variables from SY structure'.                 "#EC NOTEXT
      when others.
        super->get_message_text( exporting p_test = p_test
                                           p_code = p_code
                                 importing p_text = p_text ).
    endcase.

  endmethod.


  method if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_unreachable 'Allow unreachable' 'C'.  "#EC NOTEXT

    zzaoc_popup.

  endmethod.


  method put_attributes.

    import
      mv_errty = mv_errty
      mv_unreachable = mv_unreachable
      from data buffer p_attributes.                 "#EC CI_USE_WANTED
    assert sy-subrc = 0.

  endmethod.
ENDCLASS.
