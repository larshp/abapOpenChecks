class ZCL_AOC_CHECK_65 definition
  public
  inheriting from ZCL_AOC_SUPER_ROOT
  create public .

public section.

  methods CONSTRUCTOR .

  methods GET_MESSAGE_TEXT
    redefinition .
  methods RUN
    redefinition .
protected section.

  data MS_SETTINGS type SCI_S_NAMING_CONVENTIONS_SETUP .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_65 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    description = 'Smartforms syntax check'.                "#EC NOTEXT
    category    = 'ZCL_AOC_CATEGORY'.
    version     = '001'.
    position    = '065'.

    has_documentation = c_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

    add_obj_type( 'SSFO' ).

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = '&1'.                                      "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.


  METHOD run.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    CONSTANTS: lc_text_module TYPE tdsftype VALUE 'T'.

    DATA: lv_name  TYPE tdsfname,
          lx_check TYPE REF TO cx_ssf_fb_check,
          ls_error TYPE ssfmsgerr,
          lo_sf    TYPE REF TO cl_ssf_fb_smart_form.

    FIELD-SYMBOLS: <ls_data> LIKE LINE OF lo_sf->fsymbols.


    IF object_type <> 'SSFO'.
      RETURN.
    ENDIF.

    CREATE OBJECT lo_sf.
    lv_name = object_name.
    lo_sf->load( lv_name ).

    IF lo_sf->header-formtype = lc_text_module.
      RETURN.
    ENDIF.

    TRY.
        lo_sf->check( abap_true ).
      CATCH cx_ssf_fb_check INTO lx_check.
        LOOP AT lx_check->error_table INTO ls_error WHERE class = 'E'.
          inform( p_sub_obj_type = object_type
                  p_sub_obj_name = object_name
                  p_test         = myname
                  p_kind         = mv_errty
                  p_code         = '001'
                  p_param_1      = ls_error-msg ).
        ENDLOOP.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
