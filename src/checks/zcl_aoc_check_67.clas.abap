class ZCL_AOC_CHECK_67 definition
  public
  inheriting from ZCL_AOC_SUPER_ROOT
  create public .

public section.

  methods CONSTRUCTOR .

  methods GET_MESSAGE_TEXT
    redefinition .
  methods RUN
    redefinition .
  PROTECTED SECTION.

    DATA ms_settings TYPE sci_s_naming_conventions_setup .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_67 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    description = 'CDS syntax check'.                       "#EC NOTEXT
    category    = 'ZCL_AOC_CATEGORY'.
    version     = '001'.
    position    = '067'.

    has_documentation = c_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

    add_obj_type( 'DDLS' ).

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

    DATA: lt_warnings TYPE ddl2ddicwarnings,
          lv_str      TYPE string.


    IF object_type <> 'DDLS'.
      RETURN.
    ENDIF.

    TRY.
* TODO, downport
        cl_dd_ddl_handler_factory=>create( )->check( object_name ).
      CATCH cx_dd_ddl_check INTO DATA(lx_check).
        BREAK dk8lahpe.
        LOOP AT lx_check->get_errors( ) INTO DATA(ls_error).
          MESSAGE ID ls_error-arbgb TYPE 'E'
            NUMBER ls_error-msgnr WITH ls_error-var1 ls_error-var2 ls_error-var3 ls_error-var4
            INTO lv_str.

          inform( p_sub_obj_type = object_type
                  p_sub_obj_name = object_name
                  p_test         = myname
                  p_kind         = mv_errty
                  p_code         = '001'
                  p_param_1      = lv_str ).
        ENDLOOP.
    ENDTRY.

  ENDMETHOD.
ENDCLASS.
