CLASS zcl_aoc_check_67 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super_root
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS run
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_67 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    version     = '001'.
    position    = '067'.

    has_documentation = abap_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    add_obj_type( 'DDLS' ).

    insert_scimessage(
        iv_code = '001'
        iv_text = '&1'(m01) ).

  ENDMETHOD.


  METHOD run.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lo_handler TYPE REF TO object,
          lx_check   TYPE REF TO cx_static_check,
          lr_data    TYPE REF TO data,
          lv_str     TYPE string.

    FIELD-SYMBOLS: <lg_error>  TYPE any,
                   <lv_arbgb>  TYPE arbgb,
                   <lv_msgnr>  TYPE msgnr,
                   <lv_var1>   TYPE char50,
                   <lv_var2>   TYPE char50,
                   <lv_var3>   TYPE char50,
                   <lv_var4>   TYPE char50,
                   <lt_errors> TYPE STANDARD TABLE.


    IF object_type <> 'DDLS'.
      RETURN.
    ENDIF.

    TRY.
        CALL METHOD ('CL_DD_DDL_HANDLER_FACTORY')=>('CREATE')
          RECEIVING
            handler = lo_handler.

        CALL METHOD lo_handler->('IF_DD_DDL_HANDLER~CHECK')
          EXPORTING
            name = object_name.
      CATCH cx_static_check INTO lx_check. " CX_DD_DDL_CHECK

        CREATE DATA lr_data TYPE ('DDL2DDICERRORS').
        ASSIGN lr_data->* TO <lt_errors>.

        CALL METHOD lx_check->('GET_ERRORS')
          RECEIVING
            errors = <lt_errors>.

        LOOP AT <lt_errors> ASSIGNING <lg_error>.
          ASSIGN COMPONENT 'ARBGB' OF STRUCTURE <lg_error> TO <lv_arbgb>. "#EC CI_SUBRC
          ASSIGN COMPONENT 'MSGNR' OF STRUCTURE <lg_error> TO <lv_msgnr>. "#EC CI_SUBRC
          ASSIGN COMPONENT 'VAR1' OF STRUCTURE <lg_error> TO <lv_var1>. "#EC CI_SUBRC
          ASSIGN COMPONENT 'VAR2' OF STRUCTURE <lg_error> TO <lv_var2>. "#EC CI_SUBRC
          ASSIGN COMPONENT 'VAR3' OF STRUCTURE <lg_error> TO <lv_var3>. "#EC CI_SUBRC
          ASSIGN COMPONENT 'VAR4' OF STRUCTURE <lg_error> TO <lv_var4>. "#EC CI_SUBRC

          MESSAGE ID <lv_arbgb> TYPE 'E'
            NUMBER <lv_msgnr> WITH <lv_var1> <lv_var2> <lv_var3> <lv_var4>
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
