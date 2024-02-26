CLASS zcl_aoc_check_63 DEFINITION
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



CLASS zcl_aoc_check_63 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '063'.

    has_documentation = abap_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    add_obj_type( 'CLAS' ).
    add_obj_type( 'INTF' ).

    insert_scimessage(
        iv_code = '001'
        iv_text = TEXT-m01 ).

  ENDMETHOD.


  METHOD run.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lo_instance TYPE REF TO object,
          lr_data     TYPE REF TO data.

    FIELD-SYMBOLS: <lt_log>  TYPE STANDARD TABLE,
                   <lv_type> TYPE string,
                   <lv_text> TYPE string,
                   <lg_log>  TYPE any.


    IF object_type <> 'CLAS' AND object_type <> 'INTF'.
      RETURN.
    ENDIF.

    TRY.
        CREATE DATA lr_data TYPE ('CL_SEDI_ABAP_DOC_LOGGER=>TY_LOG_TAB').
        ASSIGN lr_data->* TO <lt_log>.
      CATCH cx_root ##CATCH_ALL.
        RETURN.
    ENDTRY.

    CALL METHOD ('CL_SEDI_ABAP_DOC_READ_ELEMINFO')=>('CREATE_INSTANCE')
      RECEIVING
        result = lo_instance.

    CASE object_type.
      WHEN 'INTF'.
        CALL METHOD lo_instance->('READ_EI_GLOBAL_INTERFACE')
          EXPORTING
            interface_name = object_name
          IMPORTING
            read_log       = <lt_log>.
      WHEN 'CLAS'.
        CALL METHOD lo_instance->('READ_EI_GLOBAL_CLASS')
          EXPORTING
            class_name = object_name
          IMPORTING
            read_log   = <lt_log>.
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

    LOOP AT <lt_log> ASSIGNING <lg_log>.
      ASSIGN COMPONENT 'TYPE' OF STRUCTURE <lg_log> TO <lv_type>.
      ASSERT sy-subrc = 0.
      IF <lv_type> = 'E'.
        ASSIGN COMPONENT 'TEXT' OF STRUCTURE <lg_log> TO <lv_text>.
        ASSERT sy-subrc = 0.

        IF <lv_text> CP 'active version cannot be read*'.
          CONTINUE.
        ENDIF.

        inform( p_sub_obj_type = object_type
                p_sub_obj_name = object_name
                p_kind         = mv_errty
                p_test         = myname
                p_code         = '001'
                p_param_1      = <lv_text> ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
