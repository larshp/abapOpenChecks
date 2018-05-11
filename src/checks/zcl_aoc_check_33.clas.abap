CLASS zcl_aoc_check_33 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super_root
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS get_message_text
        REDEFINITION.
    METHODS run
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_33 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '033'.

    has_documentation = c_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

    add_obj_type( 'TABL' ).

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Field name &1 not in customer namespace'. "#EC NOTEXT
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

    DATA: lv_name  TYPE ddobjname,
          ls_dd02v TYPE dd02v,
          lt_dd03p TYPE TABLE OF dd03p.

    FIELD-SYMBOLS: <ls_dd03p> LIKE LINE OF lt_dd03p.


    IF object_type <> 'TABL'.
      RETURN.
    ENDIF.

    lv_name = object_name.
    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name      = lv_name
      IMPORTING
        dd02v_wa  = ls_dd02v
      TABLES
        dd03p_tab = lt_dd03p.

    IF ls_dd02v-tabclass <> 'APPEND'.
      RETURN.
    ENDIF.

* dont show error for appends in customer tables
    IF ls_dd02v-sqltab(1) = 'Z' OR ls_dd02v-sqltab(1) = 'Y'.
      RETURN.
    ENDIF.

    LOOP AT lt_dd03p ASSIGNING <ls_dd03p> WHERE fieldname <> '.INCLUDE'.
      IF <ls_dd03p>-fieldname(2) <> 'ZZ' AND <ls_dd03p>-fieldname(2) <> 'YY'.
        inform( p_sub_obj_type = object_type
                p_sub_obj_name = object_name
                p_test         = myname
                p_kind         = mv_errty
                p_code         = '001'
                p_param_1      = <ls_dd03p>-fieldname ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
