CLASS zcl_aoc_check_33 DEFINITION
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



CLASS zcl_aoc_check_33 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    version  = '001'.
    position = '033'.

    has_documentation = abap_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    add_obj_type( 'TABL' ).

    enable_rfc( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Field name &1 not in customer namespace'(m01) ).

  ENDMETHOD.


  METHOD run.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_tabname     TYPE dd02l-tabname,
          ls_dd02v       TYPE dd02v,
          lv_destination TYPE rfcdest,
          lt_dd03p       TYPE TABLE OF dd03p,
          lv_sobj_name   TYPE sobj_name.

    FIELD-SYMBOLS: <ls_dd03p> LIKE LINE OF lt_dd03p.


    IF object_type <> 'TABL'.
      RETURN.
    ENDIF.

    lv_tabname = object_name.

    lv_destination = get_destination( ).

    CALL FUNCTION 'DD_TABL_GET'
      DESTINATION lv_destination
      EXPORTING
        tabl_name      = lv_tabname
      IMPORTING
        dd02v_wa_a     = ls_dd02v
      TABLES
        dd03p_tab_a    = lt_dd03p
      EXCEPTIONS
        access_failure = 1
        OTHERS         = 2.
    IF sy-subrc <> 0 OR ls_dd02v-tabclass <> 'APPEND'.
      RETURN.
    ENDIF.

* dont show error for appends in customer tables
    lv_sobj_name = ls_dd02v-sqltab.
    IF lv_sobj_name(1) = 'Z' OR
        lv_sobj_name(1) = 'Y' OR
        zcl_aoc_util_reg_atc_namespace=>is_append_obj_in_ns( lv_sobj_name ) = abap_true.
      RETURN.
    ENDIF.

    LOOP AT lt_dd03p ASSIGNING <ls_dd03p> WHERE fieldname <> '.INCLUDE'.
      lv_sobj_name = <ls_dd03p>-fieldname.
      IF lv_sobj_name(2) <> 'ZZ' AND
          lv_sobj_name(2) <> 'YY' AND
          zcl_aoc_util_reg_atc_namespace=>is_append_obj_in_ns( lv_sobj_name ) = abap_false.
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
