CLASS zcl_aoc_check_72 DEFINITION
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



CLASS ZCL_AOC_CHECK_72 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    version  = '001'.
    position = '072'.

    has_documentation = abap_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    add_obj_type( 'TABL' ).

    enable_rfc( ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Enhancement category missing'(m01) ).

  ENDMETHOD.


  METHOD run.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_tabname     TYPE dd02l-tabname,
          ls_dd02v       TYPE dd02v,
          lv_destination TYPE rfcdest.


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
      EXCEPTIONS
        access_failure = 1
        OTHERS         = 2.
    IF sy-subrc <> 0 OR ls_dd02v IS INITIAL.
      RETURN.
    ENDIF.

    IF ls_dd02v-exclass = '0'.
      inform( p_test = myname
              p_kind = mv_errty
              p_code = '001' ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
