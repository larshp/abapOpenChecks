CLASS zcl_aoc_check_83 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super_root
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS run
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS used_in_ssfo
      RETURNING
        VALUE(rv_used) TYPE abap_bool .

    METHODS used_in_func_module_signature
      RETURNING
        VALUE(rv_used) TYPE abap_bool.
ENDCLASS.



CLASS zcl_aoc_check_83 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    version  = '001'.
    position = '083'.

    has_documentation = abap_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    add_obj_type( 'DOMA' ).
    add_obj_type( 'DTEL' ).
    add_obj_type( 'TABL' ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Unreferenced DDIC object'(m01) ).

  ENDMETHOD.


  METHOD run.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: ls_dd02l     TYPE dd02l,
          ls_dd03l     TYPE dd03l,
          ls_dd04l     TYPE dd04l,
          ls_dd25l     TYPE dd25l,
          ls_dd40l     TYPE dd40l,
          ls_edisdef   TYPE edisdef,
          ls_wbcrossgt TYPE wbcrossgt.

    CASE object_type.
      WHEN 'DOMA'.
        SELECT SINGLE * FROM dd04l INTO ls_dd04l WHERE domname = object_name.
        IF sy-subrc = 0.
          RETURN.
        ENDIF.

      WHEN 'DTEL'.
        SELECT SINGLE * FROM dd03l INTO ls_dd03l WHERE rollname = object_name.
        IF sy-subrc = 0.
          RETURN.
        ENDIF.

        IF used_in_func_module_signature( ) = abap_true.
          RETURN.
        ENDIF.
      WHEN 'TABL'.
        SELECT SINGLE * FROM dd02l INTO ls_dd02l WHERE tabname = object_name. "#EC CI_NOORDER
        IF ( sy-subrc = 0 AND ls_dd02l-tabclass = 'APPEND' ) OR sy-subrc <> 0.
          RETURN.
        ENDIF.

        SELECT SINGLE * FROM dd40l INTO ls_dd40l WHERE rowtype = object_name.
        IF sy-subrc = 0.
          RETURN.
        ENDIF.

        SELECT SINGLE * FROM dd03l INTO ls_dd03l
          WHERE ( fieldname = '.INCLUDE' AND precfield = object_name )
          OR ( rollname = object_name AND datatype = 'STRU' ).
        IF sy-subrc = 0.
          RETURN.
        ENDIF.

        SELECT SINGLE * FROM dd25l INTO ls_dd25l WHERE roottab = object_name. "#EC CI_NOFIRST
        IF sy-subrc = 0.
          RETURN.
        ENDIF.

        SELECT SINGLE * FROM edisdef INTO ls_edisdef WHERE segtyp = object_name.
        IF sy-subrc = 0.
          RETURN.
        ENDIF.

        IF used_in_func_module_signature( ) = abap_true.
          RETURN.
        ENDIF.
    ENDCASE.

    SELECT SINGLE * FROM wbcrossgt
      INTO ls_wbcrossgt
      WHERE name = object_name
      AND otype = 'TY'.
    IF sy-subrc = 0.
      RETURN.
    ENDIF.

    IF ( object_type = 'DTEL' OR object_type = 'TABL' )
        AND used_in_ssfo( ) = abap_true.
      RETURN.
    ENDIF.

    inform( p_test = myname
            p_kind = mv_errty
            p_code = '001' ).

  ENDMETHOD.


  METHOD used_in_ssfo.

    TYPES: BEGIN OF ty_tadir,
             obj_name TYPE tadir-obj_name,
           END OF ty_tadir.
    DATA lt_tadir  TYPE STANDARD TABLE OF ty_tadir WITH DEFAULT KEY.
    DATA lv_name   TYPE tdsfname.
    DATA lo_sf     TYPE REF TO cl_ssf_fb_smart_form.
    FIELD-SYMBOLS <ls_data> LIKE LINE OF lo_sf->fsymbols.

* looks like SSFO are not included in where-used lists, as the data is stored
* in XML. Loop through and check all,
    SELECT obj_name FROM tadir INTO TABLE lt_tadir
      WHERE pgmid = 'R3TR'
      AND object = 'SSFO'
      AND srcsystem = sy-sysid
      AND devclass <> '$TMP'.
    LOOP AT lt_tadir INTO DATA(ls_tadir).
      CREATE OBJECT lo_sf.
      lv_name = ls_tadir-obj_name.
      TRY.
          lo_sf->load( lv_name ).
        CATCH cx_ssf_fb.
          CONTINUE.
      ENDTRY.

      LOOP AT lo_sf->fsymbols ASSIGNING <ls_data>.
        IF <ls_data>-typename = object_name.
          rv_used = abap_true.
          RETURN.
        ENDIF.
      ENDLOOP.

      LOOP AT lo_sf->gdata ASSIGNING <ls_data>.
        IF <ls_data>-typename = object_name.
          rv_used = abap_true.
          RETURN.
        ENDIF.
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.

  METHOD used_in_func_module_signature.
    DATA lv_dummy TYPE fupararef-funcname.

    SELECT SINGLE funcname FROM fupararef INTO lv_dummy WHERE structure = object_name.

    IF sy-subrc = 0.
      rv_used = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
