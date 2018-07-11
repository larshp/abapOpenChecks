CLASS zcl_aoc_dependencies DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ty_object,
        obj_type TYPE tadir-object,
        obj_name TYPE tadir-obj_name,
        package  TYPE devclass,
      END OF ty_object .
    TYPES:
      ty_objects_tt TYPE STANDARD TABLE OF ty_object WITH DEFAULT KEY .

    CLASS-METHODS resolve
      IMPORTING
        !iv_obj_type   TYPE tadir-object
        !iv_obj_name   TYPE tadir-obj_name
      RETURNING
        VALUE(rt_used) TYPE ty_objects_tt .
  PROTECTED SECTION.

    CLASS-METHODS resolve_clas
      IMPORTING
        !iv_name       TYPE tadir-obj_name
      RETURNING
        VALUE(rt_used) TYPE ty_objects_tt .
    CLASS-METHODS resolve_ddls
      IMPORTING
        !iv_name       TYPE tadir-obj_name
      RETURNING
        VALUE(rt_used) TYPE ty_objects_tt .
    CLASS-METHODS resolve_doma
      IMPORTING
        !iv_name       TYPE tadir-obj_name
      RETURNING
        VALUE(rt_used) TYPE ty_objects_tt .
    CLASS-METHODS resolve_dtel
      IMPORTING
        !iv_name       TYPE tadir-obj_name
      RETURNING
        VALUE(rt_used) TYPE ty_objects_tt .
    CLASS-METHODS resolve_fugr
      IMPORTING
        !iv_name       TYPE tadir-obj_name
      RETURNING
        VALUE(rt_used) TYPE ty_objects_tt .
    CLASS-METHODS resolve_intf
      IMPORTING
        !iv_name       TYPE tadir-obj_name
      RETURNING
        VALUE(rt_used) TYPE ty_objects_tt .
    CLASS-METHODS resolve_msag
      IMPORTING
        !iv_name       TYPE tadir-obj_name
      RETURNING
        VALUE(rt_used) TYPE ty_objects_tt .
    CLASS-METHODS resolve_prog
      IMPORTING
        !iv_name       TYPE tadir-obj_name
      RETURNING
        VALUE(rt_used) TYPE ty_objects_tt .
    CLASS-METHODS resolve_tabl
      IMPORTING
        !iv_name       TYPE tadir-obj_name
      RETURNING
        VALUE(rt_used) TYPE ty_objects_tt .
    CLASS-METHODS resolve_tran
      IMPORTING
        !iv_name       TYPE tadir-obj_name
      RETURNING
        VALUE(rt_used) TYPE ty_objects_tt .
    CLASS-METHODS resolve_ttyp
      IMPORTING
        !iv_name       TYPE tadir-obj_name
      RETURNING
        VALUE(rt_used) TYPE ty_objects_tt .
    CLASS-METHODS resolve_view
      IMPORTING
        !iv_name       TYPE tadir-obj_name
      RETURNING
        VALUE(rt_used) TYPE ty_objects_tt .
    CLASS-METHODS amend_packages
      CHANGING
        !ct_used TYPE ty_objects_tt .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_DEPENDENCIES IMPLEMENTATION.


  METHOD amend_packages.

    SORT ct_used BY obj_type ASCENDING obj_name ASCENDING.
    DELETE ADJACENT DUPLICATES FROM ct_used COMPARING obj_type obj_name.

    IF lines( ct_used ) > 0.
* todo, better handling of custom vs SAP standard packages
      SELECT object AS obj_type obj_name devclass AS package
        FROM tadir INTO TABLE ct_used
        FOR ALL ENTRIES IN ct_used
        WHERE pgmid = 'R3TR'
        AND object = ct_used-obj_type
        AND obj_name = ct_used-obj_name
        AND ( devclass LIKE 'Y%' OR devclass LIKE 'Z%' ).
    ENDIF.

  ENDMETHOD.


  METHOD resolve.

* this is a bit stupid, but it will work :o)
    CASE iv_obj_type.
      WHEN 'TRAN'.
        rt_used = resolve_tran( iv_obj_name ).
      WHEN 'CLAS'.
        rt_used = resolve_clas( iv_obj_name ).
      WHEN 'PROG'.
        rt_used = resolve_prog( iv_obj_name ).
      WHEN 'INTF'.
        rt_used = resolve_intf( iv_obj_name ).
      WHEN 'MSAG'.
        rt_used = resolve_msag( iv_obj_name ).
      WHEN 'DDLS'.
        rt_used = resolve_ddls( iv_obj_name ).
      WHEN 'DOMA'.
        rt_used = resolve_doma( iv_obj_name ).
      WHEN 'DTEL'.
        rt_used = resolve_dtel( iv_obj_name ).
      WHEN 'TABL'.
        rt_used = resolve_tabl( iv_obj_name ).
      WHEN 'FUGR'.
        rt_used = resolve_fugr( iv_obj_name ).
      WHEN 'TTYP'.
        rt_used = resolve_ttyp( iv_obj_name ).
      WHEN 'VIEW'.
        rt_used = resolve_view( iv_obj_name ).
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

    amend_packages( CHANGING ct_used = rt_used ).

  ENDMETHOD.


  METHOD resolve_clas.

* todo

  ENDMETHOD.


  METHOD resolve_ddls.

* todo

  ENDMETHOD.


  METHOD resolve_doma.

* todo

  ENDMETHOD.


  METHOD resolve_dtel.

* todo

  ENDMETHOD.


  METHOD resolve_fugr.

* todo

  ENDMETHOD.


  METHOD resolve_intf.

* todo

  ENDMETHOD.


  METHOD resolve_msag.

* Message classes does not use any other objects
    RETURN.

  ENDMETHOD.


  METHOD resolve_prog.

* todo

  ENDMETHOD.


  METHOD resolve_tabl.

    TYPES: BEGIN OF ty_dd03l,
             domname  TYPE dd03l-domname,
             rollname TYPE dd03l-rollname,
             comptype TYPE dd03l-comptype,
           END OF ty_dd03l.

    DATA: lt_dd03l TYPE STANDARD TABLE OF ty_dd03l WITH DEFAULT KEY,
          ls_used  LIKE LINE OF rt_used,
          ls_dd03l LIKE LINE OF lt_dd03l.


    SELECT domname rollname comptype
      FROM dd03l INTO TABLE lt_dd03l
      WHERE tabname = iv_name.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    LOOP AT lt_dd03l INTO ls_dd03l.
      CASE ls_dd03l-comptype.
        WHEN 'S'.
          IF NOT ls_dd03l-rollname IS INITIAL.
            CLEAR ls_used.
            ls_used-obj_type = 'DTEL'.
            ls_used-obj_name = ls_dd03l-rollname.
            APPEND ls_used TO rt_used.
            CONTINUE.
          ENDIF.
        WHEN 'T'.
          CLEAR ls_used.
          ls_used-obj_type = 'TTYP'.
          ls_used-obj_name = ls_dd03l-rollname.
          APPEND ls_used TO rt_used.
          CONTINUE.
      ENDCASE.

      IF NOT ls_dd03l-domname IS INITIAL.
        CLEAR ls_used.
        ls_used-obj_type = 'DOMA'.
        ls_used-obj_name = ls_dd03l-domname.
        APPEND ls_used TO rt_used.
      ENDIF.

      IF NOT ls_dd03l-rollname IS INITIAL.
        CLEAR ls_used.
        ls_used-obj_type = 'DTEL'.
        ls_used-obj_name = ls_dd03l-domname.
        APPEND ls_used TO rt_used.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD resolve_tran.

* todo

  ENDMETHOD.


  METHOD resolve_ttyp.

* todo

  ENDMETHOD.


  METHOD resolve_view.

* todo

  ENDMETHOD.
ENDCLASS.
