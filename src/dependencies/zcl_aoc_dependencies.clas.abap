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

    CLASS-METHODS class_or_interface
      IMPORTING
        !iv_name       TYPE csequence
      RETURNING
        VALUE(rv_type) TYPE tadir-object .
    CLASS-METHODS find_fugr
      IMPORTING
        !iv_name       TYPE csequence
      RETURNING
        VALUE(rv_name) TYPE seu_name .
    CLASS-METHODS find_type
      IMPORTING
        !iv_name       TYPE csequence
      RETURNING
        VALUE(rv_type) TYPE tadir-object .
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
    CLASS-METHODS resolve_prog
      IMPORTING
        !iv_name       TYPE tadir-obj_name
      RETURNING
        VALUE(rt_used) TYPE ty_objects_tt .
    CLASS-METHODS resolve_prog_via_cross
      IMPORTING
        !iv_name       TYPE tadir-obj_name
      RETURNING
        VALUE(rt_used) TYPE ty_objects_tt .
    CLASS-METHODS resolve_prog_via_wbcrossgt
      IMPORTING
        !iv_name       TYPE tadir-obj_name
      RETURNING
        VALUE(rt_used) TYPE ty_objects_tt .
    CLASS-METHODS resolve_prog_via_wbcrossi
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



CLASS zcl_aoc_dependencies IMPLEMENTATION.


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
        AND ( devclass LIKE 'Y%'
        OR devclass LIKE 'Z%' ).                          "#EC CI_SUBRC
    ENDIF.

  ENDMETHOD.


  METHOD class_or_interface.

    DATA: lv_clstype TYPE seoclass-clstype.

    SELECT SINGLE clstype FROM seoclass INTO lv_clstype WHERE clsname = iv_name.
    IF sy-subrc = 0.
      CASE lv_clstype.
        WHEN '0'.
          rv_type = 'CLAS'.
        WHEN '1'.
          rv_type = 'INTF'.
        WHEN OTHERS.
          ASSERT 0 = 1.
      ENDCASE.
    ENDIF.

  ENDMETHOD.


  METHOD find_fugr.

    DATA: ls_tfdir TYPE tfdir.

    SELECT SINGLE * FROM tfdir INTO ls_tfdir WHERE funcname = iv_name.
    IF sy-subrc = 0.
      rv_name = ls_tfdir-pname_main+4.
    ENDIF.

  ENDMETHOD.


  METHOD find_type.

    DATA: lv_clstype TYPE seoclass-clstype.


    SELECT COUNT(*) FROM dd04l WHERE rollname = iv_name.
    IF sy-subrc = 0.
      rv_type = 'DTEL'.
      RETURN.
    ENDIF.

    SELECT COUNT(*) FROM dd02l WHERE tabname = iv_name.
    IF sy-subrc = 0.
      rv_type = 'TABL'.
      RETURN.
    ENDIF.

    SELECT COUNT(*) FROM dd40l WHERE typename = iv_name.
    IF sy-subrc = 0.
      rv_type = 'TTYP'.
      RETURN.
    ENDIF.

    SELECT SINGLE clstype FROM seoclass INTO lv_clstype WHERE clsname = iv_name.
    IF sy-subrc = 0.
      CASE lv_clstype.
        WHEN '0'.
          rv_type = 'CLAS'.
          RETURN.
        WHEN '1'.
          rv_type = 'INTF'.
          RETURN.
        WHEN OTHERS.
          ASSERT 0 = 1.
      ENDCASE.
    ENDIF.

    ASSERT 0 = 1.

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

    DATA: lt_includes TYPE seoincl_t,
          lv_name     TYPE seoclsname.

    FIELD-SYMBOLS: <lv_include> LIKE LINE OF lt_includes.

* todo, this method does not exist on 702
    lv_name = iv_name.
    lt_includes = cl_oo_classname_service=>get_all_class_includes( lv_name ).
    LOOP AT lt_includes ASSIGNING <lv_include>.
      APPEND LINES OF resolve_prog( <lv_include> ) TO rt_used.
    ENDLOOP.

    DELETE rt_used WHERE obj_name CS iv_name.

  ENDMETHOD.


  METHOD resolve_ddls.

* todo

  ENDMETHOD.


  METHOD resolve_doma.

    DATA: lv_entitytab TYPE tadir-obj_name,
          ls_used      LIKE LINE OF rt_used.


    SELECT SINGLE entitytab FROM dd01l INTO lv_entitytab
      WHERE domname = iv_name
      AND as4local = 'A'
      AND as4vers = '0000'.                               "#EC CI_SUBRC
    IF NOT lv_entitytab IS INITIAL.
      ls_used-obj_type = find_type( lv_entitytab ).
      ls_used-obj_name = lv_entitytab.
      APPEND ls_used TO rt_used.
    ENDIF.

  ENDMETHOD.


  METHOD resolve_dtel.

    DATA: lv_domname TYPE dd04l-domname,
          ls_used    LIKE LINE OF rt_used.


    SELECT SINGLE domname FROM dd04l INTO lv_domname
      WHERE rollname = iv_name.                         "#EC CI_NOORDER
    IF sy-subrc = 0 AND NOT lv_domname IS INITIAL.
      ls_used-obj_type = 'DOMA'.
      ls_used-obj_name = lv_domname.
      APPEND ls_used TO rt_used.
    ENDIF.

  ENDMETHOD.


  METHOD resolve_fugr.

    DATA: lv_program  TYPE program,
          lt_includes TYPE rso_t_objnm,
          lv_include  LIKE LINE OF lt_includes.


    lv_program = iv_name.

    CALL FUNCTION 'RS_GET_ALL_INCLUDES'
      EXPORTING
        program      = lv_program
      TABLES
        includetab   = lt_includes
      EXCEPTIONS
        not_existent = 1
        no_program   = 2
        OTHERS       = 3. "#EC CI_SUBRC

    LOOP AT lt_includes INTO lv_include.
      APPEND LINES OF resolve_prog( lv_include ) TO rt_used.
    ENDLOOP.

  ENDMETHOD.


  METHOD resolve_intf.

* todo

  ENDMETHOD.


  METHOD resolve_prog.

    APPEND LINES OF resolve_prog_via_wbcrossi( iv_name ) TO rt_used.
    APPEND LINES OF resolve_prog_via_cross( iv_name ) TO rt_used.
    APPEND LINES OF resolve_prog_via_wbcrossgt( iv_name ) TO rt_used.

  ENDMETHOD.


  METHOD resolve_prog_via_cross.

    DATA: lv_type  TYPE tadir-object,
          lt_cross TYPE STANDARD TABLE OF cross WITH DEFAULT KEY,
          ls_used  LIKE LINE OF rt_used.

    FIELD-SYMBOLS: <ls_cross> LIKE LINE OF lt_cross.


    SELECT * FROM cross INTO TABLE lt_cross
      WHERE include = iv_name
      AND name <> '?'
      ORDER BY PRIMARY KEY.                               "#EC CI_SUBRC
    LOOP AT lt_cross ASSIGNING <ls_cross>.
      CASE <ls_cross>-type.
        WHEN 'F'.
          lv_type = 'FUGR'.
          <ls_cross>-name = find_fugr( <ls_cross>-name ).
          IF <ls_cross>-name IS INITIAL.
            CONTINUE. " seems to happen for ENQUEUE function modules
          ENDIF.
        WHEN '2'.
          lv_type = 'XSLT'.
        WHEN 'R'.
          lv_type = 'PROG'.
        WHEN 'G'.
          lv_type = 'TYPE'.
        WHEN 'A'.
          lv_type = 'SUSO'.
        WHEN '0'.
          CONTINUE. " DEVC?
        WHEN 'K'.
          CONTINUE. " macro
        WHEN '3'.
          lv_type = 'MSAG'.
        WHEN 'N' OR 'P'.
          CONTINUE. " hmm, dunno
        WHEN 'U'.
          CONTINUE. " FORMs in programs
        WHEN OTHERS.
* todo
          CONTINUE.
      ENDCASE.
      ls_used-obj_type = lv_type.
      ls_used-obj_name = <ls_cross>-name.
      APPEND ls_used TO rt_used.
    ENDLOOP.

  ENDMETHOD.


  METHOD resolve_prog_via_wbcrossgt.

    DATA: lv_type      TYPE tadir-object,
          lv_foo       TYPE string ##NEEDED,
          ls_used      LIKE LINE OF rt_used,
          lt_wbcrossgt TYPE STANDARD TABLE OF wbcrossgt WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_wbcrossgt> LIKE LINE OF lt_wbcrossgt.


    SELECT * FROM wbcrossgt INTO TABLE lt_wbcrossgt
      WHERE include = iv_name
      AND direct = abap_true
      ORDER BY PRIMARY KEY.                               "#EC CI_SUBRC
    LOOP AT lt_wbcrossgt ASSIGNING <ls_wbcrossgt>.
      CLEAR lv_type.
      CASE <ls_wbcrossgt>-otype.
        WHEN 'DA'.
          IF <ls_wbcrossgt>-name = 'SY'
              OR <ls_wbcrossgt>-name CP 'SY\*'
              OR <ls_wbcrossgt>-name = 'ABAP_TRUE'
              OR <ls_wbcrossgt>-name = 'ABAP_FALSE'.
            CONTINUE.
          ENDIF.
          IF <ls_wbcrossgt>-name CP '*\ME:*'.
            SPLIT <ls_wbcrossgt>-name AT '\ME:' INTO <ls_wbcrossgt>-name lv_foo.
            lv_type = class_or_interface( <ls_wbcrossgt>-name ).
          ENDIF.
        WHEN 'TY'.
          IF <ls_wbcrossgt>-name = 'ABAP_BOOL'
              OR <ls_wbcrossgt>-name = 'SYST'
              OR <ls_wbcrossgt>-name = 'ABAP_CHAR1'
              OR <ls_wbcrossgt>-name = 'SY'
              OR <ls_wbcrossgt>-name CP 'SY\*'
              OR <ls_wbcrossgt>-name CP 'SYST\*'.
            CONTINUE.
          ENDIF.
        WHEN 'ME'.
          SPLIT <ls_wbcrossgt>-name AT '\ME:' INTO <ls_wbcrossgt>-name lv_foo.
          lv_type = class_or_interface( <ls_wbcrossgt>-name ).
        WHEN 'EV'.
          SPLIT <ls_wbcrossgt>-name AT '\EV:' INTO <ls_wbcrossgt>-name lv_foo.
          lv_type = class_or_interface( <ls_wbcrossgt>-name ).
        WHEN 'TK'.
* todo
          CONTINUE.
        WHEN OTHERS.
* todo
          CONTINUE.
      ENDCASE.

      IF <ls_wbcrossgt>-name CP '*\*'.
        SPLIT <ls_wbcrossgt>-name AT '\' INTO <ls_wbcrossgt>-name lv_foo.
      ENDIF.

      IF lv_type IS INITIAL.
        IF <ls_wbcrossgt>-name CP 'SEOC_*'
            OR <ls_wbcrossgt>-name CP 'SEOX_*'
            OR <ls_wbcrossgt>-name CP 'WDYN_*'
            OR <ls_wbcrossgt>-name CP 'ABAP_*'
            OR <ls_wbcrossgt>-name CP 'SEOK_*'
            OR <ls_wbcrossgt>-name CP 'CNHT_*'
            OR <ls_wbcrossgt>-name CP 'CNTL_*'
            OR <ls_wbcrossgt>-name CP 'TPAK_*'
            OR <ls_wbcrossgt>-name CP 'SKWFC_*'
            OR <ls_wbcrossgt>-name CP 'WBMR_*'
            OR <ls_wbcrossgt>-name CP 'STSTC_*'
            OR <ls_wbcrossgt>-name CP 'SWBM_*'
            OR <ls_wbcrossgt>-name CP 'SVRS2_*'
            OR <ls_wbcrossgt>-name CP 'ICON_*'
            OR <ls_wbcrossgt>-name CP 'SCAN_*'
            OR <ls_wbcrossgt>-name CP 'SMODI_*'
            OR <ls_wbcrossgt>-name CP 'TRSEL_*'
            OR <ls_wbcrossgt>-name CP 'TRWBO_*'
            OR <ls_wbcrossgt>-name CP 'SEWS_*'
            OR <ls_wbcrossgt>-name CP 'SWWW_*'
            OR <ls_wbcrossgt>-name CP 'IHTTP_*'
            OR <ls_wbcrossgt>-name CP 'SBDST_*'
            OR <ls_wbcrossgt>-name CP 'WDYRT_*'
            OR <ls_wbcrossgt>-name CP 'CNDP_*'
            OR <ls_wbcrossgt>-name CP 'SWBME_*'
            OR <ls_wbcrossgt>-name CP 'SO2_*'
            OR <ls_wbcrossgt>-name CP 'SZAL_*'
            OR <ls_wbcrossgt>-name CP 'SREXT_*'
            OR <ls_wbcrossgt>-name CP 'SEOP_*'.
* todo: how to handle type pools?
          CONTINUE.
        ENDIF.

        lv_type = find_type( <ls_wbcrossgt>-name ).
      ENDIF.

      ls_used-obj_type = lv_type.
      ls_used-obj_name = <ls_wbcrossgt>-name.
      APPEND ls_used TO rt_used.
    ENDLOOP.

  ENDMETHOD.


  METHOD resolve_prog_via_wbcrossi.

    DATA: lv_type     TYPE tadir-object,
          ls_used     LIKE LINE OF rt_used,
          lt_wbcrossi TYPE STANDARD TABLE OF wbcrossi WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_wbcrossi> LIKE LINE OF lt_wbcrossi.


    SELECT * FROM wbcrossi INTO TABLE lt_wbcrossi
      WHERE include = iv_name
      ORDER BY PRIMARY KEY.                               "#EC CI_SUBRC
    LOOP AT lt_wbcrossi ASSIGNING <ls_wbcrossi>.
      CASE <ls_wbcrossi>-otype.
        WHEN 'IC'.
          lv_type = 'PROG'.
        WHEN OTHERS.
* todo
          CONTINUE.
      ENDCASE.
      ls_used-obj_type = lv_type.
      ls_used-obj_name = <ls_wbcrossi>-name.
      APPEND ls_used TO rt_used.
    ENDLOOP.

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
      WHERE tabname = iv_name
      ORDER BY domname ASCENDING.
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

    DATA: lv_rowtype TYPE tadir-obj_name,
          ls_used    LIKE LINE OF rt_used.


    SELECT SINGLE rowtype FROM dd40l INTO lv_rowtype
      WHERE typename = iv_name.                         "#EC CI_NOORDER
    IF sy-subrc = 0 AND NOT lv_rowtype IS INITIAL.
      ls_used-obj_type = find_type( lv_rowtype ).
      ls_used-obj_name = lv_rowtype.
      APPEND ls_used TO rt_used.
    ENDIF.

  ENDMETHOD.


  METHOD resolve_view.

* todo

  ENDMETHOD.
ENDCLASS.
