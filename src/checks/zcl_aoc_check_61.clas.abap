CLASS zcl_aoc_check_61 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super_root
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS get_attributes
        REDEFINITION .
    METHODS get_message_text
        REDEFINITION .
    METHODS if_ci_test~query_attributes
        REDEFINITION .
    METHODS put_attributes
        REDEFINITION .
    METHODS run
        REDEFINITION .
  PROTECTED SECTION.

    TYPES:
      ty_seosubcodf_tt TYPE STANDARD TABLE OF seosubcodf WITH DEFAULT KEY .
    TYPES:
      ty_vseosubcdf_tt TYPE STANDARD TABLE OF vseosubcdf WITH DEFAULT KEY .
  PRIVATE SECTION.

    DATA mv_level TYPE devclass .
    DATA mt_devclass TYPE packrange .

    METHODS check_tabl
      IMPORTING
        !iv_name TYPE clike .
    METHODS find_doma_package
      IMPORTING
        !iv_name       TYPE clike
      RETURNING
        VALUE(rv_devc) TYPE devclass .
    METHODS find_dtel_package
      IMPORTING
        !iv_name       TYPE clike
      RETURNING
        VALUE(rv_devc) TYPE devclass .
    METHODS find_encapsulation
      RETURNING
        VALUE(rv_parent) TYPE devclass .
    METHODS find_tabl_package
      IMPORTING
        !iv_name       TYPE clike
      RETURNING
        VALUE(rv_devc) TYPE devclass .
    METHODS is_part
      IMPORTING
        !iv_package     TYPE devclass
      RETURNING
        VALUE(rv_found) TYPE abap_bool .
    METHODS report
      IMPORTING
        !iv_objtype TYPE trobjtype
        !iv_objname TYPE clike .
ENDCLASS.



CLASS ZCL_AOC_CHECK_61 IMPLEMENTATION.


  METHOD check_tabl.

    TYPES: BEGIN OF ty_dd03l,
             domname  TYPE dd03l-domname,
             rollname TYPE dd03l-rollname,
             comptype TYPE dd03l-comptype,
           END OF ty_dd03l.

    DATA: lt_dd03l   TYPE STANDARD TABLE OF ty_dd03l WITH DEFAULT KEY,
          lv_package TYPE devclass,
          ls_dd03l   LIKE LINE OF lt_dd03l.


    lv_package = find_tabl_package( iv_name ).
    IF is_part( lv_package ) = abap_false.
      report( iv_objtype = 'TABL'
              iv_objname = iv_name ).
    ENDIF.

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
            check_tabl( ls_dd03l-rollname ).
            CONTINUE.
          ENDIF.
        WHEN 'T'.
*        check_TTYP( ls_dd03l-rollname ).
          CONTINUE.
      ENDCASE.

      IF NOT ls_dd03l-domname IS INITIAL.
        lv_package = find_doma_package( ls_dd03l-domname ).
        IF is_part( lv_package ) = abap_false.
          report( iv_objtype = 'DOMA'
                  iv_objname = ls_dd03l-domname ).
        ENDIF.
      ENDIF.

      IF NOT ls_dd03l-rollname IS INITIAL.
        lv_package = find_dtel_package( ls_dd03l-rollname ).
        IF is_part( lv_package ) = abap_false.
          report( iv_objtype = 'DTEL'
                  iv_objname = ls_dd03l-rollname ).
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '002'.
    position       = '061'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.
    has_documentation = abap_true.

    mv_errty      = c_error.

    add_obj_type( 'TABL' ).

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD find_doma_package.

    SELECT SINGLE devclass FROM tadir INTO rv_devc
      WHERE pgmid = 'R3TR'
      AND object = 'DOMA'
      AND obj_name = iv_name.

  ENDMETHOD.


  METHOD find_dtel_package.

    SELECT SINGLE devclass FROM tadir INTO rv_devc
      WHERE pgmid = 'R3TR'
      AND object = 'DTEL'
      AND obj_name = iv_name.

  ENDMETHOD.


  METHOD find_encapsulation.

    DATA: lv_devclass TYPE devclass.


    SELECT SINGLE devclass FROM tadir INTO lv_devclass
      WHERE pgmid = 'R3TR'
      AND object = object_type
      AND obj_name = object_name.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    WHILE NOT lv_devclass IS INITIAL.
      IF lv_devclass IN mt_devclass.
        rv_parent = lv_devclass.
        RETURN.
      ENDIF.

      SELECT SINGLE parentcl INTO lv_devclass FROM tdevc
        WHERE devclass = lv_devclass.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDWHILE.

  ENDMETHOD.


  METHOD find_tabl_package.

    SELECT SINGLE devclass FROM tadir INTO rv_devc
      WHERE pgmid = 'R3TR'
      AND object = 'TABL'
      AND obj_name = iv_name.

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mt_devclass = mt_devclass
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Uses &1 &2, which is outside of the encapsulation'.
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mt_devclass 'Package' 'S'.               "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD is_part.

    DATA: lv_package TYPE devclass.


    lv_package = iv_package.

    WHILE NOT lv_package IS INITIAL.
      IF lv_package = mv_level.
        rv_found = abap_true.
        RETURN.
      ENDIF.

      SELECT SINGLE parentcl FROM tdevc INTO lv_package
        WHERE devclass = lv_package.
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.
    ENDWHILE.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mt_devclass = mt_devclass
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD report.

    DATA: lv_srcsystem TYPE tadir-srcsystem.


    SELECT SINGLE srcsystem FROM tadir
      INTO lv_srcsystem
      WHERE pgmid = 'R3TR'
      AND object = iv_objtype
      AND obj_name = iv_objname.
    IF sy-subrc <> 0 OR lv_srcsystem = 'SAP'.
      RETURN.
    ENDIF.

    inform( p_sub_obj_type = object_type
            p_sub_obj_name = object_name
            p_test         = myname
            p_kind         = mv_errty
            p_code         = '001'
            p_param_1      = iv_objtype
            p_param_2      = iv_objname ).

  ENDMETHOD.


  METHOD run.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_level TYPE devclass.


    IF lines( mt_devclass ) = 0.
      RETURN.
    ENDIF.

    mv_level = find_encapsulation( ).
    IF mv_level IS INITIAL.
      RETURN.
    ENDIF.

    CASE object_type.
      WHEN 'TABL'.
        check_tabl( object_name ).
      WHEN OTHERS.
        ASSERT 0 = 1.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
