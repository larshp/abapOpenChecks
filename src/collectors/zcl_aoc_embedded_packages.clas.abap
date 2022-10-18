CLASS zcl_aoc_embedded_packages DEFINITION
  PUBLIC
  INHERITING FROM cl_ci_collector_root
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS if_ci_collector~collect
        REDEFINITION .
    METHODS if_ci_collector~get_attributes
        REDEFINITION .
    METHODS if_ci_collector~put_attributes
        REDEFINITION .
    METHODS if_ci_collector~query_attributes
        REDEFINITION .
  PROTECTED SECTION.

    TYPES ty_package_list_tt TYPE STANDARD TABLE OF devclass WITH DEFAULT KEY.

    METHODS find_embedded
      RETURNING VALUE(rt_embedded) TYPE ty_package_list_tt.
  PRIVATE SECTION.

    DATA mt_packages TYPE scit_devc .
    DATA mv_local TYPE flag .
ENDCLASS.



CLASS zcl_aoc_embedded_packages IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    description = 'AOC - Objects from Embedded Packages'(000).
    position    = '050'.
    version     = '000'.

    has_attributes = abap_true.
    group          = c_general.

  ENDMETHOD.


  METHOD find_embedded.

    DATA: lt_packages  TYPE ty_package_list_tt.


    IF lines( mt_packages ) = 0.
      RETURN.
    ENDIF.

    SELECT devclass FROM tdevc
      INTO TABLE lt_packages
      WHERE devclass IN mt_packages
      ORDER BY devclass ASCENDING.                      "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    rt_embedded = lt_packages.

    WHILE lines( lt_packages ) > 0.
      SELECT devclass
        FROM tdevc
        INTO TABLE lt_packages
        FOR ALL ENTRIES IN lt_packages
        WHERE parentcl = lt_packages-table_line
        ORDER BY PRIMARY KEY.                           "#EC CI_GENBUFF
      IF sy-subrc = 0.
        APPEND LINES OF lt_packages TO rt_embedded.
      ENDIF.
    ENDWHILE.

  ENDMETHOD.


  METHOD if_ci_collector~collect.

    DATA: lt_embedded  TYPE ty_package_list_tt,
          lt_objects   TYPE STANDARD TABLE OF scir_objs_s,
          ls_object    LIKE LINE OF p_objslist,
          lt_srcsystem TYPE RANGE OF tadir-srcsystem,
          ls_srcsystem LIKE LINE OF lt_srcsystem.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF lt_objects.


    CLEAR: p_objslist, p_srcid.

    lt_embedded = find_embedded( ).
    IF lines( lt_embedded ) = 0.
      RETURN.
    ENDIF.

    IF mv_local = abap_true.
      ls_srcsystem-option = 'EQ'.
      ls_srcsystem-sign   = 'I'.
      ls_srcsystem-low    = sy-sysid.
      APPEND ls_srcsystem TO lt_srcsystem.
    ENDIF.

    SELECT object obj_name FROM tadir
      INTO TABLE lt_objects
      FOR ALL ENTRIES IN lt_embedded
      WHERE devclass = lt_embedded-table_line
      AND pgmid = 'R3TR'
      AND object IN p_confine_objtypes
      AND obj_name IN p_confine_objnames
      AND devclass IN p_confine_devclasses
      AND author IN p_confine_responsibles
      AND srcsystem IN lt_srcsystem
      AND delflag = abap_false ##TOO_MANY_ITAB_FIELDS. "#EC CI_GENBUFF "#EC CI_SUBRC

    LOOP AT lt_objects ASSIGNING <ls_object>.
      MOVE-CORRESPONDING <ls_object> TO ls_object.
      APPEND ls_object TO p_objslist.
    ENDLOOP.

  ENDMETHOD.


  METHOD if_ci_collector~get_attributes.

    EXPORT
      mt_packages = mt_packages
      mv_local = mv_local
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD if_ci_collector~put_attributes.

    IMPORT
      mt_packages = mt_packages
      mv_local = mv_local
      FROM DATA BUFFER p_attributes.                      "#EC CI_SUBRC

  ENDMETHOD.


  METHOD if_ci_collector~query_attributes.

    DATA:
      lt_attributes TYPE sci_atttab,
      ls_attribute  LIKE LINE OF lt_attributes,
      lv_break      TYPE abap_bool.


    GET REFERENCE OF mt_packages INTO ls_attribute-ref.
    ls_attribute-text = 'Packages'(001).
    ls_attribute-kind = 'S'.
    APPEND ls_attribute TO lt_attributes.

    GET REFERENCE OF mv_local INTO ls_attribute-ref.
    ls_attribute-text = 'Local objects only'(002).
    APPEND ls_attribute TO lt_attributes.

    DO.
      lv_break = cl_ci_query_attributes=>generic(
        p_name       = myname
        p_title      = 'Settings'
        p_attributes = lt_attributes
        p_display    = p_display ) ##NO_TEXT.
      IF lv_break = abap_true.
        RETURN.
      ENDIF.
      IF lines( mt_packages ) > 0.
        attributes_ok = abap_true.
        EXIT. " current loop
      ENDIF.
    ENDDO.

  ENDMETHOD.
ENDCLASS.
