class ZCL_AOC_EMBEDDED_PACKAGES definition
  public
  inheriting from CL_CI_COLLECTOR_ROOT
  create public .

public section.

  methods CONSTRUCTOR .

  methods IF_CI_COLLECTOR~COLLECT
    redefinition .
  methods IF_CI_COLLECTOR~GET_ATTRIBUTES
    redefinition .
  methods IF_CI_COLLECTOR~PUT_ATTRIBUTES
    redefinition .
  methods IF_CI_COLLECTOR~QUERY_ATTRIBUTES
    redefinition .
PROTECTED SECTION.

  TYPES ty_package_list_tt TYPE STANDARD TABLE OF devclass WITH DEFAULT KEY.

  METHODS find_embedded
    RETURNING VALUE(rt_embedded) TYPE ty_package_list_tt.
private section.

  data MT_PACKAGES type SCIT_DEVC .
ENDCLASS.



CLASS ZCL_AOC_EMBEDDED_PACKAGES IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    description = 'AOC - Objects from Embedded Packages'(000).
    position    = '050'.
    version     = '000'.

    has_attributes = abap_true.
    group          = c_general.

  ENDMETHOD.


  METHOD find_embedded.

    DATA: lt_packages TYPE ty_package_list_tt.


    IF lines( mt_packages ) = 0.
      RETURN.
    ENDIF.

    SELECT devclass FROM tdevc
      INTO TABLE lt_packages
      WHERE devclass IN mt_packages.                    "#EC CI_GENBUFF
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    rt_embedded = lt_packages.

    WHILE lines( lt_packages ) > 0.
      SELECT devclass
        FROM tdevc
        INTO TABLE lt_packages
        FOR ALL ENTRIES IN lt_packages
        WHERE parentcl = lt_packages-table_line.        "#EC CI_GENBUFF

      APPEND LINES OF lt_packages TO rt_embedded.
    ENDWHILE.

  ENDMETHOD.


  METHOD if_ci_collector~collect.

    DATA: lt_embedded TYPE ty_package_list_tt,
          lt_objects  TYPE STANDARD TABLE OF scir_objs_s,
          ls_object   LIKE LINE OF p_objslist.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF lt_objects.


    lt_embedded = find_embedded( ).
    IF lines( lt_embedded ) = 0.
      RETURN.
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
      AND delflag = abap_false ##TOO_MANY_ITAB_FIELDS.  "#EC CI_GENBUFF

    LOOP AT lt_objects ASSIGNING <ls_object>.
      MOVE-CORRESPONDING <ls_object> TO ls_object.
      APPEND ls_object TO p_objslist.
    ENDLOOP.

  ENDMETHOD.


  METHOD if_ci_collector~get_attributes.

    EXPORT mt_packages = mt_packages TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD if_ci_collector~put_attributes.

    IMPORT mt_packages = mt_packages FROM DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD if_ci_collector~query_attributes.

    DATA:
      lt_attributes TYPE sci_atttab,
      ls_attribute  LIKE LINE OF lt_attributes,
*      l_message(72) TYPE c,
      lv_break      TYPE abap_bool,
      lv_ok         TYPE abap_bool.


    GET REFERENCE OF mt_packages INTO ls_attribute-ref.
    ls_attribute-text = 'Packages'.
    ls_attribute-kind = 'S'.
    APPEND ls_attribute TO lt_attributes.

    DO.
      lv_break = cl_ci_query_attributes=>generic(
                      p_name       = myname
                      p_title      = 'Settings'
                      p_attributes = lt_attributes
*                    p_message    = l_message
                      p_display    = p_display ).
      IF lv_break = abap_true.
        RETURN.
      ENDIF.
      IF lines( mt_packages ) > 0.
        attributes_ok = c_true.
        EXIT. " current loop
      ENDIF.
    ENDDO.

  ENDMETHOD.
ENDCLASS.
