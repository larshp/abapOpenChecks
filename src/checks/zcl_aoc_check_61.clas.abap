CLASS zcl_aoc_check_61 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super_root
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS run
        REDEFINITION .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_encapsulation,
        package TYPE devclass,
      END OF ty_encapsulation .
    TYPES:
      ty_devclass_tt TYPE STANDARD TABLE OF devclass WITH DEFAULT KEY .

    METHODS explode_pinf
      IMPORTING
        !it_permissions   TYPE permis_tab
      RETURNING
        VALUE(rt_allowed) TYPE zcl_aoc_dependencies=>ty_objects_tt .
    METHODS list_permissions
      IMPORTING
        !iv_package    TYPE devclass
      RETURNING
        VALUE(rt_list) TYPE permis_tab .
    METHODS read_classification
      IMPORTING
        !iv_package     TYPE devclass
      RETURNING
        VALUE(rv_value) TYPE cls_attribute_value .
    METHODS find_encapsulation
      RETURNING
        VALUE(rs_encapsulation) TYPE ty_encapsulation .
    METHODS check_package_interfaces
      IMPORTING
        !is_encapsulation TYPE ty_encapsulation
      CHANGING
        !ct_used          TYPE zcl_aoc_dependencies=>ty_objects_tt .
    METHODS check_used_objects
      IMPORTING
        !is_encapsulation TYPE ty_encapsulation
        VALUE(it_used)    TYPE zcl_aoc_dependencies=>ty_objects_tt .
    METHODS list_allowed_objects
      IMPORTING
        !iv_package    TYPE devclass
      RETURNING
        VALUE(rt_list) TYPE zcl_aoc_dependencies=>ty_objects_tt .
    METHODS list_allowed_packages
      IMPORTING
        !iv_package    TYPE devclass
      RETURNING
        VALUE(rt_list) TYPE ty_devclass_tt .
    METHODS list_subpackages
      IMPORTING
        !iv_package    TYPE devclass
      RETURNING
        VALUE(rt_list) TYPE ty_devclass_tt .
    METHODS list_superpackages
      IMPORTING
        !iv_package    TYPE devclass
      RETURNING
        VALUE(rt_list) TYPE ty_devclass_tt .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aoc_check_61 IMPLEMENTATION.


  METHOD check_package_interfaces.

    DATA: lt_allowed TYPE zcl_aoc_dependencies=>ty_objects_tt,
          ls_allowed LIKE LINE OF lt_allowed.


    lt_allowed = explode_pinf( list_permissions( is_encapsulation-package ) ).

    LOOP AT lt_allowed INTO ls_allowed.
      DELETE ct_used
        WHERE obj_type = ls_allowed-obj_type
        AND obj_name = ls_allowed-obj_name.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_used_objects.

    DATA: lt_allow   TYPE ty_devclass_tt,
          lv_allow   LIKE LINE OF lt_allow,
          lt_sub     TYPE ty_devclass_tt,
          lv_sub     LIKE LINE OF lt_sub,
          lt_objects LIKE it_used,
          ls_object  LIKE LINE OF lt_objects,
          ls_used    LIKE LINE OF it_used.


    IF lines( it_used ) = 0.
      RETURN.
    ENDIF.

    lt_sub = list_subpackages( is_encapsulation-package ).
    LOOP AT lt_sub INTO lv_sub.
      DELETE it_used WHERE package = lv_sub.
    ENDLOOP.

    lt_allow = list_allowed_packages( is_encapsulation-package ).
    LOOP AT lt_allow INTO lv_allow.
      DELETE it_used WHERE package = lv_allow.
      lt_sub = list_subpackages( lv_allow ).
      LOOP AT lt_sub INTO lv_sub.
        DELETE it_used WHERE package = lv_sub.
      ENDLOOP.
    ENDLOOP.

    lt_objects = list_allowed_objects( is_encapsulation-package ).
    LOOP AT lt_objects INTO ls_object.
      DELETE it_used WHERE obj_type = ls_object-obj_type AND obj_name = ls_object-obj_name.
    ENDLOOP.

    check_package_interfaces(
      EXPORTING
        is_encapsulation = is_encapsulation
      CHANGING
        ct_used          = it_used ).

    LOOP AT it_used INTO ls_used.
      inform( p_test    = myname
              p_kind    = mv_errty
              p_code    = '001'
              p_param_1 = ls_used-obj_type
              p_param_2 = ls_used-obj_name
              p_param_3 = is_encapsulation-package ).
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version  = '002'.
    position = '061'.

    has_documentation = abap_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    add_obj_type( 'TRAN' ).
    add_obj_type( 'CLAS' ).
    add_obj_type( 'PROG' ).
    add_obj_type( 'INTF' ).
    add_obj_type( 'DDLS' ).
    add_obj_type( 'DOMA' ).
    add_obj_type( 'DTEL' ).
    add_obj_type( 'TABL' ).
    add_obj_type( 'FUGR' ).
    add_obj_type( 'TTYP' ).
    add_obj_type( 'VIEW' ).

    insert_scimessage(
      iv_code = '001'
      iv_text = 'Uses &1 &2, which is outside of the encapsulation(&3)'(m01) ).

  ENDMETHOD.


  METHOD explode_pinf.

* todo, IT_PERMISSIONS-ERR_SEVER is currently ignored

    DATA: lt_pinf       TYPE permis_tab,
          ls_permission LIKE LINE OF it_permissions.


    LOOP AT it_permissions INTO ls_permission.
      SELECT elem_key AS intf_name FROM ifobjshort
        APPENDING CORRESPONDING FIELDS OF TABLE lt_pinf
        WHERE intf_name = ls_permission-intf_name
        AND elem_type = 'PINF'
        ORDER BY elem_key.                                "#EC CI_SUBRC

      SELECT elem_type AS obj_type
        elem_key AS obj_name
        FROM ifobjshort
        APPENDING TABLE rt_allowed
        WHERE intf_name = ls_permission-intf_name
        AND elem_type <> 'PINF'
        ORDER BY elem_type ASCENDING
                 elem_key  ASCENDING.                     "#EC CI_SUBRC
    ENDLOOP.

    IF lines( lt_pinf ) > 0.
      APPEND LINES OF explode_pinf( lt_pinf ) TO rt_allowed.
    ENDIF.

  ENDMETHOD.


  METHOD find_encapsulation.

    DATA: lt_packages TYPE ty_devclass_tt,
          lv_package  LIKE LINE OF lt_packages.


    SELECT SINGLE devclass INTO lv_package
      FROM tadir
      WHERE object = object_type
      AND obj_name = object_name.       "#EC CI_GENBUFF "#EC CI_NOORDER
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lt_packages = list_superpackages( lv_package ).

    LOOP AT lt_packages INTO lv_package.
      IF read_classification( lv_package ) <> 'N'.
        rs_encapsulation-package = lv_package.
        RETURN.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD list_allowed_objects.

    DATA: ls_object         TYPE pak_object_key,
          ls_classification TYPE cl_cls_attr_value_assignment=>ty_classification,
          lv_type           TYPE string,
          lv_name           TYPE string,
          ls_assignment     LIKE LINE OF ls_classification-assignments.

    FIELD-SYMBOLS: <ls_list> LIKE LINE OF rt_list.


    TRY.
        ls_object-trobjtype = 'DEVC'.
        ls_object-sobj_name = iv_package.

        cl_cls_attr_value_assignment=>get_attr_value_assignment(
          EXPORTING
            im_object         = ls_object
            im_attribute      = 'ZAOC_ENCAPSULATION_OBJECTS'
          IMPORTING
            ex_classification = ls_classification ).
      CATCH cx_pak_not_authorized cx_pak_invalid_state cx_pak_invalid_data.
        RETURN.
    ENDTRY.

    LOOP AT ls_classification-assignments INTO ls_assignment.
      SPLIT ls_assignment-value AT ',' INTO lv_type lv_name.

      APPEND INITIAL LINE TO rt_list ASSIGNING <ls_list>.
      <ls_list>-obj_type = lv_type.
      <ls_list>-obj_name = lv_name.
    ENDLOOP.

  ENDMETHOD.


  METHOD list_allowed_packages.

    DATA: ls_object         TYPE pak_object_key,
          ls_classification TYPE cl_cls_attr_value_assignment=>ty_classification,
          ls_assignment     LIKE LINE OF ls_classification-assignments.


    TRY.
        ls_object-trobjtype = 'DEVC'.
        ls_object-sobj_name = iv_package.

        cl_cls_attr_value_assignment=>get_attr_value_assignment(
          EXPORTING
            im_object         = ls_object
            im_attribute      = 'ZAOC_ENCAPSULATION_PACKAGES'
          IMPORTING
            ex_classification = ls_classification ).
      CATCH cx_pak_not_authorized cx_pak_invalid_state cx_pak_invalid_data.
        RETURN.
    ENDTRY.

    LOOP AT ls_classification-assignments INTO ls_assignment.
      APPEND ls_assignment-value TO rt_list.
    ENDLOOP.

  ENDMETHOD.


  METHOD list_permissions.

    SELECT * FROM permission INTO TABLE rt_list
      WHERE client_pak = iv_package
      ORDER BY PRIMARY KEY.                               "#EC CI_SUBRC

  ENDMETHOD.


  METHOD list_subpackages.

    DATA: lt_list     LIKE rt_list,
          lv_devclass LIKE LINE OF rt_list.


    SELECT devclass INTO TABLE rt_list
      FROM tdevc WHERE parentcl = iv_package
      ORDER BY devclass ASCENDING.        "#EC CI_GENBUFF "#EC CI_SUBRC

* note the recursion, since packages are added to the list
    LOOP AT rt_list INTO lv_devclass.
      lt_list = list_subpackages( lv_devclass ).
      APPEND LINES OF lt_list TO rt_list.
    ENDLOOP.

  ENDMETHOD.


  METHOD list_superpackages.

    DATA: lt_list   LIKE rt_list,
          lv_parent TYPE tdevc-parentcl.


    APPEND iv_package TO rt_list.

    SELECT SINGLE parentcl INTO lv_parent
      FROM tdevc WHERE devclass = iv_package.           "#EC CI_GENBUFF

* todo, rewrite to iteration instead of recursion
    IF sy-subrc = 0 AND NOT lv_parent IS INITIAL.
      lt_list = list_superpackages( lv_parent ).
      APPEND LINES OF lt_list TO rt_list.
    ENDIF.

  ENDMETHOD.


  METHOD read_classification.

    DATA: ls_object         TYPE pak_object_key,
          ls_classification TYPE cl_cls_attr_value_assignment=>ty_classification,
          ls_assignment     LIKE LINE OF ls_classification-assignments.


    TRY.
        ls_object-trobjtype = 'DEVC'.
        ls_object-sobj_name = iv_package.

        cl_cls_attr_value_assignment=>get_attr_value_assignment(
          EXPORTING
            im_object         = ls_object
            im_attribute      = 'ZAOC_ENCAPSULATION'
          IMPORTING
            ex_classification = ls_classification ).
      CATCH cx_pak_not_authorized cx_pak_invalid_state cx_pak_invalid_data.
        RETURN.
    ENDTRY.

    READ TABLE ls_classification-assignments INDEX 1 INTO ls_assignment.
    IF sy-subrc = 0.
      rv_value = ls_assignment-value.
    ENDIF.

  ENDMETHOD.


  METHOD run.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_used          TYPE zcl_aoc_dependencies=>ty_objects_tt,
          ls_encapsulation TYPE ty_encapsulation.


    ls_encapsulation = find_encapsulation( ).
    IF ls_encapsulation IS INITIAL.
      RETURN.
    ENDIF.

    lt_used = zcl_aoc_dependencies=>resolve(
      iv_obj_type = object_type
      iv_obj_name = object_name ).

    check_used_objects(
      is_encapsulation = ls_encapsulation
      it_used          = lt_used ).

  ENDMETHOD.
ENDCLASS.
