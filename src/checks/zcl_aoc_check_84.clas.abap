CLASS zcl_aoc_check_84 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super_root
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS run
        REDEFINITION .

    METHODS get_attributes
        REDEFINITION .

    METHODS put_attributes
        REDEFINITION .

    METHODS if_ci_test~query_attributes
        REDEFINITION .

  PROTECTED SECTION.
    DATA mt_classes TYPE zaoc_seoclsname_range_tt.
    DATA mv_read_only_allowed TYPE flag.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aoc_check_84 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    version  = '002'.
    position = '084'.

    has_documentation = abap_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    add_obj_type( 'INTF' ).
    add_obj_type( 'CLAS' ).

    insert_scimessage(
      iv_code = '001'
      iv_text = 'No public attributes, &1'(m01) ).

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT mv_errty   = mv_errty
           mt_classes = mt_classes
           mv_read_only_allowed = mv_read_only_allowed TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mt_classes 'Skip Classes' 'S'.           "#EC NOTEXT
    zzaoc_fill_att mv_read_only_allowed 'Allow READ-ONLY attributes' ''. "#EC NOTEXT
    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT mv_errty   = mv_errty
           mt_classes = mt_classes
           mv_read_only_allowed = mv_read_only_allowed FROM DATA BUFFER p_attributes. "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD run.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    TYPES: BEGIN OF ty_attr,
             cmpname   TYPE vseocompdf-cmpname,
             attrdonly TYPE vseocompdf-attrdonly,
           END OF ty_attr.

    DATA: lt_attr     TYPE STANDARD TABLE OF ty_attr WITH DEFAULT KEY,
          ls_attr     LIKE LINE OF lt_attr,
          lv_category TYPE seoclassdf-category.

    IF mt_classes IS NOT INITIAL AND object_name IN mt_classes.
      RETURN.
    ENDIF.

    SELECT cmpname attrdonly FROM vseocompdf INTO CORRESPONDING FIELDS OF TABLE lt_attr
      WHERE clsname = object_name
      AND version = '1'
      AND cmptype = '0'
      AND exposure = '2'
      AND attdecltyp <> '2'
      ORDER BY cmpname   ASCENDING
               attrdonly ASCENDING.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF mv_read_only_allowed = abap_true.
      DELETE lt_attr WHERE attrdonly = abap_true.
      IF lines( lt_attr ) = 0.
        RETURN.
      ENDIF.
    ENDIF.

    SELECT SINGLE category FROM seoclassdf
      INTO lv_category
      WHERE clsname = object_name
      AND version = '1'.
    IF sy-subrc = 0 AND lv_category = seoc_category_exception.
      RETURN.
    ENDIF.

    LOOP AT lt_attr INTO ls_attr.
      inform( p_test    = myname
              p_kind    = mv_errty
              p_code    = '001'
              p_param_1 = ls_attr-cmpname ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
