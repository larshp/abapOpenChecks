CLASS zcl_aoc_check_58 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
    METHODS get_attributes
        REDEFINITION .
    METHODS if_ci_test~query_attributes
        REDEFINITION .
    METHODS put_attributes
        REDEFINITION .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF gty_reference_s,
        clsname  TYPE seocompodf-clsname,
        cmpname  TYPE seocompodf-cmpname,
        exposure TYPE seocompodf-cmpname,
        alias    TYPE seocompodf-alias,
      END OF gty_reference_s .
    TYPES:
      gty_reference_t TYPE STANDARD TABLE OF gty_reference_s WITH DEFAULT KEY .
    TYPES:
      gty_include_t TYPE STANDARD TABLE OF wbcrossgt-include WITH DEFAULT KEY .

    CONSTANTS c_private TYPE seocompodf-exposure VALUE '0' ##NO_TEXT.
    CONSTANTS c_protected TYPE seocompodf-exposure VALUE '1' ##NO_TEXT.
    CONSTANTS c_public TYPE seocompodf-exposure VALUE '2' ##NO_TEXT.
    DATA mv_skip_ccau TYPE sap_bool .
    DATA mt_methods TYPE zaoc_seocmpname_range_tt .

    METHODS check_types .
    METHODS report_clas
      IMPORTING
        !is_method   TYPE gty_reference_s
        !iv_err_code TYPE sci_errc .
    METHODS is_bopf_interface
      RETURNING
        VALUE(rv_boolean) TYPE abap_bool .
    METHODS check_constants .
    METHODS check_methods .
    METHODS filter_implementations
      IMPORTING
        !is_method  TYPE gty_reference_s
      CHANGING
        !ct_include TYPE gty_include_t .
    METHODS filter_self_references
      IMPORTING
        !is_method  TYPE gty_reference_s
      CHANGING
        !ct_include TYPE gty_include_t .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_58 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    IF object_type <> 'CLAS' AND object_type <> 'INTF'.
      RETURN.
    ENDIF.

    check_methods( ).
    check_constants( ).
    check_types( ).

  ENDMETHOD.


  METHOD check_constants.

    DATA: lv_name      TYPE wbcrossgt-name,
          lv_include   TYPE programm,
          lt_constants TYPE gty_reference_t.

    FIELD-SYMBOLS: <ls_constant> LIKE LINE OF lt_constants.


    IF is_bopf_interface( ) = abap_true.
      RETURN.
    ENDIF.

    SELECT clsname cmpname exposure alias FROM seocompodf
      INTO CORRESPONDING FIELDS OF TABLE lt_constants
      WHERE clsname = object_name
      AND version = '1'
      AND exposure = c_public
      AND attdecltyp = '2'
      ORDER BY PRIMARY KEY.                               "#EC CI_SUBRC

    LOOP AT lt_constants ASSIGNING <ls_constant>.
      CONCATENATE <ls_constant>-clsname '\DA:' <ls_constant>-cmpname INTO lv_name.
      SELECT SINGLE name FROM wbcrossgt INTO lv_name WHERE otype = 'DA' AND name = lv_name ##WARN_OK.
      IF sy-subrc <> 0.
        IF object_type = 'CLAS'.
          lv_include = cl_oo_classname_service=>get_pubsec_name( <ls_constant>-clsname ).
        ELSE.
          lv_include = cl_oo_classname_service=>get_interfacepool_name( <ls_constant>-clsname ).
        ENDIF.

        inform( p_sub_obj_name = lv_include
                p_kind         = mv_errty
                p_test         = myname
                p_code         = '002'
                p_param_1      = <ls_constant>-cmpname ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD check_methods.

    DATA: lt_methods     TYPE gty_reference_t,
          lv_name        TYPE wbcrossgt-name,
          lv_category    TYPE seoclassdf-category,
          lt_ref_include TYPE gty_include_t.

    FIELD-SYMBOLS: <ls_method> LIKE LINE OF lt_methods.


    IF mv_skip_ccau = abap_true.
      SELECT SINGLE category FROM seoclassdf INTO lv_category
        WHERE version = '1' AND clsname = object_name.
      IF sy-subrc = 0 AND lv_category = '05'.
        RETURN.
      ENDIF.
    ENDIF.

    SELECT clsname cmpname exposure alias
      FROM vseocompdf
      INTO CORRESPONDING FIELDS OF TABLE lt_methods
      WHERE clsname = object_name
      AND cmptype = '1'
      AND version = '1'
      AND ( exposure = c_protected OR exposure = c_public OR exposure = c_private )
      AND type = ''
      AND cmpname <> 'CLASS_CONSTRUCTOR'
      AND cmpname <> 'CONSTRUCTOR'
      ORDER BY clsname cmpname version.                               "#EC CI_SUBRC

    LOOP AT lt_methods ASSIGNING <ls_method>.
      CONCATENATE <ls_method>-clsname '\ME:' <ls_method>-cmpname INTO lv_name.

      SELECT include FROM wbcrossgt
        INTO TABLE lt_ref_include
        WHERE otype = 'ME'
        AND name = lv_name.                               "#EC CI_SUBRC

      IF mv_skip_ccau = abap_true.
        DELETE lt_ref_include WHERE table_line+30 = 'CCAU'.
      ENDIF.

      IF lines( lt_ref_include ) = 0 AND object_type = 'CLAS'.
        IF <ls_method>-alias = abap_false.
          report_clas(
            is_method   = <ls_method>
            iv_err_code = '001' ).
        ELSE.
          report_clas(
            is_method   = <ls_method>
            iv_err_code = '007' ).
        ENDIF.
      ELSEIF lines( lt_ref_include ) = 0 AND object_type = 'INTF'.
        inform( p_param_1 = <ls_method>-cmpname
                p_kind    = mv_errty
                p_test    = myname
                p_code    = '005' ).
      ELSEIF object_type = 'CLAS' AND <ls_method>-exposure = c_public.
        filter_self_references(
          EXPORTING
            is_method  = <ls_method>
          CHANGING
            ct_include = lt_ref_include ).

        IF lines( lt_ref_include ) = 0.
          report_clas(
            is_method   = <ls_method>
            iv_err_code = '003' ).
        ENDIF.
      ELSEIF object_type = 'INTF'.
        filter_implementations(
          EXPORTING
            is_method  = <ls_method>
          CHANGING
            ct_include = lt_ref_include ).

        IF lines( lt_ref_include ) = 0.
          inform( p_param_1 = <ls_method>-cmpname
                  p_kind    = mv_errty
                  p_test    = myname
                  p_code    = '004' ).
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD check_types.

    DATA: lt_types   TYPE gty_reference_t,
          lv_name    TYPE wbcrossgt-name,
          lv_include TYPE programm.

    FIELD-SYMBOLS: <ls_type> LIKE LINE OF lt_types.


    SELECT clsname cmpname exposure alias FROM vseocompdf
      INTO CORRESPONDING FIELDS OF TABLE lt_types
      WHERE clsname = object_name
      AND cmptype = '3'
      AND version = '1'
      AND ( exposure = c_protected OR exposure = c_public OR exposure = c_private )
      AND type = ''
      ORDER BY clsname cmpname version.                               "#EC CI_SUBRC

    LOOP AT lt_types ASSIGNING <ls_type>.
      CONCATENATE <ls_type>-clsname '\TY:' <ls_type>-cmpname INTO lv_name.
      SELECT SINGLE name FROM wbcrossgt INTO lv_name WHERE otype = 'TY' AND name = lv_name ##WARN_OK.
      IF sy-subrc <> 0.
        IF object_type = 'CLAS'.
          lv_include = cl_oo_classname_service=>get_pubsec_name( <ls_type>-clsname ).
        ELSE.
          lv_include = cl_oo_classname_service=>get_interfacepool_name( <ls_type>-clsname ).
        ENDIF.

        inform( p_sub_obj_name = lv_include
                p_kind         = mv_errty
                p_test         = myname
                p_code         = '006'
                p_param_1      = <ls_type>-cmpname ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version  = '001'.
    position = '058'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_skip_ccau = abap_true.

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Method not referenced statically'(m01) ).

    insert_scimessage(
        iv_code = '002'
        iv_text = 'Constant &1 not referenced statically'(m02) ).

    insert_scimessage(
        iv_code = '003'
        iv_text = 'Method is only referenced locally. Should be changed to private or protected.'(m03) ).

    insert_scimessage(
        iv_code = '004'
        iv_text = 'Method &1 only implemented, not referenced statically'(m04) ).

    insert_scimessage(
        iv_code = '005'
        iv_text = '&1 not referenced statically'(m05) ).

    insert_scimessage(
        iv_code = '006'
        iv_text = 'Type &1 not referenced statically'(m06) ).

    insert_scimessage(
        iv_code = '007'
        iv_text = 'Alias not referenced statically'(m07) ).

  ENDMETHOD.


  METHOD filter_implementations.

    DATA: ls_mtdkey TYPE seocpdkey.

    FIELD-SYMBOLS: <lv_include> LIKE LINE OF ct_include.


    LOOP AT ct_include ASSIGNING <lv_include>.
      cl_oo_classname_service=>get_method_by_include(
        EXPORTING
          incname             = <lv_include>
        RECEIVING
          mtdkey              = ls_mtdkey
        EXCEPTIONS
          class_not_existing  = 1
          method_not_existing = 2
          OTHERS              = 3 ).
      IF sy-subrc = 0 AND ls_mtdkey-cpdname = |{ is_method-clsname }~{ is_method-cmpname }|.
        DELETE ct_include.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD filter_self_references.

    DATA: ls_mtdkey  TYPE seocpdkey,
          lv_pattern TYPE string,
          lt_method  TYPE TABLE OF abaptxt255.

    FIELD-SYMBOLS: <lv_include> LIKE LINE OF ct_include.


    LOOP AT ct_include ASSIGNING <lv_include>.
      cl_oo_classname_service=>get_method_by_include(
        EXPORTING
          incname             = <lv_include>
        RECEIVING
          mtdkey              = ls_mtdkey
        EXCEPTIONS
          class_not_existing  = 1
          method_not_existing = 2
          OTHERS              = 3 ).
      IF sy-subrc = 0 AND ls_mtdkey-clsname = is_method-clsname.

* check for parallel "CALLING method AT END OF TASK", which must be public
        READ REPORT <lv_include> INTO lt_method.
        IF sy-subrc = 0.
* this is not completely correct, but will work in most cases?
          lv_pattern = |CALLING { is_method-cmpname }|.
          LOOP AT lt_method TRANSPORTING NO FIELDS WHERE line CS lv_pattern.
            EXIT.
          ENDLOOP.
          IF sy-subrc = 0.
            CONTINUE.
          ENDIF.
        ENDIF.

        DELETE ct_include.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mv_skip_ccau = mv_skip_ccau
      mt_methods = mt_methods
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_skip_ccau 'Skip CCAU' 'C'.            "#EC NOTEXT
    zzaoc_fill_att mt_methods 'Methods' 'S'.                "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD is_bopf_interface.

    DATA: lv_clsname TYPE seometarel-clsname.

    IF object_type <> 'INTF'.
      RETURN.
    ENDIF.

    SELECT SINGLE clsname INTO lv_clsname FROM seometarel
      WHERE clsname = object_name
      AND refclsname = '/BOBF/IF_LIB_CONSTANTS'
      AND version = '1'
      AND state = '1'
      AND reltype = '0'.
    IF sy-subrc = 0.
      rv_boolean = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mv_skip_ccau = mv_skip_ccau
      mt_methods = mt_methods
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD report_clas.

    DATA: lv_include  TYPE programm,
          lt_includes TYPE seop_methods_w_include,
          ls_mtdkey   TYPE seocpdkey.


    IF NOT is_method-cmpname IN mt_methods.
      RETURN.
    ENDIF.

    ls_mtdkey-clsname = is_method-clsname.
    ls_mtdkey-cpdname = is_method-cmpname.

    cl_oo_classname_service=>get_method_include(
      EXPORTING
        mtdkey              = ls_mtdkey
      RECEIVING
        result              = lv_include
      EXCEPTIONS
        class_not_existing  = 1
        method_not_existing = 2
        OTHERS              = 3 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

* sometimes types are found via GET_METHOD_INCLUDE,
* so added an extra check to make sure it is a method
    cl_oo_classname_service=>get_all_method_includes(
      EXPORTING
        clsname            = is_method-clsname
      RECEIVING
        result             = lt_includes
      EXCEPTIONS
        class_not_existing = 1
        OTHERS             = 2 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    READ TABLE lt_includes WITH KEY incname = lv_include TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    inform( p_sub_obj_name = lv_include
            p_kind         = mv_errty
            p_test         = myname
            p_code         = iv_err_code ).

  ENDMETHOD.
ENDCLASS.
