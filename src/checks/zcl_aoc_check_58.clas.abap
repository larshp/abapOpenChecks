CLASS zcl_aoc_check_58 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
    METHODS get_message_text
        REDEFINITION .
  PROTECTED SECTION.
    TYPES:
      BEGIN OF gty_reference_s,
        clsname TYPE seocompodf-clsname,
        cmpname TYPE seocompodf-cmpname,
      END OF gty_reference_s.
    TYPES gty_reference_t TYPE STANDARD TABLE OF gty_reference_s WITH EMPTY KEY.
    TYPES gty_include_t TYPE STANDARD TABLE OF wbcrossgt-include WITH EMPTY KEY.

    METHODS is_bopf_interface
      RETURNING
        VALUE(rv_boolean) TYPE abap_bool .
    METHODS check_constants .
    METHODS check_methods .
    METHODS filter_self_references
      IMPORTING
        iv_clsname TYPE gty_reference_s-clsname
      CHANGING
        ct_include TYPE gty_include_t.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aoc_check_58 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    IF object_type <> 'CLAS' AND object_type <> 'INTF'.
      RETURN.
    ENDIF.

    check_methods( ).
    check_constants( ).

  ENDMETHOD.


  METHOD check_constants.

    DATA: lv_name      TYPE wbcrossgt-name,
          lv_include   TYPE programm,
          lt_constants TYPE gty_reference_t.

    FIELD-SYMBOLS: <ls_constant> LIKE LINE OF lt_constants.


    IF is_bopf_interface( ) = abap_true.
      RETURN.
    ENDIF.

    SELECT clsname cmpname FROM seocompodf
      INTO TABLE lt_constants
      WHERE clsname = object_name
      AND version = '1'
      AND exposure = '2'
      AND attdecltyp = '2'
      ORDER BY PRIMARY KEY.

    LOOP AT lt_constants ASSIGNING <ls_constant>.
      CONCATENATE <ls_constant>-clsname '\DA:' <ls_constant>-cmpname INTO lv_name.
      SELECT SINGLE name FROM wbcrossgt INTO lv_name WHERE otype = 'DA' AND name = lv_name ##WARN_OK.
      IF sy-subrc <> 0.
        IF object_type = 'CLAS'.
          lv_include = cl_oo_classname_service=>get_pubsec_name( <ls_constant>-clsname ).
        ELSE.
          lv_include = cl_oo_classname_service=>get_interfacepool_name( <ls_constant>-clsname ).
        ENDIF.

        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = lv_include
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
          lt_ref_include TYPE gty_include_t,
          lv_err_code    TYPE sci_errc,
          lv_include     TYPE programm,
          lt_includes    TYPE seop_methods_w_include,
          ls_mtdkey      TYPE seocpdkey.

    FIELD-SYMBOLS: <ls_method> LIKE LINE OF lt_methods.


* only look at public and protected methods, as private are covered by standard check
    SELECT clsname cmpname FROM seocompodf
      INTO TABLE lt_methods
      WHERE clsname = object_name
      AND version = '1'
      AND ( exposure = '1' OR exposure = '2' )
      AND type = ''
      AND cmpname <> 'CLASS_CONSTRUCTOR'
      AND cmpname <> 'CONSTRUCTOR'
      ORDER BY PRIMARY KEY.     "#EC CI_SUBRC "#EC CI_ALL_FIELDS_NEEDED

    LOOP AT lt_methods ASSIGNING <ls_method>.
      CLEAR lv_err_code.

      CONCATENATE <ls_method>-clsname '\ME:' <ls_method>-cmpname INTO lv_name.
      SELECT include FROM wbcrossgt INTO TABLE lt_ref_include WHERE otype = 'ME' AND name = lv_name.
      IF sy-subrc <> 0.
        lv_err_code = '001'.
      ELSE.
        me->filter_self_references(
          EXPORTING
            iv_clsname = <ls_method>-clsname
          CHANGING
            ct_include = lt_ref_include ).

        IF lines( lt_ref_include ) = 0.
          lv_err_code = '003'.
        ENDIF.
      ENDIF.

      IF lv_err_code IS NOT INITIAL.
        ls_mtdkey-clsname = <ls_method>-clsname.
        ls_mtdkey-cpdname = <ls_method>-cmpname.

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
          CONTINUE.
        ENDIF.

* sometimes types are found via GET_METHOD_INCLUDE,
* so added an extra check to make sure it is a method
        cl_oo_classname_service=>get_all_method_includes(
          EXPORTING
            clsname            = <ls_method>-clsname
          RECEIVING
            result             = lt_includes
          EXCEPTIONS
            class_not_existing = 1
            OTHERS             = 2 ).
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        READ TABLE lt_includes WITH KEY incname = lv_include TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          CONTINUE.
        ENDIF.

        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = lv_include
                p_kind         = mv_errty
                p_test         = myname
                p_code         = lv_err_code ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    description    = 'Method or constant not referenced statically'. "#EC NOTEXT
    category       = 'ZCL_AOC_CATEGORY'.
    version        = '001'.
    position       = '058'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Method not referenced statically'.        "#EC NOTEXT
      WHEN '002'.
        p_text = 'Constant &1 not referenced statically'.   "#EC NOTEXT
      WHEN '003'.
        p_text = 'Method is only referenced locally. Should be changed to private or protected.'. "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

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


  METHOD filter_self_references.

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
      IF sy-subrc = 0 AND ls_mtdkey-clsname = iv_clsname.
        DELETE ct_include.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
