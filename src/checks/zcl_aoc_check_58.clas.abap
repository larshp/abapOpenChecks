CLASS zcl_aoc_check_58 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS check
        REDEFINITION.
    METHODS get_message_text
        REDEFINITION.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_58 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_methods  TYPE STANDARD TABLE OF seocompodf WITH DEFAULT KEY,
          lv_name     TYPE wbcrossgt-name,
          lv_include  TYPE programm,
          lt_includes TYPE seop_methods_w_include,
          ls_mtdkey   TYPE seocpdkey.

    FIELD-SYMBOLS: <ls_method> LIKE LINE OF lt_methods.


    IF object_type <> 'CLAS' AND object_type <> 'INTF'.
      RETURN.
    ENDIF.

* only look at public and protected methods, as private are covered by standard check
    SELECT * FROM seocompodf
      INTO TABLE lt_methods
      WHERE clsname = object_name
      AND version = '1'
      AND ( exposure = '1' OR exposure = '2' )
      AND type = ''
      AND cmpname <> 'CLASS_CONSTRUCTOR'
      AND cmpname <> 'CONSTRUCTOR'
      ORDER BY PRIMARY KEY.     "#EC CI_SUBRC "#EC CI_ALL_FIELDS_NEEDED

    LOOP AT lt_methods ASSIGNING <ls_method>.
      CONCATENATE <ls_method>-clsname '\ME:' <ls_method>-cmpname INTO lv_name.
      SELECT SINGLE name FROM wbcrossgt INTO lv_name WHERE otype = 'ME' AND name = lv_name.
      IF sy-subrc <> 0.
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
                p_code         = '001' ).
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    description    = 'Method not referenced statically'.    "#EC NOTEXT
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
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
