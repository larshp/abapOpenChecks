CLASS zcl_aoc_check_90 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super_root
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS run
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_90 IMPLEMENTATION.


  METHOD constructor.

    DATA ls_scimessage TYPE scimessage.

    super->constructor( ).

    version  = '001'.
    position = '090'.

    has_documentation = abap_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    ls_scimessage-test = myname.
    ls_scimessage-code = '001'.
    ls_scimessage-kind = mv_errty.
    ls_scimessage-text = |Constructor visibility must be public|.
    INSERT ls_scimessage INTO TABLE scimessages.

    add_obj_type( 'CLAS' ).

  ENDMETHOD.


  METHOD run.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    CONSTANTS: BEGIN OF lc_exposure,
                 private   TYPE seocompodf-exposure VALUE '0',
                 protected TYPE seocompodf-exposure VALUE '1',
               END OF lc_exposure.

    DATA: lv_cmpname TYPE seocompodf-cmpname.


    IF object_type <> 'CLAS'.
      RETURN.
    ENDIF.

    SELECT SINGLE cmpname FROM seocompodf INTO lv_cmpname
      WHERE clsname = object_name
      AND cmpname = 'CONSTRUCTOR'
      AND ( exposure = lc_exposure-private
      OR exposure = lc_exposure-protected ).
    IF sy-subrc = 0.
      inform( p_test = myname
              p_code = '001' ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
