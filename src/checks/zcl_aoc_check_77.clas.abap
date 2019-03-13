CLASS zcl_aoc_check_77 DEFINITION
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

    DATA mv_exposure TYPE char1 .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_77 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    version  = '002'.
    position = '077'.

    has_documentation = c_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

    add_obj_type( 'CLAS' ).

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mv_exposure = mv_exposure
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Wrong instantiation level'.               "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_exposure 'Instantiation' ''.          "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mv_exposure = mv_exposure
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD run.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_exposure TYPE seoclassdf-exposure,
          lv_category TYPE seoclassdf-category.


    IF mv_exposure IS INITIAL
        OR object_type <> 'CLAS'
        OR object_name CP '*_DPC_EXT'
        OR object_name CP '*_MPC_EXT'.
      RETURN.
    ENDIF.

    SELECT SINGLE exposure category FROM seoclassdf INTO
      (lv_exposure, lv_category)
      WHERE clsname = object_name
      AND version = '1'.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF lv_category <> seoc_category_exception
        AND lv_exposure <> mv_exposure.
      inform( p_test = myname
              p_kind = mv_errty
              p_code = '001' ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
