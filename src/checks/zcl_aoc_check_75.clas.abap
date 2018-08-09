CLASS zcl_aoc_check_75 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super_root
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS get_message_text
        REDEFINITION .
    METHODS if_ci_test~query_attributes
        REDEFINITION .
    METHODS put_attributes
        REDEFINITION .
    METHODS run
        REDEFINITION .
    METHODS get_attributes
        REDEFINITION .
  PROTECTED SECTION.

    DATA mv_language TYPE char20 .
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_75 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    version  = '001'.
    position = '075'.

    has_documentation = c_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

    mv_language = sy-langu.

    add_obj_type( 'TRAN' ).
    add_obj_type( 'PROG' ).
    add_obj_type( 'CLAS' ).
    add_obj_type( 'INTF' ).
    add_obj_type( 'FUGR' ).
    add_obj_type( 'DOMA' ).
    add_obj_type( 'DTEL' ).
    add_obj_type( 'TTYP' ).
    add_obj_type( 'TABL' ).
    add_obj_type( 'VIEW' ).
    add_obj_type( 'SSFO' ).
    add_obj_type( 'FORM' ).

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mv_language = mv_language
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Wrong master language: &1'.               "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_language 'Language' ''.               "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mv_language = mv_language
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD run.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_lang TYPE tadir-masterlang.

    SELECT SINGLE masterlang FROM tadir INTO lv_lang
      WHERE pgmid = 'R3TR'
      AND object = object_type
      AND obj_name = object_name.
    IF sy-subrc <> 0 OR lv_lang CA mv_language.
      RETURN.
    ENDIF.

    inform( p_test    = myname
            p_kind    = mv_errty
            p_param_1 = lv_lang
            p_code    = '001' ).

  ENDMETHOD.
ENDCLASS.
