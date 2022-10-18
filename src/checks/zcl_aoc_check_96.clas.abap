CLASS zcl_aoc_check_96 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super_root
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS run
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.

    METHODS is_edt_lock
      IMPORTING
        !iv_program_name TYPE programm
      RETURNING
        VALUE(rv_bool)   TYPE abap_bool .
ENDCLASS.



CLASS zcl_aoc_check_96 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '096'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    add_obj_type( 'PROG' ).
    add_obj_type( 'FUGR' ).
    add_obj_type( 'CLAS' ).

    insert_scimessage(
      iv_code = '001'
      iv_text = '''Editor Lock'' is set.'(m01) ).            "#EC NOTEX

  ENDMETHOD.


  METHOD is_edt_lock.
    DATA lv_edtx  TYPE reposrc-edtx.
    SELECT SINGLE edtx
      FROM reposrc
      INTO lv_edtx
      WHERE progname = iv_program_name
      AND r3state = 'A'.
    rv_bool = boolc( lv_edtx <> abap_false ).
  ENDMETHOD.


  METHOD run.
    DATA: lv_program_name TYPE programm,
          lt_mtdkeys      TYPE seo_cpdkeys.
    FIELD-SYMBOLS <ls_mtdkey> LIKE LINE OF lt_mtdkeys.

    IF object_type = 'CLAS'.
      SELECT clsname cmpname
        FROM vseocompdf
        INTO TABLE lt_mtdkeys
        WHERE clsname = object_name
          AND version = '1'
          AND cmptype = '1'
          ORDER BY clsname ASCENDING
                   cmpname ASCENDING.
      LOOP AT lt_mtdkeys ASSIGNING <ls_mtdkey>.
        cl_oo_classname_service=>get_method_include(
          EXPORTING
            mtdkey              = <ls_mtdkey>
          RECEIVING
            result              = lv_program_name
          EXCEPTIONS
            class_not_existing  = 1
            method_not_existing = 2
            OTHERS              = 3 ).                    "#EC CI_SUBRC

        IF is_edt_lock( lv_program_name ) = abap_true.
          inform( p_sub_obj_type = c_type_include
                  p_sub_obj_name = lv_program_name
                  p_kind         = mv_errty
                  p_test         = myname
                  p_code         = '001' ).
        ENDIF.

      ENDLOOP.
    ELSEIF is_edt_lock( program_name ) = abap_true.
      inform( p_kind = mv_errty
              p_test = myname
              p_code = '001' ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
