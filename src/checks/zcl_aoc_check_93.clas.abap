CLASS zcl_aoc_check_93 DEFINITION
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



CLASS ZCL_AOC_CHECK_93 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '093'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    add_obj_type( 'CLAS' ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Class &1 has only statics methods'(m01) ).

  ENDMETHOD.


  METHOD run.
    DATA lv_cmpname TYPE vseocompdf-cmpname.

    SELECT SINGLE cmpname
      FROM vseocompdf
      INTO lv_cmpname
      WHERE clsname = object_name
        AND cmptype = '1'
        AND version = '1'
        AND mtddecltyp = '1'.
    IF sy-subrc = 0.

      SELECT SINGLE cmpname
        FROM vseocompdf
        INTO lv_cmpname
        WHERE clsname = object_name
          AND cmptype = '1'
          AND version = '1'
          AND mtddecltyp = '0'.
      IF sy-subrc <> 0.
        inform( p_param_1 = object_name
                p_kind    = mv_errty
                p_test    = myname
                p_code    = '001' ).
      ENDIF.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
