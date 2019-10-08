CLASS zcl_aoc_check_93 DEFINITION
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
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_93 IMPLEMENTATION.


  METHOD check.

    DATA lv_cmpname TYPE vseocompdf-cmpname.

    SELECT SINGLE cmpname
      FROM vseocompdf
      INTO lv_cmpname
      WHERE clsname = object_name
        AND cmptype = '1'
        AND version = '1'
        AND mtddecltyp = '1'.
    IF sy-subrc EQ 0.

      SELECT SINGLE cmpname
        FROM vseocompdf
        INTO lv_cmpname
        WHERE clsname = object_name
          AND cmptype = '1'
          AND version = '1'
          AND mtddecltyp = '0'.
      IF sy-subrc NE 0.
        inform( p_param_1 = object_name
                p_kind    = mv_errty
                p_test    = myname
                p_code    = '001' ).
      ENDIF.
    ENDIF.


  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '093'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    add_obj_type( 'CLAS' ).

  ENDMETHOD.


  METHOD get_message_text.
    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Class &1 has only statics methods'.       "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
