CLASS zcl_aoc_check_83 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super_root
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS get_message_text
        REDEFINITION .
    METHODS run
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_83 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    version  = '001'.
    position = '083'.

    has_documentation = c_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

    add_obj_type( 'DOMA' ).
    add_obj_type( 'DTEL' ).
    add_obj_type( 'TABL' ).

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Unreferenced DDIC object'.                "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.


  METHOD run.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: ls_dd02l     TYPE dd02l,
          ls_dd03l     TYPE dd03l,
          ls_dd04l     TYPE dd04l,
          ls_dd25l     TYPE dd25l,
          ls_dd40l     TYPE dd40l,
          ls_edisdef   TYPE edisdef,
          ls_wbcrossgt TYPE wbcrossgt.

    CASE object_type.
      WHEN 'DOMA'.
        SELECT SINGLE * FROM dd04l INTO ls_dd04l WHERE domname = object_name.
        IF sy-subrc = 0.
          RETURN.
        ENDIF.
      WHEN 'DTEL'.
        SELECT SINGLE * FROM dd03l INTO ls_dd03l WHERE rollname = object_name.
        IF sy-subrc = 0.
          RETURN.
        ENDIF.
      WHEN 'TABL'.
        SELECT SINGLE * FROM dd02l INTO ls_dd02l WHERE tabname = object_name.
        IF sy-subrc = 0 AND ls_dd02l-tabclass = 'APPEND'.
          RETURN.
        ENDIF.
        SELECT SINGLE * FROM dd40l INTO ls_dd40l WHERE rowtype = object_name.
        IF sy-subrc = 0.
          RETURN.
        ENDIF.
        SELECT SINGLE * FROM dd03l INTO ls_dd03l
          WHERE ( fieldname = '.INCLUDE' AND precfield = object_name )
          OR ( rollname = object_name AND datatype = 'STRU' ).
        IF sy-subrc = 0.
          RETURN.
        ENDIF.
        SELECT SINGLE * FROM dd25l INTO ls_dd25l WHERE roottab = object_name.
        IF sy-subrc = 0.
          RETURN.
        ENDIF.
        SELECT SINGLE * FROM edisdef INTO ls_edisdef WHERE segtyp = object_name.
        IF sy-subrc = 0.
          RETURN.
        ENDIF.
    ENDCASE.

    SELECT SINGLE * FROM wbcrossgt
      INTO ls_wbcrossgt
      WHERE name = object_name
      AND otype = 'TY'.
    IF sy-subrc <> 0.
      inform( p_test = myname
              p_kind = mv_errty
              p_code = '001' ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
