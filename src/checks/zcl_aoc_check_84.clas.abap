CLASS zcl_aoc_check_84 DEFINITION
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



CLASS ZCL_AOC_CHECK_84 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    version  = '001'.
    position = '084'.

    has_documentation = c_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

    add_obj_type( 'INTF' ).
    add_obj_type( 'CLAS' ).

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'No public attributes, &1'.                "#EC NOTEXT
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

    TYPES: BEGIN OF ty_attr,
             cmpname TYPE vseocompdf-cmpname,
           END OF ty_attr.

    DATA: lt_attr     TYPE STANDARD TABLE OF ty_attr WITH DEFAULT KEY,
          ls_attr     LIKE LINE OF lt_attr,
          lv_category TYPE seoclassdf-category.


    SELECT cmpname FROM vseocompdf INTO CORRESPONDING FIELDS OF TABLE lt_attr
      WHERE clsname = object_name
      AND version = '1'
      AND cmptype = '0'
      AND exposure = '2'
      AND attdecltyp <> '2'.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    SELECT SINGLE category FROM seoclassdf
      INTO lv_category
      WHERE clsname = object_name
      AND version = '1'.
    IF sy-subrc = 0 AND lv_category = seoc_category_exception.
      RETURN.
    ENDIF.

    LOOP AT lt_attr INTO ls_attr.
      inform( p_test    = myname
              p_kind    = mv_errty
              p_code    = '001'
              p_param_1 = ls_attr-cmpname ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
