CLASS zcl_aoc_check_88 DEFINITION
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



CLASS zcl_aoc_check_88 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    version  = '001'.
    position = '088'.

    has_documentation = abap_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    add_obj_type( 'INTF' ).
    add_obj_type( 'CLAS' ).

    insert_scimessage(
      iv_code = '001'
      iv_text = 'Remove description, &1'(m01) ).

  ENDMETHOD.


  METHOD run.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_texts TYPE STANDARD TABLE OF seocompotx WITH DEFAULT KEY,
          ls_text  LIKE LINE OF lt_texts.


    SELECT * FROM seocompotx INTO TABLE lt_texts
      WHERE clsname = object_name
      AND langu = sy-langu
      AND descript <> ''
      ORDER BY PRIMARY KEY.

    LOOP AT lt_texts INTO ls_text.
      inform( p_test    = myname
              p_kind    = mv_errty
              p_code    = '001'
              p_param_1 = ls_text-cmpname ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
