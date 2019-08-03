CLASS zcl_aoc_check_88 DEFINITION
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



CLASS ZCL_AOC_CHECK_88 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    version  = '001'.
    position = '088'.

    has_documentation = abap_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    add_obj_type( 'INTF' ).
    add_obj_type( 'CLAS' ).

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Remove description, &1'.                  "#EC NOTEXT
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

    DATA: lt_texts TYPE STANDARD TABLE OF seocompotx WITH DEFAULT KEY,
          ls_text  LIKE LINE OF lt_texts.


    SELECT * FROM seocompotx INTO TABLE lt_texts
      WHERE clsname = object_name
      AND langu = sy-langu
      AND descript <> ''.

    LOOP AT lt_texts INTO ls_text.
      inform( p_test    = myname
              p_kind    = mv_errty
              p_code    = '001'
              p_param_1 = ls_text-cmpname ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
