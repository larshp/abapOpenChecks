CLASS zcl_aoc_check_96 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super_root
  CREATE PUBLIC .

  PUBLIC SECTION.

    CLASS-METHODS class_constructor .
    METHODS constructor .

    METHODS get_message_text
        REDEFINITION .
    METHODS run
        REDEFINITION .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_96 IMPLEMENTATION.


  METHOD class_constructor.
  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '095'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    add_obj_type( 'PROG' ).
    add_obj_type( 'FUGR' ).

  ENDMETHOD.


  METHOD get_message_text.
    CASE p_code.
      WHEN '001'.
        p_text = '&1 is with ''Editor Lock'' set.'.         "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.
  ENDMETHOD.


  METHOD run.
    DATA lv_edtx  TYPE reposrc-edtx.

    SELECT SINGLE edtx
      FROM reposrc
      INTO lv_edtx
      WHERE progname = program_name
        AND r3state = 'A'.
    IF sy-subrc = 0 AND
       lv_edtx = abap_true.
      inform( p_param_1 = program_name
          p_kind    = mv_errty
          p_test    = myname
          p_code    = '001' ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
