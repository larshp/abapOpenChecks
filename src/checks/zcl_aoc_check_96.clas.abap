class ZCL_AOC_CHECK_96 definition
  public
  inheriting from ZCL_AOC_SUPER_ROOT
  create public .

public section.

  methods CONSTRUCTOR .

  methods GET_MESSAGE_TEXT
    redefinition .
  methods RUN
    redefinition .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_96 IMPLEMENTATION.


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
