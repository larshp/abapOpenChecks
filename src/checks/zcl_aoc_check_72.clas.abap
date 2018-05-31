CLASS zcl_aoc_check_72 DEFINITION
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



CLASS ZCL_AOC_CHECK_72 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    version  = '001'.
    position = '072'.

    has_documentation = c_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

    add_obj_type( 'TABL' ).

  ENDMETHOD.                    "CONSTRUCTOR


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Enhancement category missing'.            "#EC NOTEXT
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

    DATA: lv_exclass TYPE dd02l-exclass.

    IF object_type <> 'TABL'.
      RETURN.
    ENDIF.

    SELECT SINGLE exclass FROM dd02l INTO lv_exclass
      WHERE tabname = object_name
      AND as4local = 'A'
      AND as4vers = '0000'.

    IF sy-subrc = 0 AND lv_exclass = '0'.
      inform( p_test = myname
              p_kind = mv_errty
              p_code = '001' ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
