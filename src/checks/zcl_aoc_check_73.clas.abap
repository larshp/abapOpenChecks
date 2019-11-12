CLASS zcl_aoc_check_73 DEFINITION
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



CLASS ZCL_AOC_CHECK_73 IMPLEMENTATION.


  METHOD constructor.

    super->constructor( ).

    version  = '001'.
    position = '073'.

    has_documentation = abap_true.
    has_attributes = abap_true.
    attributes_ok  = abap_true.

    add_obj_type( 'TRAN' ).

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Inconsistent'(m01) ).

  ENDMETHOD.


  METHOD run.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_tcode TYPE tstc-tcode,
          lt_list  TYPE STANDARD TABLE OF rsmp_check WITH DEFAULT KEY.


    IF object_type <> 'TRAN'.
      RETURN.
    ENDIF.

    lv_tcode = object_name.

    CALL FUNCTION 'RS_TRANSACTION_INCONSISTENCIES'
      EXPORTING
        transaction_code = lv_tcode
      TABLES
        error_list       = lt_list
      EXCEPTIONS
        object_not_found = 1
        OTHERS           = 2.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    IF lines( lt_list ) > 0.
      inform( p_test = myname
              p_kind = mv_errty
              p_code = '001' ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
