

CLASS lcl_supported DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS: support_740sp02
      RETURNING
        VALUE(rv_supported) TYPE abap_bool.

  PRIVATE SECTION.
    CLASS-DATA gv_executed TYPE abap_bool.
    CLASS-DATA gv_740sp02 TYPE abap_bool.
    CLASS-METHODS: find_supported.

ENDCLASS.

CLASS lcl_supported IMPLEMENTATION.

  METHOD support_740sp02.

    find_supported( ).
    rv_supported = gv_740sp02.

  ENDMETHOD.

  METHOD find_supported.

    DATA: lt_itab  TYPE STANDARD TABLE OF string,
          lv_mess  TYPE string,
          lv_lin   TYPE i,
          ls_trdir TYPE trdir,
          lv_code  TYPE string,
          lv_wrd   TYPE string.


    IF gv_executed = abap_true.
      RETURN.
    ENDIF.

    lv_code = 'REPORT zfoobar.' ##NO_TEXT.
    APPEND lv_code TO lt_itab.
    lv_code = 'DATA(lo_new) = NEW cl_gui_frontend_services( ).' ##NO_TEXT.
    APPEND lv_code TO lt_itab.

    ls_trdir-uccheck = abap_true.

    SYNTAX-CHECK FOR lt_itab
      MESSAGE lv_mess
      LINE lv_lin
      WORD lv_wrd
      DIRECTORY ENTRY ls_trdir.
    IF sy-subrc = 0.
* all supported in 740SP02
      gv_740sp02 = abap_true.
    ENDIF.

    gv_executed = abap_true.

  ENDMETHOD.

ENDCLASS.
