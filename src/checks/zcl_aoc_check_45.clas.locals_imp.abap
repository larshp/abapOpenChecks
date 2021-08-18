CLASS lcl_supported IMPLEMENTATION.

  METHOD is_740sp02_supported.

    DATA: lt_itab  TYPE STANDARD TABLE OF string,
          lv_mess  TYPE string,
          lv_lin   TYPE i,
          ls_trdir TYPE trdir,
          lv_code  TYPE string,
          lv_wrd   TYPE string.

    IF gv_740sp02 <> abap_undefined.
      rv_supported = gv_740sp02.
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

    gv_740sp02 = boolc( sy-subrc = 0 ).
    rv_supported = gv_740sp02.
  ENDMETHOD.


  METHOD is_740sp08_supported.

    DATA: lt_itab  TYPE STANDARD TABLE OF string,
          lv_mess  TYPE string,
          lv_lin   TYPE i,
          ls_trdir TYPE trdir,
          lv_code  TYPE string,
          lv_wrd   TYPE string.

    IF gv_740sp08 <> abap_undefined.
      rv_supported = gv_740sp08.
      RETURN.
    ENDIF.

    lv_code = 'REPORT zfoobar.' ##NO_TEXT.
    APPEND lv_code TO lt_itab.
    lv_code = 'DATA(ok) = xsdbool( sy-subrc = 0 ).' ##NO_TEXT.
    APPEND lv_code TO lt_itab.

    ls_trdir-uccheck = abap_true.

    SYNTAX-CHECK FOR lt_itab
      MESSAGE lv_mess
      LINE lv_lin
      WORD lv_wrd
      DIRECTORY ENTRY ls_trdir.

    gv_740sp08 = boolc( sy-subrc = 0 ).
    rv_supported = gv_740sp08.
  ENDMETHOD.

ENDCLASS.
