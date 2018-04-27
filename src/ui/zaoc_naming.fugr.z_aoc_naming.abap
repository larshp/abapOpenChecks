FUNCTION z_aoc_naming.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_READ_ONLY) TYPE  ABAP_BOOL OPTIONAL
*"  CHANGING
*"     REFERENCE(CS_DATA) TYPE  ZAOC_NAMING
*"----------------------------------------------------------------------

  lcl_screen2000=>initialize(
    iv_read_only = iv_read_only
    is_data      = cs_data ).

  CALL SELECTION-SCREEN 2000 STARTING AT 1 1.

  IF lcl_screen2000=>gv_cancel = abap_false
      AND iv_read_only = abap_false.
    cs_data = lcl_screen2000=>get_data( ).
  ENDIF.

ENDFUNCTION.
