FUNCTION zaoc_is_function_module_rfc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_FUNCTION_MODULE_NAME) TYPE  FUNCNAME
*"  EXPORTING
*"     VALUE(EV_IS_RFC_ENABLED) TYPE  XFLAG
*"  EXCEPTIONS
*"      NOT_FOUND
*"----------------------------------------------------------------------
  CONSTANTS lc_fmode_remote TYPE fmode VALUE 'R'.
  "! Remote enabled function module and the flag "BasXML supported" is enabled
  CONSTANTS lc_fmode_remote_basxml TYPE fmode VALUE 'X'.

  DATA lv_fmode TYPE tfdir-fmode.

  SELECT SINGLE fmode
    FROM tfdir
    INTO lv_fmode
    WHERE funcname = iv_function_module_name.

  IF sy-subrc <> 0.
    RAISE not_found.
  ENDIF.

  IF lv_fmode = lc_fmode_remote
  OR lv_fmode = lc_fmode_remote_basxml.
    ev_is_rfc_enabled = abap_true.
  ENDIF.
ENDFUNCTION.
