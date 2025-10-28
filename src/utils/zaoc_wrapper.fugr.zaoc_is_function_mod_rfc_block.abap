FUNCTION zaoc_is_function_mod_rfc_block.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_FUNCTION_MODULE_NAME) TYPE  FUNCNAME
*"     VALUE(IV_BLOCKLIST_PACKAGE) TYPE  DEVCLASS
*"  EXPORTING
*"     VALUE(EV_IS_ON_RFC_BLOCKLIST) TYPE  XFLAG
*"----------------------------------------------------------------------
  SELECT SINGLE @abap_true
    FROM rfc_bl_server
    WHERE blpackage = @iv_blocklist_package
      AND rfm_name  = @iv_function_module_name
    INTO @ev_is_on_rfc_blocklist.
ENDFUNCTION.
