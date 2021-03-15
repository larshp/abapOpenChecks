FUNCTION zaoc_obj_doku_get_xstring_rfc.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(LANG) TYPE  SPRAS
*"     VALUE(OBJTYPE) TYPE  LXEOBJTYPE
*"     VALUE(OBJNAME) TYPE  LXEOBJNAME
*"     VALUE(SH_EMPTY) TYPE  CHAR1 DEFAULT ''
*"  EXPORTING
*"     VALUE(HEADER) TYPE  THEAD
*"     VALUE(CONTENT) TYPE  XSTRING
*"     VALUE(PSTATUS) TYPE  LXESTATPRC
*"     VALUE(K_SINK) TYPE  LXEUNITLIN
*"----------------------------------------------------------------------

  CALL FUNCTION 'LXE_OBJ_DOKU_GET_XSTRING'
    EXPORTING
      lang     = lang
      objtype  = objtype
      objname  = objname
      sh_empty = sh_empty
    IMPORTING
      header   = header
      content  = content
      pstatus  = pstatus
      k_sink   = k_sink.

ENDFUNCTION.
