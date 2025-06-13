FUNCTION zaoc_get_author_of_active_vers.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     VALUE(IV_PROGRAM_NAME) TYPE  PROGNAME
*"  EXPORTING
*"     VALUE(EV_AUTHOR) TYPE  CNAM
*"  EXCEPTIONS
*"      NO_ACTIVE_VERSION
*"----------------------------------------------------------------------
  SELECT SINGLE cnam
    FROM reposrc
    INTO ev_author
    WHERE progname = iv_program_name
      AND r3state  = 'A'.

  IF sy-subrc <> 0.
    RAISE no_active_version.
  ENDIF.
ENDFUNCTION.
