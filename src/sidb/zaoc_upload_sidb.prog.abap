REPORT zaoc_upload_sidb.

PARAMETERS: p_file TYPE text255 OBLIGATORY.

START-OF-SELECTION.
  PERFORM run.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  PERFORM f4_file.

FORM f4_file.

  DATA: lt_files TYPE filetable,
        ls_file  LIKE LINE OF lt_files,
        lv_rc    TYPE i.


  cl_gui_frontend_services=>file_open_dialog(
    EXPORTING
      file_filter             = 'ZIP Files (*.ZIP)|*.ZIP|'
      multiselection          = abap_false
    CHANGING
      file_table              = lt_files
      rc                      = lv_rc
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5 ).
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  READ TABLE lt_files INDEX 1 INTO ls_file.
  IF sy-subrc = 0.
    p_file = ls_file-filename.
  ENDIF.

ENDFORM.

FORM run.

  TYPES: ty_hex200 TYPE x LENGTH 200.

  DATA: lv_filename TYPE string,
        lv_length   TYPE i,
        lv_xstr     TYPE xstring,
        lt_data     TYPE STANDARD TABLE OF ty_hex200 WITH DEFAULT KEY.


  lv_filename = p_file.

  cl_gui_frontend_services=>gui_upload(
    EXPORTING
      filename                = lv_filename
      filetype                = 'BIN'
    IMPORTING
      filelength              = lv_length
    CHANGING
      data_tab                = lt_data
    EXCEPTIONS
      file_open_error         = 1
      file_read_error         = 2
      no_batch                = 3
      gui_refuse_filetransfer = 4
      invalid_type            = 5
      no_authority            = 6
      unknown_error           = 7
      bad_data_format         = 8
      header_not_allowed      = 9
      separator_not_allowed   = 10
      header_too_long         = 11
      unknown_dp_error        = 12
      access_denied           = 13
      dp_out_of_memory        = 14
      disk_full               = 15
      dp_timeout              = 16
      not_supported_by_gui    = 17
      error_no_gui            = 18
      OTHERS                  = 19 ).
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  CONCATENATE LINES OF lt_data INTO lv_xstr IN BYTE MODE.
  lv_xstr = lv_xstr(lv_length).

  zcl_aoc_upload_sidb=>upload( lv_xstr ).

  MESSAGE s001(zabapopenchecks).

ENDFORM.
