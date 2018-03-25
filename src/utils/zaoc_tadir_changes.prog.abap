REPORT zaoc_tadir_changes.
* show who/when package was changed for an object

CONSTANTS: c_utf16be TYPE abap_encoding VALUE '4102',
           c_utf16le TYPE abap_encoding VALUE '4103'.

PARAMETERS: p_obj  TYPE tadir-object OBLIGATORY,
            p_nam  TYPE tadir-obj_name OBLIGATORY,
            p_days TYPE i DEFAULT 100 OBLIGATORY.

TYPES: BEGIN OF ty_output,
         object   TYPE tadir-object,
         obj_name TYPE tadir-obj_name,
         logdate  TYPE dbtablog-logdate,
         logtime  TYPE dbtablog-logtime,
         username TYPE dbtablog-username,
         devclass TYPE tadir-devclass,
       END OF ty_output.

DATA: gt_output TYPE STANDARD TABLE OF ty_output WITH DEFAULT KEY.

START-OF-SELECTION.
  PERFORM run.

FORM run RAISING cx_salv_msg.

  DATA: lt_objects TYPE stprt_h_tablist,
        ls_object  LIKE LINE OF lt_objects,
        lv_from    TYPE d,
        lt_keys    TYPE stprsokey,
        lv_string  TYPE string,
        ls_key     LIKE LINE OF lt_keys,
        lt_log     TYPE stprt_log_stable_type.


  lv_from = sy-datum - p_days.

  ls_object-tab = 'TADIR'.
  INSERT ls_object INTO TABLE lt_objects.

  ls_key-sign   = 'I'.
  ls_key-option = 'EQ'.
  CONCATENATE 'R3TR' p_obj p_nam INTO ls_key-low.
  INSERT ls_key INTO TABLE lt_keys.

  CALL FUNCTION 'DBLOG_READ_TABLE'
    EXPORTING
      from_day             = lv_from
      to_day               = sy-datum
      obj_list             = lt_objects
      log_keys             = lt_keys
    CHANGING
      log_list             = lt_log
    EXCEPTIONS
      archive_access_error = 1
      no_archives_found    = 2
      OTHERS               = 3.
  IF sy-subrc <> 0.
    WRITE: / 'Error,', sy-subrc.
    RETURN.
  ENDIF.

  LOOP AT lt_log ASSIGNING FIELD-SYMBOL(<ls_log>).

    DATA(loc_conv) = cl_abap_conv_in_ce=>create(
        input       = <ls_log>-logdata
        encoding    = c_utf16be
        replacement = '?'
        ignore_cerr = abap_true ).

    TRY.
        loc_conv->read( IMPORTING data = lv_string ).
      CATCH cx_sy_conversion_codepage
          cx_sy_codepage_converter_init
          cx_parameter_invalid_type
          cx_parameter_invalid_range.
        ASSERT 0 = 1.
    ENDTRY.

    APPEND VALUE #(
      object   = p_obj
      obj_name = p_nam
      logdate  = <ls_log>-logdate
      logtime  = <ls_log>-logtime
      username = <ls_log>-username
      devclass = lv_string+81(30) ) TO gt_output.

  ENDLOOP.

  SELECT SINGLE devclass
    FROM tadir INTO @DATA(lv_current)
    WHERE pgmid = 'R3TR'
    AND object = @p_obj
    AND obj_name = @p_nam.

  APPEND VALUE #(
    devclass = lv_current ) TO gt_output.

  PERFORM show.

ENDFORM.

FORM show RAISING cx_salv_msg.

  cl_salv_table=>factory(
    IMPORTING
      r_salv_table = DATA(lo_alv)
    CHANGING
      t_table      = gt_output ).

  lo_alv->get_columns( )->set_optimize( ).

  lo_alv->get_functions( )->set_all( ).

  lo_alv->display( ).

ENDFORM.
