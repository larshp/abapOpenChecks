REPORT zaoc_orphaned_includes.

TABLES: tadir.

TYPES: BEGIN OF ty_output,
         obj_name TYPE tadir-obj_name,
         devclass TYPE tadir-devclass,
       END OF ty_output.

SELECT-OPTIONS: s_devc FOR tadir-devclass OBLIGATORY.

START-OF-SELECTION.
  PERFORM run.

FORM run RAISING cx_salv_msg.

  DATA: lt_tadir  TYPE STANDARD TABLE OF tadir WITH DEFAULT KEY,
        lv_subc   TYPE reposrc-subc,
        lo_alv    TYPE REF TO cl_salv_table,
        lt_output TYPE STANDARD TABLE OF ty_output WITH DEFAULT KEY,
        lt_main   TYPE STANDARD TABLE OF program WITH NON-UNIQUE DEFAULT KEY.

  FIELD-SYMBOLS: <ls_output> LIKE LINE OF lt_output,
                 <ls_tadir>  LIKE LINE OF lt_tadir.

  SELECT * FROM tadir INTO TABLE lt_tadir
    WHERE devclass IN s_devc
    AND pgmid = 'R3TR'
    AND object = 'PROG'.                  "#EC CI_GENBUFF "#EC CI_SUBRC

  LOOP AT lt_tadir ASSIGNING <ls_tadir>.
    SELECT SINGLE subc INTO lv_subc FROM reposrc
      WHERE progname = <ls_tadir>-obj_name AND r3state = 'A'.
    IF sy-subrc <> 0 OR lv_subc <> 'I'.
      CONTINUE.
    ENDIF.

    CLEAR lt_main.
    CALL FUNCTION 'RS_GET_MAINPROGRAMS'
      EXPORTING
        name         = <ls_tadir>-obj_name
      TABLES
        mainprograms = lt_main
      EXCEPTIONS
        cancelled    = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.

    IF lines( lt_main ) = 0.
      APPEND INITIAL LINE TO lt_output ASSIGNING <ls_output>.
      <ls_output>-obj_name = <ls_tadir>-obj_name.
      <ls_output>-devclass = <ls_tadir>-devclass.
    ENDIF.

  ENDLOOP.

  cl_salv_table=>factory(
    IMPORTING
      r_salv_table = lo_alv
    CHANGING
      t_table      = lt_output ).

  lo_alv->get_functions( )->set_all( ).
  lo_alv->display( ).

ENDFORM.
