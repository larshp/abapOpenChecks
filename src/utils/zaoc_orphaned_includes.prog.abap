REPORT zaoc_orphaned_includes.

TABLES: tadir.

SELECT-OPTIONS: s_devc FOR tadir-devclass OBLIGATORY.

START-OF-SELECTION.
  PERFORM run.

FORM run.

  DATA: lt_tadir TYPE STANDARD TABLE OF tadir WITH DEFAULT KEY,
        lv_subc  TYPE reposrc-subc,
        lt_main  TYPE STANDARD TABLE OF program WITH NON-UNIQUE DEFAULT KEY.


  SELECT * FROM tadir INTO TABLE lt_tadir
    WHERE devclass IN s_devc
    AND pgmid = 'R3TR'
    AND object = 'PROG'.

  LOOP AT lt_tadir ASSIGNING FIELD-SYMBOL(<ls_tadir>).
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
      WRITE: / <ls_tadir>-obj_name.
    ENDIF.

  ENDLOOP.

  WRITE: / 'Done'.

ENDFORM.
