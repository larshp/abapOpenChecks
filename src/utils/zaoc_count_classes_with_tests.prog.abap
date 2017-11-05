REPORT zaoc_count_classes_with_tests.

TABLES: tadir.

SELECT-OPTIONS: s_objn FOR tadir-obj_name,
                s_devc FOR tadir-devclass.

START-OF-SELECTION.
  PERFORM run.

FORM run RAISING cx_salv_msg.

  TYPES: BEGIN OF ty_alv,
           obj_name TYPE tadir-obj_name,
           with     TYPE i,
           without  TYPE i,
         END OF ty_alv.

  TYPES: BEGIN OF ty_tadir,
           object   TYPE tadir-object,
           obj_name TYPE tadir-obj_name,
         END OF ty_tadir.

  DATA: lt_tadir TYPE STANDARD TABLE OF ty_tadir WITH DEFAULT KEY,
        lr_alv   TYPE REF TO cl_salv_table,
        lt_alv   TYPE STANDARD TABLE OF ty_alv WITH DEFAULT KEY,
        ls_alv   LIKE LINE OF lt_alv.


  SELECT object obj_name FROM tadir INTO TABLE lt_tadir
    WHERE pgmid = 'R3TR'
    AND object = 'CLAS'
    AND obj_name IN s_objn
    AND devclass IN s_devc
    AND delflag = abap_false.

  LOOP AT lt_tadir INTO DATA(ls_tadir).
    DATA(ls_aunit_info) = cl_aunit_prog_info=>get_program_info(
      allow_commit = abap_true
      obj_type     = ls_tadir-object
      obj_name     = ls_tadir-obj_name ).
    ls_alv-obj_name = ls_tadir-obj_name.
    IF ls_aunit_info-has_tests = abap_true.
      ls_alv-with = 1.
    ELSE.
      ls_alv-without = 1.
    ENDIF.
    APPEND ls_alv TO lt_alv.
  ENDLOOP.

  cl_salv_table=>factory(
    IMPORTING
      r_salv_table = lr_alv
    CHANGING
      t_table      = lt_alv ).

  lr_alv->get_functions( )->set_all( ).
  lr_alv->get_columns( )->set_optimize( ).
  lr_alv->display( ).

ENDFORM.
