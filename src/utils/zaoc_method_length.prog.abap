REPORT zaoc_method_length.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

TABLES: tadir.

TYPES: BEGIN OF ty_result,
         cpdkey  TYPE seocpdkey,
         incname TYPE programm,
         lines   TYPE i,
       END OF ty_result.

TYPES: BEGIN OF ty_sum,
         text    TYPE text200,
         count   TYPE i,
         percent TYPE text200,
       END OF ty_sum.

DATA: gt_result TYPE STANDARD TABLE OF ty_result WITH DEFAULT KEY,
      gt_sum    TYPE STANDARD TABLE OF ty_sum WITH DEFAULT KEY.


SELECT-OPTIONS: s_devc FOR tadir-devclass,
                s_objn FOR tadir-obj_name.

PARAMETERS: p_det   TYPE c RADIOBUTTON GROUP g1 DEFAULT 'X',
            p_sum   TYPE c RADIOBUTTON GROUP g1,
            p_split TYPE i DEFAULT 15.

START-OF-SELECTION.
  PERFORM run.

CLASS lcl_alv DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      on_link_click
        FOR EVENT link_click OF cl_salv_events_table
        IMPORTING
          row
          column.

ENDCLASS.

CLASS lcl_alv IMPLEMENTATION.

  METHOD on_link_click.

    DATA: ls_result LIKE LINE OF gt_result.

    READ TABLE gt_result INDEX row INTO ls_result.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = ls_result-incname
        object_type   = 'PROG'
        in_new_window = abap_true.

  ENDMETHOD.

ENDCLASS.

FORM run RAISING cx_salv_error.

  TYPES: BEGIN OF ty_tadir,
           obj_name TYPE tadir-obj_name,
         END OF ty_tadir.

  DATA: lt_tadir    TYPE STANDARD TABLE OF ty_tadir WITH DEFAULT KEY,
        lv_class    TYPE seoclsname,
        lt_includes TYPE seop_methods_w_include,
        ls_include  LIKE LINE OF lt_includes,
        ls_result   LIKE LINE OF gt_result,
        lt_code     TYPE TABLE OF abaptxt255,
        lv_index    TYPE i,
        lv_float    TYPE f,
        ls_sum      LIKE LINE OF gt_sum,
        ls_tadir    LIKE LINE OF lt_tadir.

  FIELD-SYMBOLS: <ls_sum> LIKE LINE OF gt_sum.


  DO 1000 TIMES.
    CLEAR ls_sum.
    ls_sum-text = |{ ( sy-index - 1 ) * p_split } to { sy-index * p_split - 1 }|.
    APPEND ls_sum TO gt_sum.
  ENDDO.

  SELECT obj_name FROM tadir
    INTO TABLE lt_tadir
    WHERE devclass IN s_devc
    AND object = 'CLAS'
    AND obj_name IN s_objn
    AND delflag = abap_false
    ORDER BY obj_name ASCENDING.          "#EC CI_GENBUFF "#EC CI_SUBRC

  LOOP AT lt_tadir INTO ls_tadir.
    lv_class = ls_tadir-obj_name.
    lt_includes = cl_oo_classname_service=>get_all_method_includes( lv_class ).

    LOOP AT lt_includes INTO ls_include.
      READ REPORT ls_include-incname INTO lt_code.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING ls_include TO ls_result.
        ls_result-lines = lines( lt_code ).
        APPEND ls_result TO gt_result.

        lv_float = ( lines( lt_code ) / p_split ) + 1.
        lv_index = floor( lv_float ).
        READ TABLE gt_sum INDEX lv_index ASSIGNING <ls_sum>.
        ASSERT sy-subrc = 0.
        <ls_sum>-count = <ls_sum>-count + 1.
      ENDIF.
    ENDLOOP.
  ENDLOOP.

  PERFORM sum_percentage.

  PERFORM display.

ENDFORM.

FORM sum_percentage.

  DATA: lv_total   TYPE i,
        lv_percent TYPE p DECIMALS 2,
        ls_sum     LIKE LINE OF gt_sum.

  FIELD-SYMBOLS: <ls_sum> LIKE LINE OF gt_sum.

  LOOP AT gt_sum ASSIGNING <ls_sum>.
    lv_total = lv_total + <ls_sum>-count.
  ENDLOOP.

  LOOP AT gt_sum ASSIGNING <ls_sum>.
    lv_percent = ( <ls_sum>-count / lv_total ) * 100.
    <ls_sum>-percent = |{ lv_percent }%|.
  ENDLOOP.

  DO.
    lv_total = lines( gt_sum ).
    READ TABLE gt_sum INDEX lv_total INTO ls_sum.
    IF sy-subrc <> 0 OR ls_sum-count <> 0.
      EXIT.
    ENDIF.
    DELETE gt_sum INDEX lv_total.
  ENDDO.

ENDFORM.

FORM display RAISING cx_salv_error.

  DATA: lo_alv    TYPE REF TO cl_salv_table,
        lo_column TYPE REF TO cl_salv_column_list,
        lo_events TYPE REF TO cl_salv_events_table.

  IF p_det = abap_true.
    cl_salv_table=>factory(
      IMPORTING
        r_salv_table = lo_alv
      CHANGING
        t_table      = gt_result ).

    lo_events = lo_alv->get_event( ).
    SET HANDLER lcl_alv=>on_link_click FOR lo_events.

    lo_column ?= lo_alv->get_columns( )->get_column( 'INCNAME' ).
    lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
  ELSE.
    cl_salv_table=>factory(
      IMPORTING
        r_salv_table = lo_alv
      CHANGING
        t_table      = gt_sum ).

    lo_column ?= lo_alv->get_columns( )->get_column( 'PERCENT' ).
    lo_column->set_alignment( if_salv_c_alignment=>right ).
  ENDIF.

  lo_alv->get_functions( )->set_all( ).
  lo_alv->get_columns( )->set_optimize( ).
  lo_alv->display( ).

ENDFORM.
