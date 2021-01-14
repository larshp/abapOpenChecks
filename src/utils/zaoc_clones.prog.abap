REPORT zaoc_clones.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

TABLES: tadir.

TYPES: BEGIN OF ty_combi,
         method1 TYPE seop_method_w_include,
         method2 TYPE seop_method_w_include,
         match   TYPE i,
       END OF ty_combi.

TYPES: ty_combi_tt TYPE STANDARD TABLE OF ty_combi WITH DEFAULT KEY.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_devcla FOR tadir-devclass,
                s_name   FOR tadir-obj_name.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_top  TYPE i DEFAULT 100 OBLIGATORY,
            p_prog TYPE i DEFAULT 1000 OBLIGATORY,
            p_igno TYPE i DEFAULT 10 OBLIGATORY,
            p_incl TYPE i DEFAULT 5 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
*       CLASS lcl_gui DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      output
        IMPORTING it_combi TYPE ty_combi_tt
        RAISING   cx_salv_error.

  PRIVATE SECTION.
    CLASS-DATA:
      gt_combi TYPE ty_combi_tt.

    CLASS-METHODS:
      show
        IMPORTING iv_program TYPE programm,
      on_link_click
          FOR EVENT link_click OF cl_salv_events_table
        IMPORTING
          row
          column.

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_gui IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv IMPLEMENTATION.

  METHOD show.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation     = 'SHOW'
        object_name   = iv_program
        object_type   = 'PROG'
        in_new_window = abap_true.

  ENDMETHOD.

  METHOD on_link_click.

    DATA: ls_combi LIKE LINE OF gt_combi.


    READ TABLE gt_combi INTO ls_combi INDEX row.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CASE column.
      WHEN 'METHOD1-INCNAME'.
        show( ls_combi-method1-incname ).
      WHEN 'METHOD2-INCNAME'.
        show( ls_combi-method2-incname ).
    ENDCASE.

  ENDMETHOD.

  METHOD output.

    DATA: lo_column TYPE REF TO cl_salv_column_list,
          lo_events TYPE REF TO cl_salv_events_table,
          lo_alv    TYPE REF TO cl_salv_table.


    gt_combi = it_combi.
    cl_salv_table=>factory(
      IMPORTING
        r_salv_table = lo_alv
      CHANGING
        t_table      = gt_combi ).

    lo_alv->get_columns( )->set_optimize( ).
    lo_column ?= lo_alv->get_columns( )->get_column( 'METHOD1-INCNAME' ).
    lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).
    lo_column ?= lo_alv->get_columns( )->get_column( 'METHOD2-INCNAME' ).
    lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

    lo_alv->get_functions( )->set_all( ).

    lo_events = lo_alv->get_event( ).
    SET HANDLER on_link_click FOR lo_events.

    lo_alv->display( ).

  ENDMETHOD.                    "output

ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_app DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_data DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      fetch
        RETURNING VALUE(rt_combi) TYPE ty_combi_tt
        RAISING   cx_salv_error.

  PRIVATE SECTION.
    CLASS-DATA:
      BEGIN OF gs_previous,
        include TYPE programm,
        method  TYPE TABLE OF abaptxt255,
      END OF gs_previous.

    CLASS-METHODS:
      compare
        IMPORTING iv_include1     TYPE programm
                  iv_include2     TYPE programm
        RETURNING VALUE(rv_match) TYPE i,
      find_methods
        RETURNING VALUE(rt_methods) TYPE seop_methods_w_include,
      delta
        IMPORTING it_old          TYPE STANDARD TABLE
                  it_new          TYPE STANDARD TABLE
        RETURNING VALUE(rt_delta) TYPE vxabapt255_tab,
      remove_short
        CHANGING ct_methods TYPE seop_methods_w_include,
      analyze
        IMPORTING it_methods      TYPE seop_methods_w_include
        RETURNING VALUE(rt_combi) TYPE ty_combi_tt.

ENDCLASS.                    "lcl_app DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_app IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_data IMPLEMENTATION.

  METHOD remove_short.

    DATA: lv_index  TYPE i,
          lv_total  TYPE i,
          lv_count  TYPE i,
          lt_method TYPE TABLE OF abaptxt255.

    FIELD-SYMBOLS: <ls_method> LIKE LINE OF ct_methods.


    lv_total = lines( ct_methods ).
    lv_count = 0.

    LOOP AT ct_methods ASSIGNING <ls_method>.
      lv_index = sy-tabix.

      IF lv_count MOD p_prog = 0.
        cl_progress_indicator=>progress_indicate(
          i_text               = |Removing short includes, { lv_count }/{ lv_total }|
          i_processed          = lv_count
          i_total              = lv_total
          i_output_immediately = abap_true ).
      ENDIF.
      lv_count = lv_count + 1.

      READ REPORT <ls_method>-incname INTO lt_method.
      DELETE lt_method WHERE line = space.

      IF lines( lt_method ) < p_incl.
        DELETE ct_methods INDEX lv_index.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD delta.

    DATA: lt_trdirtab_old TYPE TABLE OF trdir,
          lt_trdirtab_new TYPE TABLE OF trdir,
          lt_trdir_delta  TYPE TABLE OF xtrdir.


    CALL FUNCTION 'SVRS_COMPUTE_DELTA_REPS'
      EXPORTING
        ignore_case_differences = abap_true
      TABLES
        texttab_old             = it_old
        texttab_new             = it_new
        trdirtab_old            = lt_trdirtab_old
        trdirtab_new            = lt_trdirtab_new
        trdir_delta             = lt_trdir_delta
        text_delta              = rt_delta.

  ENDMETHOD.                    "delta

  METHOD compare.

    DATA: lv_lines   TYPE i,
          lt_delta   TYPE vxabapt255_tab,
          lt_method1 TYPE TABLE OF abaptxt255,
          lt_method2 TYPE TABLE OF abaptxt255.


    IF iv_include1 <> gs_previous-include.
      READ REPORT iv_include1 INTO gs_previous-method.
      DELETE gs_previous-method WHERE line = space.
      gs_previous-include = iv_include1.
    ENDIF.
    lt_method1 = gs_previous-method.

    READ REPORT iv_include2 INTO lt_method2.
    DELETE lt_method2 WHERE line = space.

    IF lines( lt_method1 ) < lines( lt_method2 ).
      lv_lines = lines( lt_method1 ).
      lt_delta = delta( it_old = lt_method2
                        it_new = lt_method1 ).
    ELSE.
      lv_lines = lines( lt_method2 ).
      lt_delta = delta( it_old = lt_method1
                        it_new = lt_method2 ).
    ENDIF.

    rv_match = lv_lines - lines( lt_delta ).
    IF rv_match < 0.
      rv_match = 0.
    ENDIF.

  ENDMETHOD.                    "compare

  METHOD fetch.

    DATA: lt_methods TYPE seop_methods_w_include.


    lt_methods = find_methods( ).
    remove_short( CHANGING ct_methods = lt_methods ).
    rt_combi = analyze( lt_methods ).

    SORT rt_combi BY match DESCENDING.
    DELETE rt_combi FROM p_top TO lines( rt_combi ).

  ENDMETHOD.                    "run

  METHOD analyze.

    DATA: lv_index TYPE i,
          lv_count TYPE i,
          ls_combi LIKE LINE OF rt_combi,
          lv_total TYPE i.

    FIELD-SYMBOLS: <ls_method1> LIKE LINE OF it_methods,
                   <ls_method2> LIKE LINE OF it_methods.


    lv_count = lines( it_methods ) - 1.
    WHILE lv_count > 0.
      lv_total = lv_total + lv_count.
      lv_count = lv_count - 1.
    ENDWHILE.

    lv_count = 1.

    LOOP AT it_methods ASSIGNING <ls_method1>.

      lv_index = sy-tabix + 1.

      LOOP AT it_methods ASSIGNING <ls_method2> FROM lv_index.

        IF lv_count MOD p_prog = 0.
          cl_progress_indicator=>progress_indicate(
            i_text               = |Processing, { lv_count }/{ lv_total }|
            i_processed          = lv_count
            i_total              = lv_total
            i_output_immediately = abap_true ).
        ENDIF.
        lv_count = lv_count + 1.

        CLEAR ls_combi.
        ls_combi-method1 = <ls_method1>.
        ls_combi-method2 = <ls_method2>.
        ls_combi-match = compare(
          iv_include1 = <ls_method1>-incname
          iv_include2 = <ls_method2>-incname ).

        IF ls_combi-match >= p_igno.
          APPEND ls_combi TO rt_combi.
        ENDIF.

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.                    "combinations

  METHOD find_methods.

    TYPES: BEGIN OF ty_tadir,
             obj_name TYPE tadir-obj_name,
           END OF ty_tadir.

    DATA: lv_clsname TYPE seoclsname,
          lt_methods TYPE seop_methods_w_include,
          lt_tadir   TYPE TABLE OF ty_tadir.

    FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF lt_tadir.


    SELECT obj_name FROM tadir
      INTO TABLE lt_tadir
      WHERE devclass IN s_devcla
      AND obj_name IN s_name
      AND object = 'CLAS'
      AND delflag = abap_false
      ORDER BY PRIMARY KEY.               "#EC CI_GENBUFF "#EC CI_SUBRC

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      lv_clsname = <ls_tadir>-obj_name.
      cl_oo_classname_service=>get_all_method_includes(
        EXPORTING
          clsname            = lv_clsname
        RECEIVING
          result             = lt_methods
        EXCEPTIONS
          class_not_existing = 1 ).
      IF sy-subrc = 0.
        APPEND LINES OF lt_methods TO rt_methods.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "find_methods

ENDCLASS.                    "lcl_app IMPLEMENTATION

START-OF-SELECTION.
  lcl_alv=>output( lcl_data=>fetch( ) ).
