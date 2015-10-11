REPORT zaoc_clones.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

TABLES: tadir.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_devcla FOR tadir-devclass.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS: p_top TYPE i DEFAULT 100 OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b2.

*----------------------------------------------------------------------*
*       CLASS lcl_app DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_app DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      run
        RAISING cx_salv_error.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_combi,
             method1 TYPE seop_method_w_include,
             method2 TYPE seop_method_w_include,
             match   TYPE dec20_2,
           END OF ty_combi.

    TYPES: ty_combi_tt TYPE STANDARD TABLE OF ty_combi WITH DEFAULT KEY.

    CLASS-METHODS:
      compare
        IMPORTING is_combi        TYPE ty_combi
        RETURNING value(rv_match) TYPE dec20_2,
      find_methods
        RETURNING value(rt_methods) TYPE seop_methods_w_include,
      output
        IMPORTING it_combi TYPE ty_combi_tt
        RAISING   cx_salv_error,
      delta
        IMPORTING it_old          TYPE STANDARD TABLE
                  it_new          TYPE STANDARD TABLE
        RETURNING value(rt_delta) TYPE vxabapt255_tab,
      combinations
        IMPORTING it_methods      TYPE seop_methods_w_include
        RETURNING value(rt_combi) TYPE ty_combi_tt.

ENDCLASS.                    "lcl_app DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_app IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_app IMPLEMENTATION.

  METHOD output.

    DATA: lt_combi LIKE it_combi,
          lo_alv   TYPE REF TO cl_salv_table.


    lt_combi = it_combi.
    cl_salv_table=>factory(
      IMPORTING
        r_salv_table = lo_alv
      CHANGING
        t_table      = lt_combi ).

    lo_alv->get_columns( )->set_optimize( ).

    lo_alv->display( ).

  ENDMETHOD.                    "output

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

    DATA: lv_max     TYPE i,
          lt_delta   TYPE vxabapt255_tab,
          lt_method1 TYPE TABLE OF abaptxt255,
          lt_method2 TYPE TABLE OF abaptxt255.


* would be faster to cache the source code, but also take more memory
    READ REPORT is_combi-method1-incname INTO lt_method1.
    READ REPORT is_combi-method2-incname INTO lt_method2.

    lt_delta = delta( it_old = lt_method1
                      it_new = lt_method2 ).

    lv_max = lines( lt_method1 ).
    IF lines( lt_method2 ) > lv_max.
      lv_max = lines( lt_method2 ).
    ENDIF.

* this calculation might not be 100% correct but will give a good indication
    rv_match = lv_max - lines( lt_delta ).

  ENDMETHOD.                    "compare

  METHOD run.

    DATA: lt_combi   TYPE ty_combi_tt,
          lv_text    TYPE string,
          lt_methods TYPE seop_methods_w_include.

    FIELD-SYMBOLS: <ls_combi> LIKE LINE OF lt_combi.


    lt_methods = find_methods( ).
    lt_combi = combinations( lt_methods ).

    LOOP AT lt_combi ASSIGNING <ls_combi>.
      IF sy-tabix MOD 100 = 0.
        lv_text = |{ sy-tabix }/{ lines( lt_combi ) }|.
        CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
          EXPORTING
            percentage = 100
            text       = lv_text.
      ENDIF.

      <ls_combi>-match = compare( <ls_combi> ).
    ENDLOOP.

    SORT lt_combi BY match DESCENDING.
    DELETE lt_combi FROM p_top.
    output( lt_combi ).

  ENDMETHOD.                    "run

  METHOD combinations.

    DATA: lv_index TYPE i.

    FIELD-SYMBOLS: <ls_combi>   LIKE LINE OF rt_combi,
                   <ls_method1> LIKE LINE OF it_methods,
                   <ls_method2> LIKE LINE OF it_methods.


    LOOP AT it_methods ASSIGNING <ls_method1>.
      lv_index = sy-tabix + 1.

      LOOP AT it_methods ASSIGNING <ls_method2> FROM lv_index.
        APPEND INITIAL LINE TO rt_combi ASSIGNING <ls_combi>.
        <ls_combi>-method1 = <ls_method1>.
        <ls_combi>-method2 = <ls_method2>.
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
      AND object = 'CLAS'
      ORDER BY PRIMARY KEY.

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
  lcl_app=>run( ).
