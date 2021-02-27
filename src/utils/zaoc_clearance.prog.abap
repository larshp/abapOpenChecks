REPORT zaoc_clearance.
* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

* Finds objects that are not statically referenced

TABLES: tadir.

TYPES: BEGIN OF ty_output,
         object     TYPE tadir-object,
         obj_name   TYPE tadir-obj_name,
         devclass   TYPE tadir-devclass,
         created_on TYPE tadir-created_on,
       END OF ty_output.

TYPES: ty_output_tt TYPE STANDARD TABLE OF ty_output WITH DEFAULT KEY.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS: s_devcla FOR tadir-devclass OBLIGATORY,
                s_name   FOR tadir-obj_name,
                s_type   FOR tadir-object,
                s_cdat   FOR tadir-created_on.
SELECTION-SCREEN END OF BLOCK b1.

*----------------------------------------------------------------------*
*       CLASS lcl_app DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_data DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      fetch
        RETURNING VALUE(rt_data) TYPE ty_output_tt.

  PRIVATE SECTION.
    CLASS-METHODS:
      show_progress
        IMPORTING
          iv_current TYPE i
          iv_total   TYPE i,
      check_doma
        IMPORTING
          is_tadir           TYPE ty_output
        RETURNING
          VALUE(rv_obsolete) TYPE abap_bool,
      check_dtel
        IMPORTING
          is_tadir           TYPE ty_output
        RETURNING
          VALUE(rv_obsolete) TYPE abap_bool,
      check_ttyp
        IMPORTING
          is_tadir           TYPE ty_output
        RETURNING
          VALUE(rv_obsolete) TYPE abap_bool,
      check_tabl
        IMPORTING
          is_tadir           TYPE ty_output
        RETURNING
          VALUE(rv_obsolete) TYPE abap_bool,
      check_clas
        IMPORTING
          is_tadir           TYPE ty_output
        RETURNING
          VALUE(rv_obsolete) TYPE abap_bool,
      check_intf
        IMPORTING
          is_tadir           TYPE ty_output
        RETURNING
          VALUE(rv_obsolete) TYPE abap_bool.

ENDCLASS.                    "lcl_app DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_app IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_data IMPLEMENTATION.

  METHOD show_progress.

    DATA: lv_percentage TYPE i,
          lv_text       TYPE string.


    lv_percentage = ( iv_current * 100 ) / iv_total.

    lv_text = |{ iv_current }/{ iv_total }|.
    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = lv_percentage
        text       = lv_text.

  ENDMETHOD.

  METHOD fetch.

    DATA: lt_tadir    TYPE ty_output_tt,
          lv_lines    TYPE i,
          lv_obsolete TYPE abap_bool.

    FIELD-SYMBOLS: <ls_data>  LIKE LINE OF rt_data,
                   <ls_tadir> LIKE LINE OF lt_tadir.


    SELECT object obj_name devclass created_on
      FROM tadir
      INTO TABLE lt_tadir
      WHERE pgmid = 'R3TR'
      AND ( object = 'DOMA'
      OR object = 'TABL'
      OR object = 'INTF'
      OR object = 'TTYP'
      OR object = 'CLAS'
      OR object = 'DTEL' )
      AND object IN s_type
      AND obj_name IN s_name
      AND created_on IN s_cdat
      AND delflag = abap_false
      AND devclass IN s_devcla.           "#EC CI_SUBRC "#EC CI_GENBUFF

    lv_lines = lines( lt_tadir ).

    LOOP AT lt_tadir ASSIGNING <ls_tadir>.
      IF sy-tabix MOD 50 = 0 OR sy-tabix = 1.
        show_progress( iv_current = sy-tabix
                       iv_total   = lv_lines ).
      ENDIF.
      CASE <ls_tadir>-object.
        WHEN 'DOMA'.
          lv_obsolete = check_doma( <ls_tadir> ).
        WHEN 'DTEL'.
          lv_obsolete = check_dtel( <ls_tadir> ).
        WHEN 'TABL'.
          lv_obsolete = check_tabl( <ls_tadir> ).
        WHEN 'INTF'.
          lv_obsolete = check_intf( <ls_tadir> ).
        WHEN 'CLAS'.
          lv_obsolete = check_clas( <ls_tadir> ).
        WHEN 'TTYP'.
          lv_obsolete = check_ttyp( <ls_tadir> ).
        WHEN OTHERS.
          ASSERT 1 = 0.
      ENDCASE.
      IF lv_obsolete = abap_true.
        APPEND INITIAL LINE TO rt_data ASSIGNING <ls_data>.
        MOVE-CORRESPONDING <ls_tadir> TO <ls_data>.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "run

  METHOD check_clas.

    DATA: lv_name TYPE wbcrossgt-name.

    SELECT SINGLE name FROM wbcrossgt
      INTO lv_name
      WHERE otype = 'TY'
      AND name = is_tadir-obj_name
      AND direct = 'X'.
    IF sy-subrc <> 0.
      rv_obsolete = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD check_intf.

    DATA: lv_found   TYPE wbcrossgt-include,
          lv_clsname TYPE seoclsname,
          lv_include TYPE program.


    lv_clsname = is_tadir-obj_name.
    lv_include = cl_oo_classname_service=>get_intfsec_name( lv_clsname ).

    SELECT SINGLE include FROM wbcrossgt INTO lv_found
      WHERE otype = 'TY'
      AND name = is_tadir-obj_name
      AND include <> lv_include.
    IF sy-subrc <> 0.
      rv_obsolete = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD check_ttyp.

    DATA: lt_find TYPE TABLE OF rsfind,
          ls_find LIKE LINE OF lt_find.


    ls_find-object = is_tadir-obj_name.
    APPEND ls_find TO lt_find.

    CALL FUNCTION 'RS_EU_CROSSREF'
      EXPORTING
        i_find_obj_cls           = 'DA'
        rekursiv                 = abap_true
        no_dialog                = abap_true
      TABLES
        i_findstrings            = lt_find
      EXCEPTIONS
        not_executed             = 1
        not_found                = 2
        illegal_object           = 3
        no_cross_for_this_object = 4
        batch                    = 5
        batchjob_error           = 6
        wrong_type               = 7
        object_not_exist         = 8
        OTHERS                   = 9.
    IF sy-subrc = 2.
      rv_obsolete = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD check_tabl.

    DATA: lt_find     TYPE TABLE OF rsfind,
          lv_tabclass TYPE dd02l-tabclass,
          ls_find     LIKE LINE OF lt_find.


    SELECT SINGLE tabclass FROM dd02l INTO lv_tabclass
      WHERE tabname = is_tadir-obj_name
      AND as4local = 'A'
      AND as4vers = '000'.                                "#EC CI_SUBRC
    IF lv_tabclass = 'APPEND'.
      RETURN.
    ENDIF.

    ls_find-object = is_tadir-obj_name.
    APPEND ls_find TO lt_find.

    CALL FUNCTION 'RS_EU_CROSSREF'
      EXPORTING
        i_find_obj_cls           = 'DS'
        rekursiv                 = abap_true
        no_dialog                = abap_true
      TABLES
        i_findstrings            = lt_find
      EXCEPTIONS
        not_executed             = 1
        not_found                = 2
        illegal_object           = 3
        no_cross_for_this_object = 4
        batch                    = 5
        batchjob_error           = 6
        wrong_type               = 7
        object_not_exist         = 8
        OTHERS                   = 9.
    IF sy-subrc = 2.
      rv_obsolete = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD check_dtel.

    DATA: lt_find TYPE TABLE OF rsfind,
          ls_find LIKE LINE OF lt_find.


    ls_find-object = is_tadir-obj_name.
    APPEND ls_find TO lt_find.

    CALL FUNCTION 'RS_EU_CROSSREF'
      EXPORTING
        i_find_obj_cls           = 'DE'
        rekursiv                 = abap_true
        no_dialog                = abap_true
      TABLES
        i_findstrings            = lt_find
      EXCEPTIONS
        not_executed             = 1
        not_found                = 2
        illegal_object           = 3
        no_cross_for_this_object = 4
        batch                    = 5
        batchjob_error           = 6
        wrong_type               = 7
        object_not_exist         = 8
        OTHERS                   = 9.
    IF sy-subrc = 2.
      rv_obsolete = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD check_doma.

    DATA: lv_domname  TYPE dd01l-domname,
          lv_rollname TYPE dd04l-rollname.


    lv_domname = is_tadir-obj_name.
    SELECT SINGLE rollname FROM dd04l INTO lv_rollname
      WHERE domname = lv_domname
      AND ( refkind = '' OR refkind = 'D' )
      AND as4local = 'A' ##WARN_OK.
    IF sy-subrc = 0.
      RETURN. " in use
    ENDIF.

    SELECT SINGLE domname FROM dd01l INTO lv_domname
      WHERE domname = lv_domname
      AND as4local = 'A'
      AND as4vers = '0000'.
    IF sy-subrc <> 0.
      RETURN. " does not exist
    ENDIF.

    rv_obsolete = abap_true.

  ENDMETHOD.

ENDCLASS.                    "lcl_app IMPLEMENTATION

*----------------------------------------------------------------------*
*       CLASS lcl_alv DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      show
        IMPORTING it_table TYPE ty_output_tt
        RAISING   cx_salv_error.

  PRIVATE SECTION.
    CLASS-DATA:
      gt_table TYPE ty_output_tt.

    CLASS-METHODS:
      navigate
        IMPORTING
          is_data TYPE ty_output,
      on_link_click
          FOR EVENT link_click OF cl_salv_events_table
        IMPORTING
          row
          column.

ENDCLASS.                    "lcl_alv DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_alv IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_alv IMPLEMENTATION.

  METHOD show.

    DATA: lo_column TYPE REF TO cl_salv_column_list,
          lo_events TYPE REF TO cl_salv_events_table,
          lo_alv    TYPE REF TO cl_salv_table.


    gt_table = it_table.
    cl_salv_table=>factory(
      IMPORTING
        r_salv_table = lo_alv
      CHANGING
        t_table      = gt_table ).

    lo_alv->get_columns( )->set_optimize( ).
    lo_column ?= lo_alv->get_columns( )->get_column( 'OBJ_NAME' ).
    lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

    lo_alv->get_functions( )->set_all( ).

    lo_events = lo_alv->get_event( ).
    SET HANDLER on_link_click FOR lo_events.
    lo_alv->display( ).

  ENDMETHOD.                    "show

  METHOD on_link_click.

    DATA: ls_data LIKE LINE OF gt_table.


    READ TABLE gt_table INTO ls_data INDEX row.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CASE column.
      WHEN 'OBJ_NAME'.
        navigate( ls_data ).
    ENDCASE.

  ENDMETHOD.                    "on_link_click

  METHOD navigate.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = is_data-obj_name
        object_type         = is_data-object
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3.
    IF sy-subrc <> 0.
      ASSERT 1 = 0.
    ENDIF.

  ENDMETHOD.                    "on_link_click

ENDCLASS.

START-OF-SELECTION.
  lcl_alv=>show( lcl_data=>fetch( ) ).
