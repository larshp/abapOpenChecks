REPORT zaoc_date_analysis.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS: p_insp TYPE sciins_inf-inspecname OBLIGATORY,
            p_vers TYPE sci_vers OBLIGATORY DEFAULT '001',
            p_glob TYPE c RADIOBUTTON GROUP g1 DEFAULT 'X',
            p_loc  TYPE c RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-002.
PARAMETERS: p_cdat TYPE c RADIOBUTTON GROUP g2 DEFAULT 'X',
            p_udat TYPE c RADIOBUTTON GROUP g2.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-003.
PARAMETERS: p_erro TYPE c AS CHECKBOX DEFAULT 'X',
            p_warn TYPE c AS CHECKBOX DEFAULT 'X',
            p_info TYPE c AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b3.

************************************

TYPES: BEGIN OF ty_sub,
         sobjtype TYPE scir_alvlist-sobjtype,
         sobjname TYPE scir_alvlist-sobjname,
         line     TYPE scir_alvlist-line,
         udat     TYPE reposrc-udat,
         cdat     TYPE reposrc-cdat,
         code     TYPE scir_alvlist-code,
         text     TYPE scir_alvlist-text,
       END OF ty_sub.

TYPES: ty_sub_tt TYPE STANDARD TABLE OF ty_sub WITH DEFAULT KEY.

TYPES: BEGIN OF ty_output,
         test        TYPE scir_alvlist-test,
         date        TYPE datum,
         count       TYPE i,
         description TYPE scir_alvlist-description,
         sub         TYPE ty_sub_tt,
       END OF ty_output.

TYPES: ty_output_tt TYPE STANDARD TABLE OF ty_output WITH DEFAULT KEY.

************************************

CLASS lcl_data DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      fetch
        RETURNING VALUE(rt_data) TYPE ty_output_tt.

  PRIVATE SECTION.
    CLASS-METHODS:
      aggregate
        IMPORTING it_results     TYPE scit_alvlist
        RETURNING VALUE(rt_data) TYPE ty_output_tt,
      read_inspection
        RETURNING VALUE(rt_results) TYPE scit_alvlist.

ENDCLASS.

CLASS lcl_data IMPLEMENTATION.

  METHOD fetch.
    rt_data = aggregate( read_inspection( ) ).
  ENDMETHOD.

  METHOD aggregate.

    DATA: lv_cdat TYPE reposrc-cdat,
          lv_cnam TYPE reposrc-cnam,
          lv_date TYPE datum,
          lv_udat TYPE reposrc-udat.

    FIELD-SYMBOLS: <ls_data>   LIKE LINE OF rt_data,
                   <ls_sub>    LIKE LINE OF <ls_data>-sub,
                   <ls_result> LIKE LINE OF it_results.


    LOOP AT it_results ASSIGNING <ls_result> WHERE sobjtype = 'PROG'.

      IF ( p_erro = abap_false AND <ls_result>-kind = 'E' )
          OR ( p_warn = abap_false AND <ls_result>-kind = 'W' )
          OR ( p_info = abap_false AND <ls_result>-kind = 'N' ).
        CONTINUE.
      ENDIF.

      SELECT SINGLE cdat udat cnam FROM reposrc INTO (lv_cdat, lv_udat, lv_cnam)
        WHERE progname = <ls_result>-sobjname
        AND r3state = 'A'.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      IF lv_cdat IS INITIAL OR lv_cnam = 'SAP'.
* typically SAP objects
        CONTINUE.
      ENDIF.

      IF p_cdat = abap_true.
        lv_date = lv_cdat.
      ELSE.
        lv_date = lv_udat.
      ENDIF.

      READ TABLE rt_data ASSIGNING <ls_data>
        WITH KEY
        test = <ls_result>-test
        date = lv_date.
      IF sy-subrc <> 0.
        APPEND INITIAL LINE TO rt_data ASSIGNING <ls_data>.
        <ls_data>-test        = <ls_result>-test.
        <ls_data>-date        = lv_date.
        <ls_data>-description = <ls_result>-description.
      ENDIF.
      <ls_data>-count = <ls_data>-count + 1.

      APPEND INITIAL LINE TO <ls_data>-sub ASSIGNING <ls_sub>.
      <ls_sub>-sobjtype = <ls_result>-sobjtype.
      <ls_sub>-sobjname = <ls_result>-sobjname.
      <ls_sub>-line     = <ls_result>-line.
      <ls_sub>-udat     = lv_udat.
      <ls_sub>-cdat     = lv_cdat.
      <ls_sub>-code     = <ls_result>-code.
      <ls_sub>-text     = <ls_result>-text.

    ENDLOOP.

  ENDMETHOD.

  METHOD read_inspection.

    DATA: lv_user TYPE sci_user,
          lo_ci   TYPE REF TO cl_ci_inspection.


    IF p_loc = abap_true.
      lv_user = sy-uname.
    ENDIF.

    cl_ci_inspection=>get_ref(
      EXPORTING
        p_user          = lv_user
        p_name          = p_insp
        p_vers          = p_vers
      RECEIVING
        p_ref           = lo_ci
      EXCEPTIONS
        insp_not_exists = 1
        OTHERS          = 2 ).
    IF sy-subrc = 1.
      RETURN.
    ENDIF.

* make sure SAP note 2043027 is installed
    lo_ci->plain_list(
      IMPORTING
        p_list = rt_results ).
    DELETE rt_results WHERE objtype = 'STAT'.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_alv_sub DEFINITION FINAL.

  PUBLIC SECTION.
    CLASS-METHODS:
      show
        IMPORTING it_table TYPE ty_sub_tt
        RAISING   cx_salv_error.

  PRIVATE SECTION.
    CLASS-DATA:
      gt_table TYPE ty_sub_tt.

    CLASS-METHODS:
      navigate
        IMPORTING
          is_data TYPE ty_sub,
      on_link_click
          FOR EVENT link_click OF cl_salv_events_table
        IMPORTING
          row
          column.

ENDCLASS.

CLASS lcl_alv_sub IMPLEMENTATION.

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
    lo_column ?= lo_alv->get_columns( )->get_column( 'SOBJNAME' ).
    lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

    lo_alv->get_functions( )->set_all( ).

    lo_events = lo_alv->get_event( ).
    SET HANDLER on_link_click FOR lo_events.
    lo_alv->display( ).

  ENDMETHOD.

  METHOD on_link_click.

    DATA: ls_data LIKE LINE OF gt_table.


    READ TABLE gt_table INTO ls_data INDEX row.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    CASE column.
      WHEN 'SOBJNAME'.
        navigate( ls_data ).
    ENDCASE.

  ENDMETHOD.                    "on_link_click

  METHOD navigate.

    CALL FUNCTION 'RS_TOOL_ACCESS'
      EXPORTING
        operation           = 'SHOW'
        object_name         = is_data-sobjname
        object_type         = is_data-sobjtype
        position            = is_data-line
      EXCEPTIONS
        not_executed        = 1
        invalid_object_type = 2
        OTHERS              = 3 ##FM_SUBRC_OK.                          "#EC CI_SUBRC

  ENDMETHOD.                    "on_link_click

ENDCLASS.

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
      on_link_click
          FOR EVENT link_click OF cl_salv_events_table
        IMPORTING
          row
          column.

ENDCLASS.

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
    lo_column ?= lo_alv->get_columns( )->get_column( 'COUNT' ).
    lo_column->set_cell_type( if_salv_c_cell_type=>hotspot ).

    lo_alv->get_functions( )->set_all( ).

    lo_events = lo_alv->get_event( ).
    SET HANDLER on_link_click FOR lo_events.
    lo_alv->display( ).

  ENDMETHOD.

  METHOD on_link_click.

    DATA: ls_data LIKE LINE OF gt_table.


    READ TABLE gt_table INTO ls_data INDEX row.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    TRY.
        CASE column.
          WHEN 'COUNT'.
            lcl_alv_sub=>show( ls_data-sub ).
        ENDCASE.
      CATCH cx_salv_error.
        ASSERT 0 = 1.
    ENDTRY.

  ENDMETHOD.                    "on_link_click

ENDCLASS.

START-OF-SELECTION.
  lcl_alv=>show( lcl_data=>fetch( ) ).
