REPORT zaoc_open_transport.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

TABLES: e070.

TYPES: BEGIN OF ty_output,
         trkorr      TYPE e070-trkorr,
         as4user     TYPE e070-as4user,
         kind        TYPE scir_alvlist-kind,
         text        TYPE scir_alvlist-text,
         description TYPE scir_alvlist-description,
         sobjtype    TYPE scir_alvlist-sobjtype,
         sobjname    TYPE scir_alvlist-sobjname,
         line        TYPE scir_alvlist-line,
       END OF ty_output.

TYPES: ty_output_tt TYPE STANDARD TABLE OF ty_output WITH DEFAULT KEY.

PARAMETERS: p_insp TYPE sciins_inf-inspecname OBLIGATORY.

SELECT-OPTIONS: s_trkorr FOR e070-trkorr,
                s_user   FOR e070-as4user.

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
    TYPES: BEGIN OF ty_objects,
             trkorr   TYPE e070-trkorr,
             as4user  TYPE e070-as4user,
             pgmid    TYPE e071-pgmid,
             object   TYPE e071-object,
             obj_name TYPE e071-obj_name,
             include  TYPE programm,
           END OF ty_objects.

    CLASS-DATA: gt_sci     TYPE scit_alvlist,
                gt_objects TYPE TABLE OF ty_objects WITH DEFAULT KEY.

    CLASS-METHODS:
      find_objects,
      read_inspection,
      filter
        RETURNING VALUE(rt_data) TYPE ty_output_tt.

ENDCLASS.                    "lcl_app DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_app IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_data IMPLEMENTATION.

  METHOD fetch.

    find_objects( ).
    read_inspection( ).

    rt_data = filter( ).

  ENDMETHOD.                    "run

  METHOD filter.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF gt_objects,
                   <ls_sci>    LIKE LINE OF gt_sci,
                   <ls_data>   LIKE LINE OF rt_data.


    SORT gt_objects BY include ASCENDING.
    DELETE ADJACENT DUPLICATES FROM gt_objects COMPARING include.

    LOOP AT gt_sci ASSIGNING <ls_sci>.

      IF <ls_sci>-sobjtype = 'PROG'.
        READ TABLE gt_objects ASSIGNING <ls_object>
          WITH KEY include = <ls_sci>-sobjname BINARY SEARCH.
        IF sy-subrc = 0.
          APPEND INITIAL LINE TO rt_data ASSIGNING <ls_data>.
          MOVE-CORRESPONDING <ls_sci> TO <ls_data>.
          MOVE-CORRESPONDING <ls_object> TO <ls_data>.
        ENDIF.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.                    "filter

  METHOD read_inspection.

    DATA: lo_ci TYPE REF TO cl_ci_inspection.


    cl_ci_inspection=>get_ref(
      EXPORTING
        p_user          = ''
        p_name          = p_insp
      RECEIVING
        p_ref           = lo_ci
      EXCEPTIONS
        insp_not_exists = 1
        OTHERS          = 2 ).
    IF sy-subrc = 1.
      RETURN.
    ENDIF.
    ASSERT sy-subrc = 0.

* make sure SAP note 2043027 is installed
    lo_ci->plain_list(
      IMPORTING
        p_list = gt_sci ).
    DELETE gt_sci WHERE objtype = 'STAT'.

  ENDMETHOD.                    "read_inspection

  METHOD find_objects.

    DATA: ls_mtdkey TYPE seocpdkey.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF gt_objects.


    SELECT e070~trkorr as4user pgmid object obj_name
      FROM e070
      INNER JOIN e071 ON e070~trkorr = e071~trkorr
      INTO TABLE gt_objects
      WHERE trfunction = 'S'
      AND trstatus = 'D'
      AND e070~trkorr IN s_trkorr
      AND as4user IN s_user
      ORDER BY e070~trkorr ASCENDING
               as4user     ASCENDING ##TOO_MANY_ITAB_FIELDS.                     "#EC CI_SUBRC

    LOOP AT gt_objects ASSIGNING <ls_object>.
      IF <ls_object>-pgmid = 'LIMU'
          AND <ls_object>-object = 'METH'.
        ls_mtdkey = <ls_object>-obj_name.
        cl_oo_classname_service=>get_method_include(
          EXPORTING
            mtdkey              = ls_mtdkey
          RECEIVING
            result              = <ls_object>-include
          EXCEPTIONS
            class_not_existing  = 1
            method_not_existing = 2
            OTHERS              = 3 ).                    "#EC CI_SUBRC
      ELSEIF <ls_object>-object = 'PROG'
          OR <ls_object>-object = 'CINC'
          OR <ls_object>-object = 'REPS'.
        <ls_object>-include = <ls_object>-obj_name.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.                    "find_objects

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
    lo_column ?= lo_alv->get_columns( )->get_column( 'SOBJNAME' ).
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

ENDCLASS.                    "lcl_alv IMPLEMENTATION

START-OF-SELECTION.
  lcl_alv=>show( lcl_data=>fetch( ) ).
