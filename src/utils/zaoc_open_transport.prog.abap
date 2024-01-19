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

PARAMETERS p_insp TYPE sciins_inf-inspecname OBLIGATORY.

SELECT-OPTIONS: s_trkorr FOR e070-trkorr,
                s_user   FOR e070-as4user.

PARAMETERS p_mail AS CHECKBOX DEFAULT ''.

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
        RETURNING VALUE(rt_data) TYPE ty_output_tt,
      send_mails
        IMPORTING
          it_data TYPE ty_output_tt.

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

    IF p_mail = abap_true.
      send_mails( rt_data ).
    ENDIF.

  ENDMETHOD.                    "run

  METHOD filter.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF gt_objects,
                   <ls_sci>    LIKE LINE OF gt_sci,
                   <ls_data>   LIKE LINE OF rt_data.


    SORT gt_objects BY include ASCENDING.
    DELETE ADJACENT DUPLICATES FROM gt_objects COMPARING include.

    LOOP AT gt_objects ASSIGNING <ls_object>.
      CASE <ls_object>-object.
        WHEN 'PROG'.
          READ TABLE gt_sci ASSIGNING <ls_sci>
            WITH KEY sobjname = <ls_object>-include BINARY SEARCH.
        WHEN OTHERS.
          READ TABLE gt_sci ASSIGNING <ls_sci>
            WITH KEY objname = <ls_object>-obj_name.
      ENDCASE.
      IF sy-subrc = 0.
        APPEND INITIAL LINE TO rt_data ASSIGNING <ls_data>.
        MOVE-CORRESPONDING <ls_sci> TO <ls_data>.
        MOVE-CORRESPONDING <ls_object> TO <ls_data>.
        IF <ls_data>-sobjtype IS INITIAL.
          <ls_data>-sobjtype = <ls_object>-object.
        ENDIF.
        IF <ls_data>-sobjname IS INITIAL.
          <ls_data>-sobjname = <ls_object>-obj_name.
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

  METHOD send_mails.

    DATA: lt_mail_body TYPE bcsy_text,
          lv_mail_line TYPE soli,
          lv_mail_subject TYPE so_obj_des.

    TRY.
        DATA(lo_send_request) = cl_bcs=>create_persistent( ).

        lv_mail_line-line = 'Generic message about development guidelines.'.
        APPEND lv_mail_line TO lt_mail_body.

        LOOP AT it_data ASSIGNING FIELD-SYMBOL(<ls_object>)
            GROUP BY ( key1 = <ls_object>-as4user )
            INTO DATA(ls_group1).

          LOOP AT GROUP ls_group1 ASSIGNING FIELD-SYMBOL(<ls_group>).
            cl_abap_container_utilities=>fill_container_c(
              EXPORTING
                im_value = <ls_group>
              IMPORTING
                ex_container = DATA(lv_container) ).
            lv_mail_line-line = lv_container.
            APPEND lv_mail_line TO lt_mail_body.
            CLEAR lv_mail_line.
          ENDLOOP.

          DATA(lv_recipient) = cl_cam_address_bcs=>create_user_home_address(
                                i_commtype = 'INT'
                                i_user     = <ls_group>-as4user ).

          IF lv_recipient IS INITIAL.
            CONTINUE.
          ENDIF.

          lo_send_request->add_recipient( i_recipient = lv_recipient ).

          DATA(lv_sender) = cl_cam_address_bcs=>create_user_home_address(
                            i_commtype = 'INT'
                            i_user     = sy-uname ).

          lo_send_request->set_sender( i_sender = lv_sender ).

          DATA(lo_document) = cl_document_bcs=>create_document(
            i_type    = 'RAW'
            i_text    = lt_mail_body
            i_subject = lv_mail_subject ).
          lo_send_request->set_document( lo_document ).

          lo_send_request->send( i_with_error_screen = 'X' ).
          COMMIT WORK.

        ENDLOOP.
      CATCH cx_bcs.
        RETURN.
    ENDTRY.

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