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
DATA: go_container TYPE REF TO cl_gui_custom_container,
      gt_email_head TYPE soli_tab,
      gt_email_foot TYPE soli_tab,
      gv_ucomm      TYPE sy-ucomm.

PARAMETERS p_insp TYPE sciins_inf-inspecname OBLIGATORY.

SELECT-OPTIONS: s_trkorr FOR e070-trkorr,
                s_user   FOR e070-as4user.

SELECTION-SCREEN SKIP 1.

PARAMETERS p_mail AS CHECKBOX DEFAULT ''.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-t01.

PARAMETERS: p_load TYPE abap_bool RADIOBUTTON GROUP g1 DEFAULT 'X' USER-COMMAND c_load,
            p_save TYPE abap_bool RADIOBUTTON GROUP g1.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-t05.
PARAMETERS p_id   TYPE thead-tdid MODIF ID i1.
PARAMETERS p_name TYPE thead-tdname MODIF ID i1.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-t06.
PARAMETERS: p_sub  TYPE string MODIF ID i2.
SELECTION-SCREEN COMMENT /1(30) TEXT-t02 MODIF ID i2.
SELECTION-SCREEN PUSHBUTTON 33(8) TEXT-t04 USER-COMMAND c_head MODIF ID i2.
SELECTION-SCREEN COMMENT /1(30) TEXT-t03 MODIF ID i2.
SELECTION-SCREEN PUSHBUTTON 33(8) TEXT-t04 USER-COMMAND c_foot MODIF ID i2.
SELECTION-SCREEN END OF BLOCK b3.


AT SELECTION-SCREEN OUTPUT.
  CASE p_load.
    WHEN abap_true.
      LOOP AT SCREEN.
        CASE screen-group1.
          WHEN 'I1'.
            screen-active = '1'.
            MODIFY SCREEN.
          WHEN 'I2'.
            screen-active = '0'.
            MODIFY SCREEN.
        ENDCASE.
      ENDLOOP.
    WHEN abap_false.
      LOOP AT SCREEN.
        CASE screen-group1.
          WHEN 'I1'.
            screen-active = '0'.
            MODIFY SCREEN.
          WHEN 'I2'.
            screen-active = '1'.
            MODIFY SCREEN.
        ENDCASE.
      ENDLOOP.
  ENDCASE.

AT SELECTION-SCREEN.

  IF sy-ucomm = 'C_HEAD' OR sy-ucomm = 'C_FOOT'.
    gv_ucomm = sy-ucomm.
    CALL SCREEN 1001 STARTING AT 10 5.
  ENDIF.

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
      load_template,
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
      IF p_load = abap_true.
        load_template( ).
      ENDIF.
      send_mails( rt_data ).
      CLEAR: gt_email_head, gt_email_foot, p_id, p_name, p_sub.
    ENDIF.

  ENDMETHOD.                    "run

  METHOD filter.

    FIELD-SYMBOLS: <ls_object> LIKE LINE OF gt_objects,
                   <ls_sci>    LIKE LINE OF gt_sci,
                   <ls_data>   LIKE LINE OF rt_data.


    SORT gt_objects BY include ASCENDING.
    DELETE ADJACENT DUPLICATES FROM gt_objects COMPARING trkorr
                                                        object
                                                        obj_name
                                                        include.

    LOOP AT gt_sci ASSIGNING <ls_sci>.

      CASE <ls_sci>-sobjtype.
        WHEN 'PROG'.
          READ TABLE gt_objects ASSIGNING <ls_object>
            WITH KEY include = <ls_sci>-sobjname BINARY SEARCH.
        WHEN OTHERS.
          READ TABLE gt_objects ASSIGNING <ls_object>
            WITH KEY obj_name = <ls_sci>-objname.
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

    SORT rt_data BY as4user ASCENDING.

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
          lv_mail_table TYPE soli,
          lv_mail_subject TYPE so_obj_des.

    TRY.

        lv_mail_subject = p_sub.

        LOOP AT it_data ASSIGNING FIELD-SYMBOL(<ls_object>).
          AT NEW as4user.
            CLEAR lt_mail_body.
            LOOP AT gt_email_head ASSIGNING FIELD-SYMBOL(<ls_head>).
              lv_mail_table-line = <ls_head>.
              APPEND lv_mail_table TO lt_mail_body.
            ENDLOOP.
            lv_mail_table-line = '<tbody>'.
            APPEND lv_mail_table TO lt_mail_body.
            lv_mail_table-line = |System: { sy-sysid } |.
            APPEND lv_mail_table TO lt_mail_body.
            lv_mail_table-line = |<html><table border="1"><thead><tr align= "left"><th>Request/Task</th><th>Owner</th>|.
            APPEND lv_mail_table TO lt_mail_body.
            lv_mail_table-line = |<th>Text</th><th>Description</th><th>Check Description</th><th>Object Type</th>|.
            APPEND lv_mail_table TO lt_mail_body.
            lv_mail_table-line = |<th>Object name</th><th>Line of code</th></tr></thead>|.
            APPEND lv_mail_table TO lt_mail_body.
            LOOP AT it_data ASSIGNING FIELD-SYMBOL(<ls_group>)
                WHERE as4user = <ls_object>-as4user.
              lv_mail_table-line = |<tr><td> { <ls_group>-trkorr } </td><td> { <ls_group>-as4user } </td>|.
              APPEND lv_mail_table TO lt_mail_body.
              lv_mail_table-line = |<td> { <ls_group>-kind } </td><td> { <ls_group>-text } </td>|.
              APPEND lv_mail_table TO lt_mail_body.
              lv_mail_table-line = |<td> { <ls_group>-description } </td><td> { <ls_group>-sobjtype } </td>|.
              APPEND lv_mail_table TO lt_mail_body.
              lv_mail_table-line = |<td> { <ls_group>-sobjname } </td><td> { <ls_group>-line } </td></tr>|.
              APPEND lv_mail_table TO lt_mail_body.
            ENDLOOP.
            lv_mail_table-line = '</table>'.
            APPEND lv_mail_table TO lt_mail_body.
            lv_mail_table-line = '<br>'.
            APPEND lv_mail_table TO lt_mail_body.
            LOOP AT gt_email_foot ASSIGNING FIELD-SYMBOL(<ls_foot>).
              lv_mail_table-line = <ls_foot>.
              APPEND lv_mail_table TO lt_mail_body.
            ENDLOOP.
            CLEAR lv_mail_table.

            DATA(lo_send_request) = cl_bcs=>create_persistent( ).

            DATA(lv_recipient) = cl_cam_address_bcs=>create_user_home_address(
                                  i_commtype = 'INT'
                                  i_user     = <ls_object>-as4user ).
            IF lv_recipient IS INITIAL.
              FREE lv_recipient.
              CONTINUE.
            ENDIF.
            lo_send_request->add_recipient( i_recipient = lv_recipient ).
            FREE lv_recipient.

            DATA(lv_sender) = cl_cam_address_bcs=>create_user_home_address(
                              i_commtype = 'INT'
                              i_user     = sy-uname ).
            lo_send_request->set_sender( i_sender = lv_sender ).
            FREE lv_sender.

            DATA(lo_document) = cl_document_bcs=>create_document(
              i_type    = 'HTM'
              i_text    = lt_mail_body
              i_subject = lv_mail_subject ).
            lo_send_request->set_document( lo_document ).
            DATA(lv_sent_to_all) = lo_send_request->send( i_with_error_screen = 'X' ).
            IF lv_sent_to_all = abap_true.
              COMMIT WORK.
            ENDIF.
            CLEAR lo_send_request.
          ENDAT.
        ENDLOOP.
      CATCH cx_bcs.
        RETURN.
    ENDTRY.

  ENDMETHOD.

  METHOD load_template.

    DATA: lt_lines TYPE STANDARD TABLE OF tline WITH EMPTY KEY,
          ls_header TYPE thead.

    CALL FUNCTION 'READ_TEXT'
      EXPORTING
        client = sy-mandt
        object = 'TEXT'
        name = p_name
        id = p_id
        language = sy-langu
      IMPORTING
        header = ls_header
      TABLES
        lines = lt_lines
      EXCEPTIONS
        id = 1
        language = 2
        name = 3
        not_found = 4
        object = 5
        reference_check = 6
        wrong_access_to_archive = 7
        OTHERS = 8.

    LOOP AT lt_lines ASSIGNING FIELD-SYMBOL(<ls_line>).
      CASE <ls_line>-tdformat. "texts, text objects, styles configurable via std. SO10.
        WHEN 'YT'.
          p_sub = <ls_line>-tdline.
        WHEN 'Y1'.
          APPEND <ls_line>-tdline TO gt_email_head.
        WHEN 'Y2'.
          APPEND <ls_line>-tdline TO gt_email_foot.
      ENDCASE.
    ENDLOOP.

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
    IF p_mail = abap_true.
      MESSAGE 'Relevant emails to the users with problematic objects were sent, check SOST' TYPE 'S' DISPLAY LIKE 'I'.
    ENDIF.
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

MODULE status_1001 OUTPUT.
  SET PF-STATUS 'POPUP'.
  SET TITLEBAR 'POPUP'.
  go_container = NEW #( container_name = 'CONTAINER' ).
  DATA(lo_editor) = NEW cl_gui_textedit( parent        = go_container
                                     wordwrap_mode     = '2'
                                     wordwrap_position = '250' ).
  CASE gv_ucomm.
    WHEN 'C_HEAD'.
      lo_editor->set_text_as_r3table( table = gt_email_head[] ).
    WHEN 'C_FOOT'.
      lo_editor->set_text_as_r3table( table = gt_email_foot[] ).
  ENDCASE.
  cl_gui_docking_container=>set_focus( lo_editor ).
  lo_editor->set_readonly_mode( 0 ).
ENDMODULE.
MODULE user_command_1001 INPUT.
  CASE sy-ucomm.
    WHEN 'OK'.
      CASE gv_ucomm.
        WHEN 'C_HEAD'.
          lo_editor->get_text_as_r3table( IMPORTING table = gt_email_head[] ).
        WHEN 'C_FOOT'.
          lo_editor->get_text_as_r3table( IMPORTING table = gt_email_foot[] ).
      ENDCASE.
      go_container->free( ).
      cl_gui_cfw=>flush( ).
      SET SCREEN 0.
      LEAVE SCREEN.
    WHEN 'CANCEL'.
      SET SCREEN 0.
      LEAVE SCREEN.
  ENDCASE.
ENDMODULE.
