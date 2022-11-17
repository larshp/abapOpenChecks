REPORT zaoc_tadir_checks.

TYPES: BEGIN OF ty_output,
         object   TYPE tadir-object,
         obj_name TYPE tadir-obj_name,
         text     TYPE string,
       END OF ty_output.

DATA: gt_output TYPE STANDARD TABLE OF ty_output WITH DEFAULT KEY.

TABLES: tadir.

SELECT-OPTIONS: s_devc FOR tadir-devclass OBLIGATORY.
PARAMETERS: p_pack TYPE c AS CHECKBOX DEFAULT 'X',
            p_del  TYPE c AS CHECKBOX DEFAULT 'X'.

START-OF-SELECTION.
  PERFORM run.
  PERFORM output.

CLASS lcl_logic DEFINITION FINAL.
  PUBLIC SECTION.
    CLASS-METHODS:
      in_open_transport IMPORTING is_tadir       TYPE tadir
                        RETURNING VALUE(rv_flag) TYPE abap_bool.
ENDCLASS.

CLASS lcl_logic IMPLEMENTATION.

  METHOD in_open_transport.

    DATA: lt_e070 TYPE STANDARD TABLE OF e070 WITH DEFAULT KEY,
          lt_e071 TYPE STANDARD TABLE OF e071 WITH DEFAULT KEY.

    SELECT * FROM e071 INTO TABLE lt_e071
      WHERE pgmid = is_tadir-pgmid
      AND object = is_tadir-object
      AND obj_name = is_tadir-obj_name
      ORDER BY PRIMARY KEY.                               "#EC CI_SUBRC
    IF lines( lt_e071 ) > 0.
      SELECT * FROM e070 INTO TABLE lt_e070
        FOR ALL ENTRIES IN lt_e071
        WHERE trkorr = lt_e071-trkorr
        AND trstatus = 'D'
        ORDER BY PRIMARY KEY.                             "#EC CI_SUBRC
    ENDIF.

    IF lines( lt_e070 ) > 0.
      rv_flag = abap_true.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

FORM run.

  DATA: lt_tadir    TYPE STANDARD TABLE OF tadir WITH DEFAULT KEY,
        ls_output   LIKE LINE OF gt_output,
        lv_devclass TYPE devclass.

  FIELD-SYMBOLS: <ls_tadir> LIKE LINE OF lt_tadir.


  SELECT * FROM tadir INTO TABLE lt_tadir
    WHERE devclass IN s_devc
    ORDER BY PRIMARY KEY.                 "#EC CI_GENBUFF "#EC CI_SUBRC

  LOOP AT lt_tadir ASSIGNING <ls_tadir>.
    IF sy-tabix MOD 500 = 0.
      cl_progress_indicator=>progress_indicate(
        i_text               = 'Processing'
        i_processed          = sy-tabix
        i_total              = lines( lt_tadir )
        i_output_immediately = abap_true ) ##NO_TEXT.
    ENDIF.

    SELECT SINGLE devclass FROM tdevc INTO lv_devclass
      WHERE devclass = <ls_tadir>-devclass.             "#EC CI_GENBUFF
    IF sy-subrc <> 0
        AND p_pack = abap_true
        AND NOT ( <ls_tadir>-object = 'DEVC'
        AND lcl_logic=>in_open_transport( <ls_tadir> ) = abap_true ).
      ls_output-object   = <ls_tadir>-object.
      ls_output-obj_name = <ls_tadir>-obj_name.
      ls_output-text     = |Package { <ls_tadir>-devclass }, does not exist|.
      APPEND ls_output TO gt_output.
    ENDIF.

    IF <ls_tadir>-delflag = abap_true
        AND p_del = abap_true
        AND lcl_logic=>in_open_transport( <ls_tadir> ) = abap_false.
      ls_output-object   = <ls_tadir>-object.
      ls_output-obj_name = <ls_tadir>-obj_name.
      ls_output-text     = 'deletion flag, but not in open tr' ##NO_TEXT.
      APPEND ls_output TO gt_output.
    ENDIF.

  ENDLOOP.

ENDFORM.

FORM output RAISING cx_salv_msg.

  DATA: lo_alv TYPE REF TO cl_salv_table.

  cl_salv_table=>factory(
    IMPORTING
      r_salv_table = lo_alv
    CHANGING
      t_table      = gt_output ).

  lo_alv->get_columns( )->set_optimize( ).
  lo_alv->get_functions( )->set_all( ).
  lo_alv->display( ).

ENDFORM.
