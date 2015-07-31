REPORT zaoc_performance.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

TABLES: seoclassdf.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

SELECT-OPTIONS: s_class FOR seoclassdf-clsname.

PARAMETERS: p_objs   TYPE sci_dynp-i_objs OBLIGATORY,
            p_user   TYPE sci_dynp-usr DEFAULT sy-uname,
            p_global RADIOBUTTON GROUP g1,
            p_local  RADIOBUTTON GROUP g1.

SELECTION-SCREEN END OF BLOCK b1.

DATA: go_variant   TYPE REF TO cl_ci_checkvariant,
      go_objectset TYPE REF TO cl_ci_objectset.

INITIALIZATION.
  PERFORM initialization.

START-OF-SELECTION.
  PERFORM run.

*&---------------------------------------------------------------------*
*&      Form  INITIALIZATION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM initialization.

  FIELD-SYMBOLS: <ls_class> LIKE LINE OF s_class.


  IF NOT s_class IS INITIAL.
    RETURN.
  ENDIF.

  APPEND INITIAL LINE TO s_class ASSIGNING <ls_class>.
  ASSERT sy-subrc = 0.

  <ls_class>-option = 'CP'.
  <ls_class>-sign   = 'I'.
  <ls_class>-low    = 'ZCL_AOC_CHECK_++'.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  RUN
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM run.

  DATA: lt_classdf  TYPE TABLE OF seoclassdf,
        lv_descript TYPE seoclasstx-descript,
        lv_t1       TYPE i,
        lv_t2       TYPE i,
        lv_seconds  TYPE i.

  FIELD-SYMBOLS: <ls_classdf> LIKE LINE OF lt_classdf.


  PERFORM get_objectset.

  SELECT * FROM seoclassdf
    INTO TABLE lt_classdf
    WHERE clsname IN s_class
    AND version = '1'.                                  "#EC CI_GENBUFF
  IF sy-subrc <> 0.
    WRITE: / 'No classes found'(002).
  ENDIF.

  LOOP AT lt_classdf ASSIGNING <ls_classdf>.
    GET RUN TIME FIELD lv_t1.

    PERFORM create_variant USING <ls_classdf>-clsname.
    PERFORM run_inspection USING <ls_classdf>-clsname.

    GET RUN TIME FIELD lv_t2.
    lv_seconds = ( lv_t2 - lv_t1 ) / 1000000.
    CLEAR lv_descript.
    SELECT SINGLE descript FROM seoclasstx INTO lv_descript
      WHERE clsname = <ls_classdf>-clsname AND langu = sy-langu. "#EC CI_SUBRC
    ASSERT sy-subrc = 0.
    WRITE: / <ls_classdf>-clsname, lv_descript, lv_seconds, 's'.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  GET_OBJECTSET
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_objectset.

  DATA: lv_user TYPE syuname.

  IF p_global = abap_true.
    CLEAR lv_user.
  ELSE.
    lv_user = p_user.
  ENDIF.

  cl_ci_objectset=>get_ref(
    EXPORTING
      p_user                    = lv_user
      p_objsnam                 = p_objs
    RECEIVING
      p_ref                     = go_objectset
    EXCEPTIONS
      missing_parameter         = 1
      objs_not_exists           = 2
      invalid_request           = 3
      object_not_exists         = 4
      object_may_not_be_checked = 5
      no_main_program           = 6
      OTHERS                    = 7 ).                    "#EC CI_SUBRC
  ASSERT sy-subrc = 0.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  RUN_INSPECTION
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM run_inspection USING pv_class TYPE seoclsname.

  DATA: lv_name TYPE sci_insp,
        lv_text TYPE sci_text,
        lv_date TYPE datum,
        lo_ci   TYPE REF TO cl_ci_inspection.


  lv_name = pv_class.
  lv_text = pv_class.

  cl_ci_inspection=>create(
    EXPORTING
      p_user           = sy-uname
      p_name           = lv_name
    RECEIVING
      p_ref            = lo_ci
    EXCEPTIONS
      locked           = 1
      error_in_enqueue = 2
      not_authorized   = 3
      OTHERS           = 4 ).                             "#EC CI_SUBRC
  ASSERT sy-subrc = 0.

  lv_date = sy-datum + 10.

  lo_ci->set( p_chkv    = go_variant
              p_objs    = go_objectset
              p_text    = lv_text
              p_deldate = lv_date ).

  lo_ci->save( EXCEPTIONS
                 missing_information = 1
                 insp_no_name        = 2
                 not_enqueued        = 3
                 OTHERS              = 4 ).               "#EC CI_SUBRC
  ASSERT sy-subrc = 0.

  lo_ci->run( EXPORTING
                p_howtorun            = 'D'
              EXCEPTIONS
                invalid_check_version = 1
                OTHERS                = 2 ).              "#EC CI_SUBRC
  ASSERT sy-subrc = 0.

  lo_ci->leave_change( ).

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  CREATE_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_<LS_CLASSDF>_CLSNAME  text
*----------------------------------------------------------------------*
FORM create_variant USING pv_clsname TYPE seoclsname.

  DATA: lv_name    TYPE sci_chkv,
        lt_variant TYPE sci_tstvar,
        ls_variant LIKE LINE OF lt_variant.


  lv_name = pv_clsname.

  cl_ci_checkvariant=>create(
    EXPORTING
      p_user              = sy-uname
      p_name              = lv_name
    RECEIVING
      p_ref               = go_variant
    EXCEPTIONS
      chkv_already_exists = 1
      locked              = 2
      error_in_enqueue    = 3
      not_authorized      = 4
      OTHERS              = 5 ).                          "#EC CI_SUBRC
  ASSERT sy-subrc = 0.

  ls_variant-testname = pv_clsname.
  INSERT ls_variant INTO TABLE lt_variant.

  go_variant->set_variant(
    EXPORTING
      p_variant       = lt_variant
    EXCEPTIONS
      not_enqueued    = 1
      OTHERS          = 2 ).                              "#EC CI_SUBRC
  ASSERT sy-subrc = 0.

  go_variant->leave_change( ).

ENDFORM.