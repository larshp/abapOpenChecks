REPORT zaoc_performance.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

TABLES: seoclassdf.

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-003.
PARAMETERS: p_chkvar TYPE sci_chkv.
SELECT-OPTIONS: s_class FOR seoclassdf-clsname.
PARAMETERS: p_uchkv TYPE xfeld RADIOBUTTON GROUP g2,
            p_uclsv TYPE xfeld RADIOBUTTON GROUP g2.

SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-004.
PARAMETERS: p_objs  TYPE sci_dynp-i_objs OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-005.
PARAMETERS: p_user   TYPE sci_dynp-usr DEFAULT sy-uname,
            p_global RADIOBUTTON GROUP g1,
            p_local  RADIOBUTTON GROUP g1.
SELECTION-SCREEN END OF BLOCK b4.

SELECTION-SCREEN END OF BLOCK b1.

TYPES: BEGIN OF ty_classdf,
         clsname TYPE seoclassdf-clsname,
         sort    TYPE i,
       END OF ty_classdf.
TYPES tty_classdf TYPE STANDARD TABLE OF ty_classdf.
DATA: go_variant   TYPE REF TO cl_ci_checkvariant ##NEEDED,
      go_objectset TYPE REF TO cl_ci_objectset ##NEEDED.
DATA gv_error TYPE xfeld.

INITIALIZATION.
  PERFORM initialization.

START-OF-SELECTION.
  PERFORM check_sel.
  PERFORM run.

FORM initialization.

  FIELD-SYMBOLS: <ls_class> LIKE LINE OF s_class.


  IF s_class IS NOT INITIAL.
    RETURN.
  ENDIF.

  APPEND INITIAL LINE TO s_class ASSIGNING <ls_class>.
  ASSERT sy-subrc = 0.

  <ls_class>-option = 'CP'.
  <ls_class>-sign   = 'I'.
  <ls_class>-low    = 'ZCL_AOC_CHECK_++'.

  APPEND INITIAL LINE TO s_class ASSIGNING <ls_class>.
  ASSERT sy-subrc = 0.

  <ls_class>-option = 'CP'.
  <ls_class>-sign   = 'I'.
  <ls_class>-low    = 'ZCL_AOC_CHECK_+++'.

ENDFORM.

FORM run.

  DATA: lt_classdf  TYPE TABLE OF ty_classdf,
        lv_descript TYPE seoclasstx-descript,
        lv_t1       TYPE i,
        lv_t2       TYPE i,
        lv_seconds  TYPE i.
  DATA lv_cprefix TYPE string.
  DATA lv_number TYPE string.

  FIELD-SYMBOLS: <ls_classdf> LIKE LINE OF lt_classdf.

  IF gv_error = abap_true.
    RETURN.
  ENDIF.


  PERFORM get_objectset.

  IF p_uchkv = abap_true.
    PERFORM get_chk_class_from_variant CHANGING lt_classdf.
  ELSE.
    SELECT clsname FROM seoclassdf
      INTO CORRESPONDING FIELDS OF TABLE lt_classdf
      WHERE clsname IN s_class
      AND version = '1'
      ORDER BY PRIMARY KEY.                             "#EC CI_GENBUFF
  ENDIF.

  IF lt_classdf IS INITIAL.
    WRITE: / 'No classes found'(002).
  ENDIF.

  LOOP AT lt_classdf ASSIGNING <ls_classdf>.
    SPLIT <ls_classdf>-clsname AT 'ZCL_AOC_CHECK_' INTO lv_cprefix lv_number.
    TRY.
        <ls_classdf>-sort = lv_number.
      CATCH cx_root.
    ENDTRY.
  ENDLOOP.

  SORT lt_classdf BY sort.

  LOOP AT lt_classdf ASSIGNING <ls_classdf>.
    GET RUN TIME FIELD lv_t1.

    PERFORM create_variant USING <ls_classdf>-clsname.
    PERFORM run_inspection USING <ls_classdf>-clsname.

    GET RUN TIME FIELD lv_t2.
    lv_seconds = ( lv_t2 - lv_t1 ) / 1000000.
    CLEAR lv_descript.
    SELECT SINGLE descript FROM seoclasstx INTO lv_descript "#EC CI_SEL_NESTED
      WHERE clsname = <ls_classdf>-clsname
      AND langu = 'E'. "only E is supported
    ASSERT sy-subrc = 0.
    WRITE: / <ls_classdf>-clsname, lv_descript, lv_seconds, 's'.
  ENDLOOP.

ENDFORM.

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

FORM get_chk_class_from_variant CHANGING ct_classes TYPE tty_classdf.

  DATA lo_chk_var TYPE REF TO cl_ci_checkvariant.
  DATA lo_tests TYPE REF TO cl_ci_tests.
  DATA lo_tree TYPE REF TO cl_ci_category_top.
  DATA lr_category TYPE REF TO if_ci_test.
  DATA lr_check TYPE REF TO if_ci_test.

  FIELD-SYMBOLS <ls_class> TYPE ty_classdf.

  TRY.
      cl_ci_checkvariant=>get_ref(
        EXPORTING
          p_user            = space
          p_name            = p_chkvar
        RECEIVING
          p_ref             = lo_chk_var
        EXCEPTIONS
          chkv_not_exists   = 1
          missing_parameter = 2
          OTHERS            = 3 ).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      lo_chk_var->get_info(
        EXCEPTIONS
          could_not_read_variant = 1
          OTHERS                 = 2 ).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      cl_ci_tests=>get_tree(
        EXPORTING
          p_variant          = lo_chk_var->variant
          p_transport        = lo_chk_var->chkvinf-transport
        RECEIVING
          p_result           = lo_tests
        EXCEPTIONS
          invalid_category   = 1
          invalid_class_name = 2
          OTHERS             = 3 ).
      IF sy-subrc <> 0.
        RETURN.
      ENDIF.

      lo_tree ?= lo_tests->tree.

      LOOP AT lo_tree->childs INTO lr_category.
        IF lr_category->selected = abap_false.
          CONTINUE.
        ENDIF.
        IF lr_category->name <> 'ZCL_AOC_CATEGORY'.
          CONTINUE.
        ENDIF.
        LOOP AT lr_category->childs INTO lr_check.
          IF lr_check->selected = abap_true.
            APPEND INITIAL LINE TO ct_classes ASSIGNING <ls_class>.
            <ls_class>-clsname = lr_check->name.
          ENDIF.
        ENDLOOP.
      ENDLOOP.

    CATCH cx_root.
  ENDTRY.

ENDFORM.

FORM check_sel.

  IF ( p_uchkv = abap_true AND p_chkvar IS INITIAL ) OR
      ( p_uclsv = abap_true AND s_class IS INITIAL ).
    MESSAGE TEXT-006 TYPE 'S' DISPLAY LIKE 'E'.
    gv_error = abap_true.
  ENDIF.

ENDFORM.