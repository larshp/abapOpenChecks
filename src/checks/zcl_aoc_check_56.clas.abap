CLASS zcl_aoc_check_56 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS check
        REDEFINITION .
    METHODS get_attributes
        REDEFINITION .
    METHODS if_ci_test~query_attributes
        REDEFINITION .
    METHODS put_attributes
        REDEFINITION .
  PROTECTED SECTION.

    TYPES:
      BEGIN OF ty_vseosubcdf,
        clsname    TYPE vseosubcdf-clsname,
        cmpname    TYPE vseosubcdf-cmpname,
        sconame    TYPE vseosubcdf-sconame,
        version    TYPE vseosubcdf-version,
        paroptionl TYPE vseosubcdf-paroptionl,
      END OF ty_vseosubcdf .
    TYPES:
      ty_vseosubcdf_tt TYPE STANDARD TABLE OF ty_vseosubcdf WITH DEFAULT KEY .

    DATA mv_supplied TYPE flag .
    DATA mv_referenced TYPE flag .

    METHODS report_unused_clas
      IMPORTING
        !is_method    TYPE seocompo
        !is_parameter TYPE ty_vseosubcdf .
    METHODS check_supplied
      IMPORTING
        !is_method TYPE seocompo .
    METHODS get_name
      IMPORTING
        !iv_full_name  TYPE string
      RETURNING
        VALUE(rv_name) TYPE seosconame .
    METHODS check_locally_referenced
      IMPORTING
        !is_method TYPE seocompo .
    METHODS check_method
      IMPORTING
        !is_method TYPE seocompo .
    METHODS find_where_used
      IMPORTING
        !is_method       TYPE seocompo
      RETURNING
        VALUE(rt_founds) TYPE sci_findlst .
    METHODS report_unreferenced
      IMPORTING
        !is_method     TYPE seocompo
        !it_parameters TYPE ty_vseosubcdf_tt .
    METHODS report_unused
      IMPORTING
        !is_method     TYPE seocompo
        !it_parameters TYPE ty_vseosubcdf_tt .
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_aoc_check_56 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_methods TYPE STANDARD TABLE OF seocompo WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_method> LIKE LINE OF lt_methods.


    IF object_type <> 'CLAS' AND object_type <> 'INTF'.
      RETURN.
    ENDIF.

* SEOCOMPO
    SELECT * FROM seocompo
      INTO TABLE lt_methods
      WHERE clsname = object_name
      AND cmptype = '1'
      ORDER BY PRIMARY KEY.     "#EC CI_SUBRC "#EC CI_ALL_FIELDS_NEEDED

    LOOP AT lt_methods ASSIGNING <ls_method>.
      check_method( <ls_method> ).
    ENDLOOP.

  ENDMETHOD.


  METHOD check_locally_referenced.

    CONSTANTS: lc_macro TYPE char1 VALUE 'D'.

    DATA: lt_compiler   TYPE scr_refs,
          ls_compiler   LIKE LINE OF lt_compiler,
          ls_mtdkey     TYPE seocpdkey,
          lo_compiler   TYPE REF TO zcl_aoc_compiler,
          lv_include    TYPE programm,
          lv_name       TYPE seosconame,
          lt_parameters TYPE ty_vseosubcdf_tt.

    IF mv_referenced = abap_false.
      RETURN.
    ENDIF.

    SELECT * FROM vseosubcdf
      INTO CORRESPONDING FIELDS OF TABLE lt_parameters
      WHERE clsname = is_method-clsname
      AND cmpname = is_method-cmpname
      AND version = '1'
      AND cmptype = '1'
      AND scotype <> '1'
      ORDER BY clsname cmpname sconame version. "#EC CI_ALL_FIELDS_NEEDED
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    lo_compiler = zcl_aoc_compiler=>get_instance(
      iv_object_type = object_type
      iv_object_name = object_name ).
    IF lo_compiler->has_error( ) = abap_true.
      RETURN.
    ENDIF.
    lt_compiler = lo_compiler->get_result( ).

    ls_mtdkey-clsname = is_method-clsname.
    ls_mtdkey-cpdname = is_method-cmpname.

    cl_oo_classname_service=>get_method_include(
      EXPORTING
        mtdkey                = ls_mtdkey
      RECEIVING
        result                = lv_include
      EXCEPTIONS
        class_not_existing    = 1
        method_not_existing   = 2
        OTHERS                = 3 ).
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

* used in parrallel RECEIVE RESULTS / CALLING methods
    DELETE lt_parameters WHERE sconame = 'P_TASK'.

    LOOP AT lt_compiler INTO ls_compiler
        WHERE ( tag = cl_abap_compiler=>tag_data OR tag = cl_abap_compiler=>tag_method OR
                tag = cl_abap_compiler=>tag_message_id OR tag = cl_abap_compiler=>tag_message_number OR
                tag = cl_abap_compiler=>tag_message_type )
        AND ( statement->source_info->name = lv_include
        OR statement->source_info->kind = lc_macro ).
      lv_name = get_name( ls_compiler-full_name ).
      IF lv_name IS INITIAL.
        lv_name = ls_compiler-name.
      ENDIF.
      DELETE lt_parameters WHERE sconame = lv_name.
      CLEAR lv_name.
    ENDLOOP.

    IF lines( lt_parameters ) > 0.
      report_unreferenced(
         is_method     = is_method
         it_parameters = lt_parameters ).
    ENDIF.

  ENDMETHOD.


  METHOD check_method.

    DATA: lv_clsname TYPE seometarel-clsname.


    check_supplied( is_method ).

    IF object_type = 'CLAS'.
      SELECT SINGLE clsname INTO lv_clsname
        FROM seometarel
        WHERE clsname = is_method-clsname
        AND refclsname = 'IF_AMDP_MARKER_HDB'
        AND version = '1'
        AND reltype = '1'.
      IF sy-subrc <> 0.
        check_locally_referenced( is_method ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD check_supplied.

    DATA: lt_found      TYPE sci_findlst,
          lv_index      TYPE i,
          lv_prefered   TYPE seosubcodf-parpreferd,
          lt_parameters TYPE ty_vseosubcdf_tt.

    FIELD-SYMBOLS: <ls_parameter> LIKE LINE OF lt_parameters,
                   <ls_found>     LIKE LINE OF lt_found.


    IF mv_supplied = abap_false.
      RETURN.
    ENDIF.

    SELECT * FROM vseosubcdf
      INTO CORRESPONDING FIELDS OF TABLE lt_parameters
      WHERE clsname = is_method-clsname
      AND cmpname = is_method-cmpname
      AND version = '1'
      AND pardecltyp = '0'
      AND scotype = '0'
      ORDER BY clsname cmpname sconame version. "#EC CI_SUBRC "#EC CI_ALL_FIELDS_NEEDED

    IF lines( lt_parameters ) <= 1.
      RETURN.
    ENDIF.

    LOOP AT lt_parameters ASSIGNING <ls_parameter>.
      lv_index = sy-tabix.
      SELECT SINGLE parpreferd FROM seosubcodf INTO lv_prefered
        WHERE clsname = <ls_parameter>-clsname
        AND cmpname = <ls_parameter>-cmpname
        AND sconame = <ls_parameter>-sconame
        AND version = <ls_parameter>-version.
      IF sy-subrc = 0 AND lv_prefered = abap_true.
        DELETE lt_parameters INDEX lv_index.
      ENDIF.
    ENDLOOP.

    READ TABLE lt_parameters WITH KEY paroptionl = abap_false TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
* all parameters optional
      RETURN.
    ENDIF.

    DELETE lt_parameters WHERE paroptionl = abap_false.

    IF lines( lt_parameters ) = 0.
      RETURN.
    ENDIF.

    lt_found = find_where_used( is_method ).

    IF lines( lt_found ) = 0.
* assume method called dynamic or development ongoing
* this class will only check parameter usage, not method
      RETURN.
    ENDIF.

    LOOP AT lt_parameters ASSIGNING <ls_parameter>.
      lv_index = sy-tabix.

* this is not completely correct, string might contain the parameter name
* but it will only result in not reporting the finding
      LOOP AT lt_found ASSIGNING <ls_found>
          WHERE source CS <ls_parameter>-sconame
          OR source CS 'PARAMETER-TABLE'.
        EXIT.
      ENDLOOP.
      IF sy-subrc = 0.
        DELETE lt_parameters INDEX lv_index.
      ENDIF.
    ENDLOOP.

    report_unused( is_method     = is_method
                   it_parameters = lt_parameters ).

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version        = '001'.
    position       = '056'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_referenced = abap_true.
    mv_supplied   = abap_true.

    insert_scimessage(
        iv_code = '001'
        iv_text = 'Parameter &1 not supplied anywhere'(m01) ). "#EC NOTEX

    insert_scimessage(
        iv_code = '002'
        iv_text = 'Parameter &1 not supplied anywhere, method &2'(m02) ). "#EC NOTEX

    insert_scimessage(
        iv_code = '003'
        iv_text = 'Parameter &1 not referenced in method'(m03) ). "#EC NOTEX

  ENDMETHOD.


  METHOD find_where_used.

    DATA: lt_findstrings TYPE STANDARD TABLE OF rsfind WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_find> LIKE LINE OF lt_findstrings.


    APPEND INITIAL LINE TO lt_findstrings ASSIGNING <ls_find>.
    <ls_find>-object   = is_method-cmpname.
    <ls_find>-encl_obj = is_method-clsname.

    CALL FUNCTION 'RS_EU_CROSSREF'
      EXPORTING
        i_find_obj_cls               = 'OM'
        no_dialog                    = abap_true
        expand_source_in_online_mode = abap_true
        without_text                 = abap_true
      TABLES
        i_findstrings                = lt_findstrings
        o_founds                     = rt_founds
      EXCEPTIONS
        not_executed                 = 1
        not_found                    = 2
        illegal_object               = 3
        no_cross_for_this_object     = 4
        batch                        = 5
        batchjob_error               = 6
        wrong_type                   = 7
        object_not_exist             = 8
        OTHERS                       = 9 ##fm_subrc_ok.   "#EC CI_SUBRC

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mv_referenced = mv_referenced
      mv_supplied = mv_supplied
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD get_name.
    DATA: lv_moffset TYPE i,
          lv_mlenght TYPE i.

    FIND REGEX '([^:]*)$' IN iv_full_name MATCH OFFSET lv_moffset MATCH LENGTH lv_mlenght.
    IF sy-subrc = 0.
      rv_name = iv_full_name+lv_moffset(lv_mlenght).
    ENDIF.
  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT
    zzaoc_fill_att mv_referenced 'Check referenced' ''.     "#EC NOTEXT
    zzaoc_fill_att mv_supplied 'Check supplied' ''.         "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mv_referenced = mv_referenced
      mv_supplied = mv_supplied
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.


  METHOD report_unreferenced.

    DATA: lv_include TYPE programm,
          ls_mtdkey  TYPE seocpdkey.

    FIELD-SYMBOLS: <ls_parameter> LIKE LINE OF it_parameters.


    LOOP AT it_parameters ASSIGNING <ls_parameter>.
      CASE object_type.
        WHEN 'CLAS'.
          ls_mtdkey-clsname = is_method-clsname.
          ls_mtdkey-cpdname = is_method-cmpname.
          lv_include = cl_oo_classname_service=>get_method_include( ls_mtdkey ).
          inform( p_sub_obj_name = lv_include
                  p_param_1      = <ls_parameter>-sconame
                  p_kind         = mv_errty
                  p_test         = myname
                  p_code         = '003' ).
        WHEN OTHERS.
          ASSERT 0 = 1.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD report_unused.

    FIELD-SYMBOLS: <ls_parameter> LIKE LINE OF it_parameters.


    LOOP AT it_parameters ASSIGNING <ls_parameter>.
      CASE object_type.
        WHEN 'CLAS'.
          report_unused_clas(
            is_method    = is_method
            is_parameter = <ls_parameter> ).
        WHEN 'INTF'.
          inform( p_param_1      = <ls_parameter>-sconame
                  p_param_2      = is_method-cmpname
                  p_kind         = mv_errty
                  p_test         = myname
                  p_code         = '002' ).
        WHEN OTHERS.
          ASSERT 0 = 1.
      ENDCASE.
    ENDLOOP.

  ENDMETHOD.


  METHOD report_unused_clas.

    DATA: lv_include TYPE programm,
          ls_mtdkey  TYPE seocpdkey.


    ls_mtdkey-clsname = is_method-clsname.
    ls_mtdkey-cpdname = is_method-cmpname.

    cl_oo_classname_service=>get_method_include(
      EXPORTING
        mtdkey = ls_mtdkey
      RECEIVING
        result = lv_include
      EXCEPTIONS
        OTHERS = 0 ).

    IF lv_include IS NOT INITIAL.
      inform( p_sub_obj_name = lv_include
              p_param_1      = is_parameter-sconame
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '001' ).
    ELSE.
      inform( p_param_1      = is_parameter-sconame
              p_param_2      = is_method-cmpname
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '002' ).
    ENDIF.

  ENDMETHOD.
ENDCLASS.
