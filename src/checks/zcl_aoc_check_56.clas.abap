CLASS zcl_aoc_check_56 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC.

  PUBLIC SECTION.

    METHODS constructor.

    METHODS check
        REDEFINITION.
    METHODS get_message_text
        REDEFINITION.
  PROTECTED SECTION.

    TYPES:
      ty_seosubcodf_tt TYPE STANDARD TABLE OF seosubcodf WITH DEFAULT KEY.

    METHODS check_method
      IMPORTING
        !is_method TYPE seocompodf.
    METHODS find_where_used
      IMPORTING
        !is_method       TYPE seocompodf
      RETURNING
        VALUE(rt_founds) TYPE sci_findlst.
    METHODS report_unused
      IMPORTING
        !is_method     TYPE seocompodf
        !it_parameters TYPE ty_seosubcodf_tt.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_AOC_CHECK_56 IMPLEMENTATION.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lt_methods TYPE STANDARD TABLE OF seocompodf WITH DEFAULT KEY.

    FIELD-SYMBOLS: <ls_method> LIKE LINE OF lt_methods.


    IF object_type <> 'CLAS' AND object_type <> 'INTF'.
      RETURN.
    ENDIF.

    SELECT * FROM seocompodf
      INTO TABLE lt_methods
      WHERE clsname = object_name
      AND version = '1'
      ORDER BY PRIMARY KEY.                               "#EC CI_SUBRC

    LOOP AT lt_methods ASSIGNING <ls_method>.
      check_method( <ls_method> ).
    ENDLOOP.

  ENDMETHOD.


  METHOD check_method.

    DATA: lt_found      TYPE sci_findlst,
          lv_index      TYPE i,
          lt_parameters TYPE ty_seosubcodf_tt.

    FIELD-SYMBOLS: <ls_parameter> LIKE LINE OF lt_parameters,
                   <ls_found>     LIKE LINE OF lt_found.


    SELECT * FROM seosubcodf INTO TABLE lt_parameters
      WHERE clsname = is_method-clsname
      AND cmpname = is_method-cmpname
      AND version = '1'
      AND pardecltyp = '0'
      ORDER BY PRIMARY KEY.                               "#EC CI_SUBRC

    IF lines( lt_parameters ) <= 1.
      RETURN.
    ENDIF.

    READ TABLE lt_parameters WITH KEY paroptionl = abap_false TRANSPORTING NO FIELDS.
    IF sy-subrc <> 0.
* all parameters optional
      RETURN.
    ENDIF.

    DELETE lt_parameters WHERE paroptionl = abap_false.

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

    description    = 'Method parameters'.                   "#EC NOTEXT
    category       = 'ZCL_AOC_CATEGORY'.
    version        = '001'.
    position       = '056'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_errty = c_error.

  ENDMETHOD.                    "CONSTRUCTOR


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
        OTHERS                       = 9 ##FM_SUBRC_OK. "#EC CI_SUBRC

  ENDMETHOD.


  METHOD get_message_text.

    CLEAR p_text.

    CASE p_code.
      WHEN '001'.
        p_text = 'Parameter &1 not supplied anywhere'.      "#EC NOTEXT
      WHEN OTHERS.
        super->get_message_text( EXPORTING p_test = p_test
                                           p_code = p_code
                                 IMPORTING p_text = p_text ).
    ENDCASE.

  ENDMETHOD.


  METHOD report_unused.

    DATA: lv_include TYPE programm,
          ls_mtdkey  TYPE seocpdkey.

    FIELD-SYMBOLS: <ls_parameter> LIKE LINE OF it_parameters.


    LOOP AT it_parameters ASSIGNING <ls_parameter>.
      IF object_type = 'CLAS'.
        ls_mtdkey-clsname = is_method-clsname.
        ls_mtdkey-cpdname = is_method-cmpname.
        lv_include = cl_oo_classname_service=>get_method_include( ls_mtdkey ).
      ENDIF.

      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = lv_include
              p_param_1      = <ls_parameter>-sconame
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '001' ).
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
