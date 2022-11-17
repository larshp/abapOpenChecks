CLASS zcl_aoc_check_06 DEFINITION
  PUBLIC
  INHERITING FROM zcl_aoc_super
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS constructor.

    METHODS check
        REDEFINITION.
    METHODS get_attributes
        REDEFINITION.
    METHODS if_ci_test~query_attributes
        REDEFINITION.
    METHODS put_attributes
        REDEFINITION.

  PROTECTED SECTION.
    DATA mv_skipc TYPE flag.
    DATA mv_one_finding TYPE flag.
    DATA mv_hikey TYPE flag.
    DATA mv_lower TYPE flag.
    DATA mv_upper TYPE flag.
    DATA mv_lokey TYPE flag.
    DATA mv_flow TYPE flag.

  PRIVATE SECTION.
    METHODS build_option
      RETURNING
        VALUE(rv_option) TYPE string .
    METHODS check_flow .
    METHODS check_source
      IMPORTING
        !io_scan TYPE REF TO zcl_aoc_scan .
    METHODS pretty_print
      IMPORTING
        !it_code         TYPE string_table
      RETURNING
        VALUE(rt_pretty) TYPE string_table .
    METHODS skip_object
      IMPORTING
        !iv_name       TYPE csequence
      RETURNING
        VALUE(rv_skip) TYPE abap_bool.
ENDCLASS.



CLASS zcl_aoc_check_06 IMPLEMENTATION.


  METHOD build_option.

    DATA: ls_rseumod TYPE rseumod.

    " check workbench settings
    CALL FUNCTION 'RS_WORKBENCH_CUSTOMIZING'
      EXPORTING
        choice          = 'WB'
        suppress_dialog = 'X'
      IMPORTING
        setting         = ls_rseumod.
    IF ls_rseumod-lowercase = 'X'.
      rv_option = 'LOWER'.
    ELSEIF ls_rseumod-lowercase = 'G'.
      rv_option = 'HIKEY'.
    ELSEIF ls_rseumod-lowercase = 'L'.
      rv_option = 'LOKEY'.
    ELSE.
      rv_option = 'UPPER'.
    ENDIF.

  ENDMETHOD.


  METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

    DATA: lv_option  TYPE c LENGTH 5.


    lv_option = build_option( ).

    IF ( mv_lower = abap_true AND lv_option <> 'LOWER' )
        OR ( mv_hikey = abap_true AND lv_option <> 'HIKEY' )
        OR ( mv_lokey = abap_true AND lv_option <> 'LOKEY' )
        OR ( mv_upper = abap_true AND lv_option <> 'UPPER' ).
      inform( p_sub_obj_type = object_type
              p_sub_obj_name = object_name
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '002' ).
      RETURN.
    ENDIF.

    check_source( io_scan ).

    IF mv_flow = abap_true.
      check_flow( ).
    ENDIF.

  ENDMETHOD.


  METHOD check_flow.

    TYPES: BEGIN OF ty_d020s,
             dnum TYPE d020s-dnum,
             prog TYPE d020s-prog,
           END OF ty_d020s.

    DATA: BEGIN OF ls_dynp_id,
            prog TYPE d020s-prog,
            dnum TYPE d020s-dnum,
          END OF ls_dynp_id,
          lt_d020s  TYPE STANDARD TABLE OF ty_d020s WITH DEFAULT KEY,
          lv_option TYPE c LENGTH 5,
          ls_h      TYPE d020s,                             "#EC NEEDED
          lt_f      TYPE TABLE OF d021s,                    "#EC NEEDED
          lt_e      TYPE TABLE OF d022s,
          lt_m      TYPE TABLE OF d023s.                    "#EC NEEDED

    FIELD-SYMBOLS: <ls_e>     LIKE LINE OF lt_e,
                   <ls_d020s> LIKE LINE OF lt_d020s.


    lv_option = build_option( ).
    IF lv_option <> 'HIKEY'.
      " todo, so far it only works partly for keywords upper case
      RETURN.
    ENDIF.

    SELECT * FROM d020s
      INTO CORRESPONDING FIELDS OF TABLE lt_d020s
      WHERE prog = object_name
      AND type <> 'S'
      ORDER BY PRIMARY KEY.
    IF sy-subrc <> 0.
      RETURN.
    ENDIF.

    " the pretty_print method does not work for screen flow logic
    LOOP AT lt_d020s ASSIGNING <ls_d020s>.
      ls_dynp_id-prog = <ls_d020s>-prog.
      ls_dynp_id-dnum = <ls_d020s>-dnum.

      IMPORT DYNPRO ls_h lt_f lt_e lt_m ID ls_dynp_id.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      LOOP AT lt_e ASSIGNING <ls_e>.
        CONDENSE <ls_e>-line.
        IF <ls_e>-line CP '#M#o#d#u#l#e*'
            OR <ls_e>-line CP '#m#o#d#u#l#e*'.
          inform( p_kind    = mv_errty
                  p_test    = myname
                  p_code    = '003'
                  p_param_1 = <ls_d020s>-dnum ).
          IF mv_one_finding = abap_true.
            EXIT. " one finding per screen
          ENDIF.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD check_source.

    DATA: lt_code   TYPE string_table,
          lt_pretty TYPE string_table,
          lv_level  TYPE i,
          lv_row    TYPE i.

    FIELD-SYMBOLS: <ls_level>  LIKE LINE OF io_scan->levels,
                   <lv_code>   LIKE LINE OF lt_code,
                   <lv_pretty> LIKE LINE OF lt_pretty.



    LOOP AT io_scan->levels ASSIGNING <ls_level> WHERE type = io_scan->gc_level-program.
      lv_level = sy-tabix.

      IF is_class_pool( <ls_level>-name ) = abap_true.
        CONTINUE.
      ELSEIF mv_skipc = abap_true
          AND is_class_definition( <ls_level>-name ) = abap_true.
        CONTINUE. " current loop
      ELSEIF <ls_level>-name(8) = '/1BCWDY/'.
        " todo, web dynpro
        RETURN.
      ELSEIF <ls_level>-name(4) = 'SAPL'.
        " exclude functionpool
        CONTINUE.
      ELSEIF skip_object( <ls_level>-name ) = abap_true.
        " Generated include
        CONTINUE.
      ENDIF.

      " make sure the source code is not empty, as it will cause the pretty
      " printer to show an error message
      READ TABLE io_scan->statements WITH KEY level = lv_level TRANSPORTING NO FIELDS.
      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      lt_code = get_source( <ls_level> ).
      lt_pretty = pretty_print( lt_code ).

      LOOP AT lt_code ASSIGNING <lv_code>.
        lv_row = sy-tabix.
        READ TABLE lt_pretty INDEX lv_row ASSIGNING <lv_pretty>.
        ASSERT sy-subrc = 0.

        IF <lv_code> <> <lv_pretty>.
          inform( p_sub_obj_name = <ls_level>-name
                  p_line         = lv_row
                  p_kind         = mv_errty
                  p_test         = myname
                  p_code         = '001' ).
          IF mv_one_finding = abap_true.
            EXIT. " current loop, only one error per level
          ENDIF.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD constructor.

    super->constructor( ).

    version  = '003'.
    position = '006'.

    has_attributes = abap_true.
    attributes_ok  = abap_true.

    mv_hikey       = abap_true.
    mv_lokey       = abap_false.
    mv_lower       = abap_false.
    mv_upper       = abap_false.
    mv_one_finding = abap_true.
    mv_flow        = abap_true.
    mv_skipc       = abap_true.

    enable_rfc( ).

    insert_scimessage(
      iv_code = '001'
      iv_text = 'Use pretty printer'(m01) ).
    insert_scimessage(
      iv_code = '002'
      iv_text = 'Pretty printer settings do not match'(m02) ).
    insert_scimessage(
      iv_code = '003'
      iv_text = 'Use pretty printer, screen &1'(m03) ).

  ENDMETHOD.


  METHOD get_attributes.

    EXPORT
      mv_errty = mv_errty
      mv_hikey = mv_hikey
      mv_lokey = mv_lokey
      mv_lower = mv_lower
      mv_upper = mv_upper
      mv_one_finding = mv_one_finding
      mv_flow = mv_flow
      mv_skipc = mv_skipc
      TO DATA BUFFER p_attributes.

  ENDMETHOD.


  METHOD if_ci_test~query_attributes.

    zzaoc_top.

    zzaoc_fill_att mv_errty 'Error Type' ''.                "#EC NOTEXT

    zzaoc_fill_att_rb mv_hikey 'Keywords upper case' 'R' 'TYPE'. "#EC NOTEXT
    zzaoc_fill_att_rb mv_lokey 'Keywords lower case' 'R' 'TYPE'. "#EC NOTEXT
    zzaoc_fill_att_rb mv_upper 'Upper case' 'R' 'TYPE'.     "#EC NOTEXT
    zzaoc_fill_att_rb mv_lower 'Lower case' 'R' 'TYPE'.     "#EC NOTEXT

    zzaoc_fill_att mv_one_finding 'Report one finding per include' 'C'. "#EC NOTEXT
    zzaoc_fill_att mv_flow 'Check dynpro flow logic' 'C'.   "#EC NOTEXT
    zzaoc_fill_att mv_skipc 'Skip global class definitions' 'C'. "#EC NOTEXT

    zzaoc_popup.

  ENDMETHOD.


  METHOD pretty_print.

    DATA: lv_option TYPE c LENGTH 5.

    lv_option = build_option( ).

    rt_pretty = it_code.

    CALL FUNCTION 'CREATE_PRETTY_PRINT_FORMAT'
      EXPORTING
        mode          = lv_option
      TABLES
        source        = rt_pretty
      EXCEPTIONS
        syntax_errors = 1
        OTHERS        = 2.                       "#EC FB_RC "#EC CI_SUBRC

  ENDMETHOD.


  METHOD skip_object.

    IF object_type = 'FUGR'.
      SELECT COUNT(*)
        FROM trdir
        WHERE name = iv_name
          AND edtx = abap_true
          AND unam = 'SAP*'.
      rv_skip = boolc( sy-subrc = 0 ).
    ELSE.
      rv_skip = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD put_attributes.

    IMPORT
      mv_errty = mv_errty
      mv_hikey = mv_hikey
      mv_lokey = mv_lokey
      mv_lower = mv_lower
      mv_upper = mv_upper
      mv_one_finding = mv_one_finding
      mv_flow = mv_flow
      mv_skipc = mv_skipc
      FROM DATA BUFFER p_attributes.                 "#EC CI_USE_WANTED
    ASSERT sy-subrc = 0.

  ENDMETHOD.
ENDCLASS.
