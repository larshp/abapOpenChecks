class ZCL_AOC_CHECK_06 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.
*"* public components of class ZCL_AOC_CHECK_06
*"* do not include other source files here!!!

  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_ATTRIBUTES
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
  methods IF_CI_TEST~QUERY_ATTRIBUTES
    redefinition .
  methods PUT_ATTRIBUTES
    redefinition .
protected section.
*"* protected components of class ZCL_AOC_CHECK_06
*"* do not include other source files here!!!

  data MV_ONE_FINDING type FLAG .
  data MV_HIKEY type FLAG .
  data MV_LOWER type FLAG .
  data MV_UPPER type FLAG .
  data MV_LOKEY type FLAG .
  data MV_FLOW type FLAG .
private section.
*"* private components of class ZCL_AOC_CHECK_06
*"* do not include other source files here!!!

  methods BUILD_OPTION
    returning
      value(RV_OPTION) type STRING .
  methods CHECK_FLOW .
  methods CHECK_SOURCE
    importing
      !IT_LEVELS type SLEVEL_TAB
      !IT_STATEMENTS type SSTMNT_TAB .
  methods PRETTY_PRINT
    importing
      !IT_CODE type STRING_TABLE
    returning
      value(RT_PRETTY) type STRING_TABLE .
ENDCLASS.



CLASS ZCL_AOC_CHECK_06 IMPLEMENTATION.


METHOD build_option.

  DATA: ls_rseumod TYPE rseumod.


* check workbench settings
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

  check_source( it_levels     = it_levels
                it_statements = it_statements ).

  IF mv_flow = abap_true.
    check_flow( ).
  ENDIF.

ENDMETHOD.


METHOD check_flow.

  DATA: BEGIN OF ls_dynp_id,
          prog TYPE d020s-prog,
          dnum TYPE d020s-dnum,
        END OF ls_dynp_id.

  DATA: lt_d020s TYPE STANDARD TABLE OF d020s WITH DEFAULT KEY,
        lv_option TYPE c LENGTH 5,
        ls_h TYPE d020s,                                    "#EC NEEDED
        lt_f TYPE TABLE OF d021s,                           "#EC NEEDED
        lt_e TYPE TABLE OF d022s,
        lt_m TYPE TABLE OF d023s.                           "#EC NEEDED

  FIELD-SYMBOLS: <ls_e> LIKE LINE OF lt_e,
                 <ls_d020s> LIKE LINE OF lt_d020s.


  lv_option = build_option( ).
  IF lv_option <> 'HIKEY'.
* todo, so far it only works partly for keywords upper case
    RETURN.
  ENDIF.

  SELECT * FROM d020s INTO TABLE lt_d020s
    WHERE prog = object_name
    AND type <> 'S'.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

* the pretty_print method does not work for screen flow logic

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

  DATA: lt_code    TYPE string_table,
        lt_pretty  TYPE string_table,
        lv_level   TYPE i,
        lv_row     TYPE i.

  FIELD-SYMBOLS: <ls_level>  LIKE LINE OF it_levels,
                 <lv_code>   LIKE LINE OF lt_code,
                 <lv_pretty> LIKE LINE OF lt_pretty.



  LOOP AT it_levels ASSIGNING <ls_level> WHERE type = scan_level_type-program.
    lv_level = sy-tabix.

* skip class definitions, they are auto generated(in most cases)
* todo: move this to check configuration
    IF strlen( <ls_level>-name ) = 32
        AND ( object_type = 'CLAS' OR object_type = 'INTF' )
        AND ( <ls_level>-name+30(2) = 'CU'
        OR <ls_level>-name+30(2) = 'CO'
        OR <ls_level>-name+30(2) = 'CI'
        OR <ls_level>-name+30(2) = 'CP'
        OR <ls_level>-name+30(2) = 'IP'
        OR <ls_level>-name+30(2) = 'IU' ).
      CONTINUE. " current loop
    ENDIF.
    IF <ls_level>-name(8) = '/1BCWDY/'.
* todo, web dynpro
      RETURN.
    ENDIF.
    IF <ls_level>-name(4) = 'SAPL'.
* exclude functionpool
      CONTINUE.
    ENDIF.

* make sure the source code is not empty, as it will cause the pretty
* printer to show an error message
    READ TABLE it_statements WITH KEY level = lv_level TRANSPORTING NO FIELDS.
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
        inform( p_sub_obj_type = c_type_include
                p_sub_obj_name = <ls_level>-name
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

  description    = 'Check pretty printer use'.              "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '002'.
  position       = '006'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.
  mv_hikey = abap_true.
  mv_lokey = abap_false.
  mv_lower = abap_false.
  mv_upper = abap_false.
  mv_one_finding = abap_true.
  mv_flow = abap_true.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_attributes.

  EXPORT
    mv_errty = mv_errty
    mv_hikey = mv_hikey
    mv_lokey = mv_lokey
    mv_lower = mv_lower
    mv_upper = mv_upper
    mv_one_finding = mv_one_finding
    mv_flow = mv_flow
    TO DATA BUFFER p_attributes.

ENDMETHOD.


METHOD get_message_text.

  CLEAR p_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Use pretty printer'.                        "#EC NOTEXT
    WHEN '002'.
      p_text = 'Pretty printer settings does not match'.    "#EC NOTEXT
    WHEN '003'.
      p_text = 'Use pretty printer, screen &1'.             "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 0 = 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT


METHOD if_ci_test~query_attributes.

  zzaoc_top.

  zzaoc_fill_att mv_errty 'Error Type' ''.                  "#EC NOTEXT

  zzaoc_fill_att_rb mv_hikey 'Keywords upper case' 'R' 'TYPE'. "#EC NOTEXT
  zzaoc_fill_att_rb mv_lokey 'Keywords lower case' 'R' 'TYPE'. "#EC NOTEXT
  zzaoc_fill_att_rb mv_upper 'Upper case' 'R' 'TYPE'.       "#EC NOTEXT
  zzaoc_fill_att_rb mv_lower 'Lower case' 'R' 'TYPE'.       "#EC NOTEXT

  zzaoc_fill_att mv_one_finding 'Report one finding per include' 'C'. "#EC NOTEXT
  zzaoc_fill_att mv_flow 'Check dynpro flow logic' 'C'.     "#EC NOTEXT

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


METHOD put_attributes.

  IMPORT
    mv_errty = mv_errty
    mv_hikey = mv_hikey
    mv_lokey = mv_lokey
    mv_lower = mv_lower
    mv_upper = mv_upper
    mv_one_finding = mv_one_finding
    mv_flow = mv_flow
    FROM DATA BUFFER p_attributes.                   "#EC CI_USE_WANTED
  ASSERT sy-subrc = 0.

ENDMETHOD.
ENDCLASS.