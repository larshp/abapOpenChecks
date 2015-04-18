class ZCL_AOC_CHECK_17 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.
*"* public components of class ZCL_AOC_CHECK_17
*"* do not include other source files here!!!

  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_ATTRIBUTES
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
  methods PUT_ATTRIBUTES
    redefinition .
  methods IF_CI_TEST~QUERY_ATTRIBUTES
    redefinition .
protected section.
*"* protected components of class ZCL_AOC_CHECK_17
*"* do not include other source files here!!!

  data MV_TYPES type I .
  data MV_DEFINE type I .
  data MV_CONSTANTS type I .
  data MV_DATA type I .
  data MV_FS type I .
  data MV_STATICS type I .

  type-pools ABAP .
  methods CHECK_MODE
    importing
      !IV_TYPE type I
    returning
      value(RV_EXIT) type ABAP_BOOL .
private section.
*"* private components of class ZCL_AOC_CHECK_17
*"* do not include other source files here!!!

  data MS_STATEMENT type SSTMNT .
  data MS_TOKEN type STOKESX .
  data MV_MODE type I .
  constants C_MY_NAME type SEOCLSNAME value 'ZCL_AOC_CHECK_17'. "#EC NOTEXT
ENDCLASS.



CLASS ZCL_AOC_CHECK_17 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lv_exit   TYPE abap_bool,
        lv_define TYPE abap_bool,
        lv_others TYPE i.

  FIELD-SYMBOLS: <ls_structure> LIKE LINE OF it_structures.


  lv_others = mv_constants + mv_data + mv_fs + mv_statics + mv_types + mv_define.

  LOOP AT it_structures ASSIGNING <ls_structure>
      WHERE type = scan_struc_type-routine.

    mv_mode = 0.

    LOOP AT it_statements INTO ms_statement
        FROM <ls_structure>-stmnt_from + 1
        TO <ls_structure>-stmnt_to - 1
        WHERE type <> scan_stmnt_type-macro_call.

      READ TABLE it_tokens INTO ms_token INDEX ms_statement-from.
      IF sy-subrc <> 0
          OR ms_token-type = scan_token_type-comment
          OR ms_token-type = scan_token_type-pragma.
        CONTINUE. " current loop
      ENDIF.

* skip INCLUDE if it is part of TYPE definition
      IF mv_mode = mv_types AND ms_token-str = 'INCLUDE'.
        CONTINUE.
      ENDIF.

      IF lv_define = abap_true AND ms_token-str = 'END-OF-DEFINITION'.
        lv_define = abap_false.
        CONTINUE.
      ELSEIF lv_define = abap_true.
        CONTINUE. " current loop.
      ENDIF.

      CASE ms_token-str.
        WHEN 'TYPE' OR 'TYPES'.
          lv_exit = check_mode( mv_types ).
        WHEN 'CONSTANT' OR 'CONSTANTS'.
          lv_exit = check_mode( mv_constants ).
        WHEN 'DATA'.
          lv_exit = check_mode( mv_data ).
        WHEN 'FIELD-SYMBOLS'.
          lv_exit = check_mode( mv_fs ).
        WHEN 'STATICS'.
          lv_exit = check_mode( mv_statics ).
        WHEN 'DEFINE'.
          lv_exit = check_mode( mv_define ).
          lv_define = abap_true.
        WHEN OTHERS.
          lv_exit = check_mode( lv_others ).
      ENDCASE.

      IF lv_exit = abap_true.
        EXIT. " current loop
      ENDIF.

    ENDLOOP.

  ENDLOOP.

ENDMETHOD.


METHOD check_mode.

  DATA: lv_include TYPE program.


  IF mv_mode > iv_type.
    rv_exit = abap_true.

    lv_include = get_include( p_level = ms_statement-level ).

    inform( p_sub_obj_type = c_type_include
            p_sub_obj_name = lv_include
            p_line = ms_token-row
            p_kind = mv_errty
            p_test = c_my_name
            p_code = '001' ).
  ENDIF.

  mv_mode = iv_type.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Definitions in top of routine'.         "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '000'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_types     = 1.
  mv_define    = 2.
  mv_constants = 2.
  mv_statics   = 2.
  mv_data      = 2.
  mv_fs        = 3.

  mv_errty = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_attributes.

  EXPORT
    mv_errty     = mv_errty
    mv_constants = mv_constants
    mv_data      = mv_data
    mv_fs        = mv_fs
    mv_statics   = mv_statics
    mv_types     = mv_types
    mv_define    = mv_define
    TO DATA BUFFER p_attributes.

ENDMETHOD.


METHOD get_message_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Reorder definitions to top of routine'.     "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 1 + 1.
  ENDCASE.

ENDMETHOD.                    "GET_MESSAGE_TEXT


METHOD if_ci_test~query_attributes.

  DEFINE fill_att.
    get reference of &1 into ls_attribute-ref.
    ls_attribute-text = &2.
    ls_attribute-kind = &3.
    append ls_attribute to lt_attributes.
  END-OF-DEFINITION.

  DATA: lv_ok         TYPE abap_bool,
        lv_message    TYPE c LENGTH 72,
        lt_attributes TYPE sci_atttab,
        ls_attribute  LIKE LINE OF lt_attributes.


  fill_att mv_errty 'Error Type' ''.                        "#EC NOTEXT
  fill_att mv_types 'TYPES' ''.                             "#EC NOTEXT
  fill_att mv_define 'DEFINE' ''.                           "#EC NOTEXT
  fill_att mv_constants 'CONSTANTS' ''.                     "#EC NOTEXT
  fill_att mv_data 'DATA' ''.                               "#EC NOTEXT
  fill_att mv_statics 'STATICS' ''.                         "#EC NOTEXT
  fill_att mv_fs 'FIELD-SYMBOLS' ''.                        "#EC NOTEXT

  WHILE lv_ok = abap_false.
    cl_ci_query_attributes=>generic(
                          p_name       = c_my_name
                          p_title      = 'Options'
                          p_attributes = lt_attributes
                          p_message    = lv_message
                          p_display    = p_display ).       "#EC NOTEXT
    IF mv_errty = c_error OR mv_errty = c_warning OR mv_errty = c_note.
      lv_ok = abap_true.
    ELSE.
      lv_message = 'Fill attributes'.                       "#EC NOTEXT
    ENDIF.
  ENDWHILE.

ENDMETHOD.


METHOD put_attributes.

  IMPORT
    mv_errty     = mv_errty
    mv_constants = mv_constants
    mv_data      = mv_data
    mv_fs        = mv_fs
    mv_statics   = mv_statics
    mv_types     = mv_types
    mv_define    = mv_define
    FROM DATA BUFFER p_attributes.                   "#EC CI_USE_WANTED

ENDMETHOD.
ENDCLASS.