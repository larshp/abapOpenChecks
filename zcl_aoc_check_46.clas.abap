class ZCL_AOC_CHECK_46 definition
  public
  inheriting from ZCL_AOC_SUPER
  create public .

public section.

*"* public components of class ZCL_AOC_CHECK_46
*"* do not include other source files here!!!
  methods CONSTRUCTOR .

  methods CHECK
    redefinition .
  methods GET_MESSAGE_TEXT
    redefinition .
protected section.
*"* protected components of class ZCL_AOC_CHECK_46
*"* do not include other source files here!!!
private section.

  methods IS_SEC
    importing
      !IV_INCLUDE type STRING
    returning
      value(RV_SEC) type ABAP_BOOL .
  methods METHOD_INCLUDE
    importing
      !IV_NAME type STRING
    returning
      value(RV_INCLUDE) type PROGRAMM .
ENDCLASS.



CLASS ZCL_AOC_CHECK_46 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lt_result  TYPE scr_refs,
        lv_include TYPE programm,
        lv_name    TYPE string,
        lv_line    TYPE i,
        lv_code    TYPE sci_errc.

  FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_result.


  lt_result = get_compiler( ).
* downport, cl_abap_compiler=>mode2_def
  DELETE lt_result WHERE mode2 <> '2' OR tag <> 'DA'.

  LOOP AT lt_result ASSIGNING <ls_result>.
    lv_name = <ls_result>-full_name.
    lv_code = '001'.
    lv_include = <ls_result>-statement->source_info->name.
    lv_line = <ls_result>-statement->start_line.

    IF <ls_result>-full_name CP '*\ME:*'.

      IF is_sec( <ls_result>-statement->source_info->name ) = abap_true.
        lv_include = method_include( lv_name ).
        IF lv_include IS INITIAL.
          CONTINUE. " current loop
        ENDIF.
        lv_line = 1.
        lv_code = '002'.
      ENDIF.

      REPLACE FIRST OCCURRENCE OF REGEX '\\ME:\w+\\' IN lv_name WITH '\'.
    ELSEIF <ls_result>-full_name CP '*\FO:*'.
      REPLACE FIRST OCCURRENCE OF REGEX '\\FO:\w+\\' IN lv_name WITH '\'.
    ELSE.
      CONTINUE. " current loop
    ENDIF.

    READ TABLE lt_result WITH KEY full_name = lv_name TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = lv_include
              p_line         = lv_line
              p_kind         = mv_errty
              p_test         = myname
              p_code         = lv_code
              p_param_1      = <ls_result>-name ).
    ENDIF.
  ENDLOOP.

ENDMETHOD.


METHOD constructor.

  super->constructor( ).

  description    = 'Shadowed variable'.                     "#EC NOTEXT
  category       = 'ZCL_AOC_CATEGORY'.
  version        = '001'.
  position       = '046'.

  has_attributes = abap_true.
  attributes_ok  = abap_true.

  mv_errty = c_error.

ENDMETHOD.                    "CONSTRUCTOR


METHOD get_message_text.

  CLEAR p_text.

  CASE p_code.
    WHEN '001'.
      p_text = 'Shadowing variable &1'.                     "#EC NOTEXT
    WHEN '002'.
      p_text = 'Parameter shadowing variable &1'.           "#EC NOTEXT
    WHEN OTHERS.
      ASSERT 1 = 0.
  ENDCASE.

ENDMETHOD.


METHOD is_sec.

  DATA: lv_cls TYPE seoclsname.


  rv_sec = abap_false.

  lv_cls = object_name.

  IF object_type = 'CLAS'
      AND ( iv_include = cl_oo_classname_service=>get_pubsec_name( lv_cls )
      OR iv_include = cl_oo_classname_service=>get_prisec_name( lv_cls )
      OR iv_include = cl_oo_classname_service=>get_prosec_name( lv_cls ) ).
    rv_sec = abap_true.
  ENDIF.

ENDMETHOD.


METHOD method_include.

  DATA: ls_mtdkey TYPE seocpdkey,
        lv_off    TYPE i,
        lv_len    TYPE i.


  FIND REGEX '\\ME:.*\\' IN iv_name
    MATCH OFFSET lv_off
    MATCH LENGTH lv_len.
  lv_off = lv_off + 4.
  lv_len = lv_len - 5.
  ls_mtdkey-clsname = object_name.
  ls_mtdkey-cpdname = iv_name+lv_off(lv_len).
  cl_oo_classname_service=>get_method_include(
    EXPORTING
      mtdkey              = ls_mtdkey
    RECEIVING
      result              = rv_include
    EXCEPTIONS
      class_not_existing  = 1
      method_not_existing = 2
      OTHERS              = 3 ).

ENDMETHOD.
ENDCLASS.