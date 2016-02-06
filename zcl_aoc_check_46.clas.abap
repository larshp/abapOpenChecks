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
ENDCLASS.



CLASS ZCL_AOC_CHECK_46 IMPLEMENTATION.


METHOD check.

* abapOpenChecks
* https://github.com/larshp/abapOpenChecks
* MIT License

  DATA: lt_result  TYPE scr_refs,
        lv_include TYPE programm,
        lv_name    TYPE string.

  FIELD-SYMBOLS: <ls_result> LIKE LINE OF lt_result.


  lt_result = get_compiler( ).
  DELETE lt_result WHERE mode2 <> '2' OR tag <> 'DA'. " downport, cl_abap_compiler=>mode2_def

  LOOP AT lt_result ASSIGNING <ls_result>.
    lv_name = <ls_result>-full_name.
    IF <ls_result>-full_name CP '*\ME:*'.
      REPLACE FIRST OCCURRENCE OF REGEX '\\ME:.*\\' IN lv_name WITH '\'.
    ELSEIF <ls_result>-full_name CP '*\FO:*'.
      REPLACE FIRST OCCURRENCE OF REGEX '\\FO:.*\\' IN lv_name WITH '\'.
    ELSE.
      CONTINUE. " current loop
    ENDIF.

    READ TABLE lt_result WITH KEY full_name = lv_name TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      lv_include = <ls_result>-statement->source_info->name.
      inform( p_sub_obj_type = c_type_include
              p_sub_obj_name = lv_include
              p_line         = <ls_result>-statement->start_line
              p_kind         = mv_errty
              p_test         = myname
              p_code         = '001'
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
    WHEN OTHERS.
      ASSERT 1 = 0.
  ENDCASE.

ENDMETHOD.
ENDCLASS.